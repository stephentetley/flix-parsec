/*
 * Copyright 2020 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/// Parser for PowerShell `dir` output.
/// Note - PowerShell ouput is commonly UTF16_LE
///
/// Also note, a `dir` listing is flat without recursion and the results
/// are therefore a list not a tree. 

use TextParser.return;

type alias TimeStamp = { day: Int32, month: Int32, year: Int32, hour: Int32, minute: Int32 }

/// Don't care about attributes for the moment 

opaque type Mode = String

/// type alias File = { name: String, size: Int64, modified: TimeStamp }
/// type alias Folder = { name: String, modified: TimeStamp }

enum DirListingLine {
    case FolderLine(String, Mode, TimeStamp),
    case FileLine(String, Mode, TimeStamp, Int64)
}

type alias DirListingFolder = { path: String, contents: List[DirListingLine] }

namespace DirListing {

    pub def dirListingLineToString(x: dirListingLine): String = match x {
        case FolderLine(name, _, _) => name
        case FileLine(name, _, _, sz) => name + " " + Int64.toString(sz)
    }

    pub def toString(x: DirListingFolder): String = toStringHelper(x) as & Pure

    def toStringHelper(x: DirListingFolder): String & Impure = 
        let sb = StringBuilder.new();
        StringBuilder.appendLine(sb, "");
        StringBuilder.appendLine(sb, "Directory: " + x.path);
        StringBuilder.appendInt32(sb, List.length(x.contents));
        StringBuilder.appendLine(sb, " items");
        List.foldLeft((_,x1) -> StringBuilder.appendLine(sb, dirListingLineToString(x1)), (), x.contents);
        StringBuilder.toString(sb)

    pub def printTimeStamp(ts: TimeStamp): String = 
        "${ Int32.toString(ts.day)}/${Int32.toString(ts.month)}/${ Int32.toString(ts.year)} ${ Int32.toString(ts.hour)}:${Int32.toString(ts.minute)}"

    pub def pUkTimeStamp(): TextParser.Parser[TimeStamp] = 
        use TextParser.flatMap;
        let* dy  = TextParser.seqLeft(TextParser.int32(), TextParser.char('/'));
        let* mon = TextParser.seqLeft(TextParser.int32(), TextParser.char('/'));
        let* yr  = TextParser.int32();
        let* hr  = TextParser.seqLeft(TextParser.int32(), TextParser.char(':'));
        let* min = TextParser.int32();
        return({ day=dy, month=mon, year=yr, hour=hr, minute=min })

    pub def fileStats(): TextParser.Parser[(TimeStamp, Int64)] = 
        TextParser.tuple2(
            pUkTimeStamp(),
            TextParser.int64()
        )
    
    /// Note - Windows Dir Listing can run to muliple lines
    pub def multilineName(offset: Int32): TextParser.Parser[String] = 
        use TextParser.flatMap;        
        let lineCont = 
            TextParser.seqRight(
                TextParser.count(offset, TextParser.char(' ')),
                TextParser.restOfLine(true)
            );
        let* x = TextParser.restOfLine(true);
        let* xs = TextParser.many(lineCont);
        let ans = List.foldLeft((ss,s) -> ss + s, x, xs) |> String.trim;
        return(ans) |> TextParser.nestError("multilineName") 

    def pFile(mode: Mode): TextParser.Parser[DirListingLine] = 
        use TextParser.flatMap;
        let* stats = TextParser.bounded(fileStats(), 36);
        let* name = multilineName(49);
        let (timestamp, size) = stats; 
        return(FileLine(name, mode, timestamp, size))

    def pFolder(mode: Mode): TextParser.Parser[DirListingLine] = 
        use TextParser.flatMap;
        let* timestamp = TextParser.bounded(pUkTimeStamp(), 36);
        let* name = multilineName(49);
        return(FolderLine(name, mode, timestamp))

    pub def ignoreLine(): TextParser.Parser[Option[a]] = 
        TextParser.map(_ -> None, TextParser.restOfLine(true))

    def mode1() : TextParser.Parser[Mode] = 
        TextParser.map(
            x -> Mode(x),
            TextParser.many1Chars(TextParser.alt(TextParser.lower(), TextParser.char('-'))) 
        )
    
    def pMode() : TextParser.Parser[Mode] = TextParser.bounded(mode1(), 13)

    pub def isDirectory (mode: Mode) : Bool = 
        let Mode(str) = mode;
        match String.toList(str) { 
            case x :: _ => x == 'd'
            case _ => false
        }

    /// mode is a common prefix
    pub def dirListingLine(): TextParser.Parser[DirListingLine] = 
        use TextParser.flatMap;
        let* mode = pMode();
        let* ans = if (isDirectory(mode)) pFolder(mode) else pFile(mode);
        return(ans) |> TextParser.nestError("dirListingLine") 

    pub def directoryDirective(): TextParser.Parser[String] = 
        use TextParser.flatMap;        
        let* _ = TextParser.count(4, TextParser.char(' '));
        let* _ = TextParser.symbol("Directory:");
        let* name = multilineName(4);
        return(name) |> TextParser.nestError("directoryDirective")


    def directoryHeadings1(): TextParser.Parser[Unit] = 
        use TextParser.flatMap;
        let* _ = TextParser.symbol("Mode");
        let* _ = TextParser.symbol("LastWriteTime");
        let* _ = TextParser.symbol("Length");
        let* _ = TextParser.symbol("Name");
        return(())
        
    pub def directoryHeadings(): TextParser.Parser[Unit] = 
        use TextParser.flatMap;
        let underline = TextParser.many1Chars(TextParser.oneOf(' ' ::  '-' :: Nil));
        let* _ = directoryHeadings1();
        let* _ = underline;
        let* _ = TextParser.newline();
        return(()) |> TextParser.nestError("directoryHeadings")
    
    def blankLine(): TextParser.Parser[Unit] = TextParser.newline()
    
    pub def blankLines1(): TextParser.Parser[Unit] = 
        TextParser.ignore(TextParser.many1(blankLine())) 
            |> TextParser.nestError("blankLines1")

    pub def dirListingFolder(): TextParser.Parser[DirListingFolder] = 
        use TextParser.flatMap;
        let* p1 = directoryDirective();
        let* _ = TextParser.seqRight(blankLines1(), directoryHeadings());
        let* xs = TextParser.manyTill(dirListingLine(), blankLines1());
        return({ path = p1, contents = xs }) 
            |> TextParser.nestError("dirListingFolder")

    pub def directoryListing() : TextParser.Parser[List[DirListingFolder]] = 
        use TextParser.flatMap;
        let* _ = TextParser.byteOrderMark();
        let* _ = blankLines1();
        TextParser.manyTill(dirListingFolder(), TextParser.endOfInput())
            |> TextParser.nestError("directoryListing")


    pub def parseDirListing(path: String): Result[List[DirListingFolder], TextParser.ParseError] & Impure = 
        let path1 = FilePath.new(path);
        TextParser.parseFile(DirListing.directoryListing(), path1, Charset.utf_16le())
            
}