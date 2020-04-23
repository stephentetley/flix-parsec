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

use TextParser.Parser;
use TextParser.{return, nestError, alt};
use TextParser/Combinators.{seqLeft, seqRight, count, many, many1, manyTill, ignore, tuple2};
use TextParser/Text.{char, lower, oneOf, newline, byteOrderMark, restOfLine, many1Chars};
use TextParser/Token.{symbol, int32, int64};

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
        StringBuilder.appendLine!(sb, "");
        StringBuilder.appendLine!(sb, "Directory: " + x.path);
        StringBuilder.appendInt32!(sb, List.length(x.contents));
        StringBuilder.appendLine!(sb, " items");
        List.foldLeft((_,x1) -> StringBuilder.appendLine!(sb, dirListingLineToString(x1)), (), x.contents);
        StringBuilder.toString(sb)

    pub def printTimeStamp(ts: TimeStamp): String = 
        "${ Int32.toString(ts.day)}/${Int32.toString(ts.month)}/${ Int32.toString(ts.year)} ${ Int32.toString(ts.hour)}:${Int32.toString(ts.minute)}"

    pub def pUkTimeStamp(): Parser[TimeStamp] = 
        use TextParser.flatMap;
        let* dy  = seqLeft(int32(), char('/'));
        let* mon = seqLeft(int32(), char('/'));
        let* yr  = int32();
        let* hr  = seqLeft(int32(), char(':'));
        let* min = int32();
        return({ day=dy, month=mon, year=yr, hour=hr, minute=min })

    pub def fileStats(): Parser[(TimeStamp, Int64)] = 
        tuple2(
            pUkTimeStamp(),
            int64()
        )
    
    /// Note - Windows Dir Listing can run to muliple lines
    pub def multilineName(offset: Int32): Parser[String] = 
        use TextParser.flatMap;        
        let lineCont = 
            seqRight(
                count(offset, char(' ')),
                restOfLine(true)
            );
        let* x = restOfLine(true);
        let* xs = many(lineCont);
        let ans = List.foldLeft((ss,s) -> ss + s, x, xs) |> String.trim;
        return(ans) |> nestError("multilineName") 

    def pFile(mode: Mode): Parser[DirListingLine] = 
        use TextParser.flatMap;
        let* stats = TextParser.bounded(fileStats(), 36);
        let* name = multilineName(49);
        let (timestamp, size) = stats; 
        return(FileLine(name, mode, timestamp, size))

    def pFolder(mode: Mode): Parser[DirListingLine] = 
        use TextParser.flatMap;
        let* timestamp = TextParser.bounded(pUkTimeStamp(), 36);
        let* name = multilineName(49);
        return(FolderLine(name, mode, timestamp))

    pub def ignoreLine(): Parser[Option[a]] = 
        TextParser.map(_ -> None, restOfLine(true))

    def mode1() : Parser[Mode] = 
        TextParser.map(
            x -> Mode(x),
            many1Chars(alt(lower(), char('-'))) 
        )
    
    def pMode() : Parser[Mode] = TextParser.bounded(mode1(), 13)

    pub def isDirectory (mode: Mode) : Bool = 
        let Mode(str) = mode;
        match String.toList(str) { 
            case x :: _ => x == 'd'
            case _ => false
        }

    /// mode is a common prefix
    pub def dirListingLine(): Parser[DirListingLine] = 
        use TextParser.flatMap;
        let* mode = pMode();
        let* ans = if (isDirectory(mode)) pFolder(mode) else pFile(mode);
        return(ans) |> TextParser.nestError("dirListingLine") 

    pub def directoryDirective(): Parser[String] = 
        use TextParser.flatMap;        
        let* _ = count(4, char(' '));
        let* _ = symbol("Directory:");
        let* name = multilineName(4);
        return(name) |> nestError("directoryDirective")


    def directoryHeadings1(): Parser[Unit] = 
        use TextParser.flatMap;
        let* _ = symbol("Mode");
        let* _ = symbol("LastWriteTime");
        let* _ = symbol("Length");
        let* _ = symbol("Name");
        return(())
        
    pub def directoryHeadings(): Parser[Unit] = 
        use TextParser.flatMap;
        let underline = many1Chars(oneOf(' ' ::  '-' :: Nil));
        let* _ = directoryHeadings1();
        let* _ = underline;
        let* _ = newline();
        return(()) |> nestError("directoryHeadings")
    
    def blankLine(): Parser[Unit] = newline()
    
    pub def blankLines1(): Parser[Unit] = 
        ignore(many1(blankLine())) 
            |> nestError("blankLines1")

    pub def dirListingFolder(): Parser[DirListingFolder] = 
        use TextParser.flatMap;
        let* p1 = directoryDirective();
        let* _ = seqRight(blankLines1(), directoryHeadings());
        let* xs = manyTill(dirListingLine(), blankLines1());
        return({ path = p1, contents = xs }) 
            |> nestError("dirListingFolder")

    pub def directoryListing() : Parser[List[DirListingFolder]] = 
        use TextParser.flatMap;
        let* _ = byteOrderMark();
        let* _ = blankLines1();
        manyTill(dirListingFolder(), TextParser.endOfInput())
            |> nestError("directoryListing")


    pub def parseDirListing(path: String): Result[List[DirListingFolder], TextParser.ParseError] & Impure = 
        let path1 = System/FilePath.new(path);
        TextParser.parseFile(DirListing.directoryListing(), path1, System/Charset.utf_16le())
            
}