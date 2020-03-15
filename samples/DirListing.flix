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
        TextParser.pipe5(
            TextParser.seqLeft(TextParser.int32(), TextParser.char('/')),
            TextParser.seqLeft(TextParser.int32(), TextParser.char('/')),
            TextParser.int32(),
            TextParser.seqLeft(TextParser.int32(), TextParser.char(':')),
            TextParser.int32(),
            (d, mon, y, h, min) -> { day=d, month=mon, year=y, hour=h, minute=min }
        )

    pub def fileStats(): TextParser.Parser[(TimeStamp, Int64)] = 
        TextParser.tuple2(
            pUkTimeStamp(),
            TextParser.int64()
        )
    
    /// Note - Windows Dir Listing can run to muliple lines
    pub def multilineName(offset: Int32): TextParser.Parser[String] = 
        let line1 = TextParser.restOfLine(true);
        let lineCont = 
            TextParser.seqRight(
                TextParser.count(offset, TextParser.char(' ')),
                TextParser.restOfLine(true)
            );
        TextParser.pipe2(
            line1, 
            TextParser.many(lineCont),
            (x, xs) -> List.foldLeft((ss,s) -> ss + s, x, xs) |> String.trim
        ) |> TextParser.nestError("multilineName") 

    def pFile(mode: Mode): TextParser.Parser[DirListingLine] = 
        TextParser.pipe2(
            TextParser.bounded(fileStats(), 36),
            multilineName(49),
            (stats, name) -> { 
                let (timestamp, size) = stats; 
                FileLine(name, mode, timestamp, size)
            }
        )

    def pFolder(mode: Mode): TextParser.Parser[DirListingLine] = 
        TextParser.pipe2(
            TextParser.bounded(pUkTimeStamp(), 36),
            multilineName(49),
            (timestamp, name) -> FolderLine(name, mode, timestamp)
        )

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

    pub def dirListingLine(): TextParser.Parser[DirListingLine] = 
        TextParser.bind(
            pMode(),
            x -> if (isDirectory(x)) pFolder(x) else pFile(x)            
        ) |> TextParser.nestError("dirListingLine") 

    pub def directoryDirective(): TextParser.Parser[String] = 
        let indent = TextParser.count(4, TextParser.char(' '));
        TextParser.pipe3(
            indent,
            TextParser.symbol("Directory:"),
            multilineName(4),
            (_,_,z) -> z
        ) |> TextParser.nestError("directoryDirective")


    def directoryHeadings1(): TextParser.Parser[Unit] = 
        TextParser.pipe4(
            TextParser.symbol("Mode"),
            TextParser.symbol("LastWriteTime"),
            TextParser.symbol("Length"),
            TextParser.symbol("Name"),
            (_,_,_,_) -> ()
        )
        
    pub def directoryHeadings(): TextParser.Parser[Unit] = 
        let underline = TextParser.many1Chars(TextParser.oneOf(' ' ::  '-' :: Nil));
        TextParser.pipe3(
            directoryHeadings1(),
            underline, 
            TextParser.newline(),
            (_,_,_) -> ()
        ) |> TextParser.nestError("directoryHeadings")
    
    def blankLine(): TextParser.Parser[Unit] = TextParser.newline()
    
    pub def blankLines1(): TextParser.Parser[Unit] = 
        TextParser.ignore(TextParser.many1(blankLine())) 
            |> TextParser.nestError("blankLines1")

    pub def dirListingFolder(): TextParser.Parser[DirListingFolder] = 
        TextParser.pipe3(
            directoryDirective(),
            TextParser.seqRight(blankLines1(), directoryHeadings()),
            TextParser.manyTill(dirListingLine(), blankLines1()),
            (x, _, zs) -> { path = x, contents = zs }
        ) |> TextParser.nestError("dirListingFolder")

    pub def directoryListing() : TextParser.Parser[List[DirListingFolder]] = 
        TextParser.pipe3(
            TextParser.byteOrderMark(),
            blankLines1(),
            TextParser.manyTill(dirListingFolder(), TextParser.endOfInput()),
            (_,_,z) -> z
        ) |> TextParser.nestError("directoryListing")


    pub def parseDirListing(path: String): Result[List[DirListingFolder], TextParser.ParseError] & Impure = 
        let path1 = FilePath.new(path);
        TextParser.parseFile(DirListing.directoryListing(), path1, Charset.utf_16le())
            
}