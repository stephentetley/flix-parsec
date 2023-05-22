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
/// Note - PowerShell ouput is commonly "UTF-16LE"
///
/// Also note, a `dir` listing is flat without recursion and the results
/// are therefore a list not a tree. 

mod TestDirListing {

    def run1(path: String): Bool \ IO = 
        match DirListing.parseFile(charSet = "UTF-16LE", path) {
            case Ok(xs)   => {List.forEach(DirListing.dirListingFolderToString >> println, xs); true}
            case Err(msg) => {println(msg); false}
        }

    // @test
    // def test01(): Bool \ IO = run1("e:\\coding\\flix\\flix-parsec\\testdata\\dir.txt")

    @test
    def test02(): Bool \ IO = run1("e:\\coding\\flix\\flix-parsec\\testdata\\long_names_dir.txt")

    // @test
    // def test03(): Bool \ IO = run1("e:\\coding\\flix\\flix-parsec\\testdata\\flix_dir.txt")


}


mod DirListing {

    use GradedApplicative.{*>, point, product}
    use GradedMonad.{>>=}

    use FlixParsec.GenParser
    use FlixParsec.Prim.{bounded, endOfInput, commit}
    use FlixParsec.Combinators.{count, many, many1, manyTill, endBy}
    use FlixParsec.Char.{char, oneOf}
    use FlixParsec.Text.{restOfLine, newline}
    use FlixParsec.Regex.{lookingAt, skippingAt}
    use FlixParsec.TokenSimple.{int32, int64, symbol}

    use DirListing.DirListingLine.{FolderLine, FileLine}
    use DirListing.Mode.{Mode}

    type alias TimeStamp = 
        { day    = Int32
        , month  = Int32
        , year   = Int32
        , hour   = Int32
        , minute = Int32 
        }

    /// Don't care about attributes...

    pub enum Mode(String) 

    /// type alias File = { name: String, size: Int64, modified: TimeStamp }
    /// type alias Folder = { name: String, modified: TimeStamp }

    pub enum DirListingLine {
        case FolderLine(String, Mode, TimeStamp),
        case FileLine(String, Mode, TimeStamp, Int64)
    }

    type alias DirListingFolder = 
        { path = String
        , contents = List[DirListingLine] 
        }


    pub def dirListingLineToString(x: DirListingLine): String = match x {
        case FolderLine(name, _, _)   => name
        case FileLine(name, _, _, sz) => name + " " + Int64.toString(sz)
    }

    pub def dirListingFolderToString(x: DirListingFolder): String = region rc { 
        let sb = StringBuilder.new(rc);
        StringBuilder.appendLine!("", sb);
        StringBuilder.appendLine!("Directory: ${x.path}", sb);
        StringBuilder.appendLine!("${List.length(x.contents)} items", sb);
        List.forEach(x1 -> StringBuilder.appendLine!(dirListingLineToString(x1), sb), x.contents);
        StringBuilder.toString(sb)
    }

    pub def printTimeStamp(ts: TimeStamp): String = 
        "${ Int32.toString(ts.day)}/${Int32.toString(ts.month)}/${ Int32.toString(ts.year)} ${ Int32.toString(ts.hour)}:${Int32.toString(ts.minute)}"



    pub def parseFile(charSet: {charSet = String}, path: String): Result[String, List[DirListingFolder]] \ IO = 
        FlixParsec.Prim.parsesFile(dirOutput(), charSet, path)

    pub def dirOutput() : GenParser[ka, ust, ef, List[DirListingFolder]] = 
        byteOrderMark()     *>
        blankLines2()       *>
        endBy(dirListingFolder(), blankLines2())


    ///
    /// Byte order mark '\ufeff'
    ///
    pub def byteOrderMark(): GenParser[ka, ust, ef, Char] = char('\ufeff')

    pub def dirListingFolder(): GenParser[ka, ust, ef, DirListingFolder] = 
        commit(directoryDirective())                >>= d1  -> 
        blankLines2()                               >>= _   -> 
        directoryHeadings()                         >>= _   ->
        many(dirListingLine())                      >>= xs  ->
        point({path = d1, contents = xs}) 

    pub def directoryDirective(): GenParser[ka, ust, ef, String] =       
        count(4, char(' '))     *>
        symbol("Directory:")    *>
        multilineName(4)


    /// mode is a common prefix
    pub def dirListingLine(): GenParser[ka, ust, ef, DirListingLine] = 
        pMode()                                 >>= mode ->
        if (isDirectory(mode)) pFolder(mode) else pFile(mode)
    
    def pMode(): GenParser[ka, ust, ef, Mode] = 
        bounded(13, mode1())

    def mode1(): GenParser[ka, ust, ef, Mode] = 
        lookingAt(regex"(\\-|\\p{Lower}){6}") >>= s ->
        point(Mode(s))

    pub def isDirectory (mode: Mode) : Bool = 
        let Mode(str) = mode;
        match String.toList(str) { 
            case x :: _ => x == 'd'
            case _      => false
        }

    def pFile(mode: Mode): GenParser[ka, ust, ef, DirListingLine] = 
        bounded(36, fileStats())    >>= stats   -> 
        multilineName(49)           >>= name    -> {
            let (tstamp, sz) = stats;
            point(FileLine(name, mode, tstamp, sz))
        }


    def pFolder(mode: Mode): GenParser[ka, ust, ef, DirListingLine] = 
        bounded(36, pUkTimeStamp())     >>= tstamp  ->
        multilineName(49)               >>= name    -> 
        point(FolderLine(name, mode, tstamp))


    /// Note - Windows Dir Listing can run to muliple lines
    pub def multilineName(offset: Int32): GenParser[ka, ust, ef, String] = 
        let lineCont = () -> {
            count(offset, char(' '))    >>= _    -> 
            restOfLine(false)           >>= body ->
            newline()                   >>= _    ->
            point(body)
        };
        restOfLine(false)       >>= s1  -> 
        newline()               >>= _   ->
        many(lineCont())        >>= ss  -> {
            let ans = String.flatten(s1 :: ss) |> String.trim;
            point(ans)
        }


    pub def directoryHeadings(): GenParser[ka, ust, ef, Unit] = 
        let underline = () -> many1(oneOf(List#{' ', '-'}));
        directoryHeadings1()   *>
        underline()            *>
        newline()


    def directoryHeadings1(): GenParser[ka, ust, ef, Unit] = 
        symbol("Mode")          *>
        symbol("LastWriteTime") *>
        symbol("Length")        *>
        symbol("Name")          *>
        point()


    pub def fileStats(): GenParser[ka, ust, ef, (TimeStamp, Int64)] = 
        product(pUkTimeStamp(), int64())
   

    /// "28/02/2020     15:52"
    pub def pUkTimeStamp(): GenParser[ka, ust, ef, TimeStamp] = 
        int32()                     >>= dys  ->
        prefixedInt32('/')          >>= mons ->
        prefixedInt32('/')          >>= yrs  ->
        int32()                     >>= hrs  ->
        prefixedInt32(':')          >>= mins -> 
        point({day = dys, month = mons, year = yrs, hour = hrs, minute = mins})

    def prefixedInt32(ch: Char): GenParser[ka, ust, ef, Int32] = char(ch) *> int32()


    def blankLines2(): GenParser[ka, ust, ef, Unit] = 
        skippingAt(regex"\\R{2}")



}
