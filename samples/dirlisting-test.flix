use TextParser.ParseError;
use TextParser.{runs, return};
use TextParser/Combinators.{pipe3, seqLeft, seqRight, many1};
use TextParser/Char.{byteOrderMark};
use TextParser/Text.{restOfLine, restOfInput};

@test
def ukTimeStamp01(): Result[String, String] = 
    TextParser.runs(DirListing.pUkTimeStamp(), "11/04/2019  08:49") 
        |> Result.map(DirListing.printTimeStamp)


pub def src() : Str = "G:\work\Projects\rtu\mk5-mmims\diffs\sws.txt"

pub def mapError(f: e -> e1, a: Result[a,e]): Result[a,e1] = 
    match a {
        case Err(e) => Err(f(e))
        case Ok(x) => Ok(x)
    }

// @test
// def readAll01(): Result[String,String] = 
//     let path = Path.new(src());
//     Internal.readAll(path)

@test
def exists01(): Result[Bool,String] & Impure = 
    let path = Path.new(src());
    Path.exists(path) |> mapError(_ -> "io error")

@test
def parseLines01(): Result[List[String], ParseError] & Impure = 
    let path = Path.new(src());
    TextParser.parseLines(restOfInput(), path)

@test
def readLines01(): Result[List[String], String] & Impure = 
    let path = Path.new(src());
    Path.readLines(path) |> mapError(_ -> "io error")


// pub def getFiles(path: Path): Result[List[Option[DirListingRow]], String] = 
//     let parser = 
//        TextParser.optional(
//                 DirListing.pFile()       
//         );
//     TextParser.parseLines(parser, path) 

pub def showOptionFiles(xs: List[Option[DirListingLine]]): Unit & Impure = 
    let getName = x -> match x {
        case FolderLine(s, _, _) => s
        case FileLine(s, _, _, _) => s
    };
    match xs {
        case Nil => ()
        case Some(x) :: rs => { Console.printLine(getName(x)); showOptionFiles(rs) }
        case None :: rs => showOptionFiles(rs)
    }

@test
def parseFile01(): Unit & Impure = 
    let path = "e:\coding\flix\text-parser\testdata\dir.txt";
    let write1 = Console.printLine << DirListing.toString;
    match DirListing.parseDirListing(path) {
        case Err(e) => Console.printLine(TextParser.showParserError(e))
        case Ok(xs) => match xs { 
            case Nil => Console.printLine("empty results")
            case _ => List.foreach(write1, xs)
        }
    } 

@test
def parseFile01a(): Result[DirListingFolder, ParseError] & Impure = 
    let path = FilePath.new("D:\coding\flix\text-parser\samples\test-data\dir.txt");
    let p1 = pipe3(
        byteOrderMark(),
        DirListing.blankLines1(),
        DirListing.dirListingFolder(),
        (_,_,z) -> z
    );
    TextParser.parseFile(p1, path, Charset.utf_16le())

@test
def parseFile01b(): Result[String, ParseError] & Impure = 
    let path = FilePath.new("D:\coding\flix\text-parser\samples\test-data\dir.txt");
    let p1 = pipe3(
        byteOrderMark(),
        DirListing.blankLines1(),
        restOfLine(true),
        (_,_,z) -> z
    );
    TextParser.parseFile(p1, path, Charset.utf_16le())


@test
def parseFile01c(): Result[String, ParseError] & Impure = 
    use TextParser.flatMap;
    let path = FilePath.new("D:\coding\flix\text-parser\samples\test-data\dir.txt");
    let p1 = {
        let* _ = byteOrderMark();
        let* _ = restOfLine(true);
        let* _ = restOfLine(true);
        let* a = restOfLine(true);
        return(a)
    };
    TextParser.parseFile(p1, path, Charset.utf_16le())



@test
def fileLength01(): Result[Int32, String] & Impure =  
    let path = FilePath.new("D:\coding\flix\text-parser\samples\test-data\dir.txt");
    File.readFile(path, Charset.utf_16le()) 
        |> Result.map(s -> String.length(s))

@test
def fileChar01(): Result[Bool, String] & Impure =  
    let path = FilePath.new("D:\coding\flix\text-parser\samples\test-data\dir.txt");
    File.readFile(path, Charset.utf_16le()) 
        |> Result.map(s -> String.charAt(1, s) == '\r')

@test
def fileChar02(): Result[Bool, String] & Impure =  
    let path = FilePath.new("D:\coding\flix\text-parser\samples\test-data\dir.txt");
    File.readFile(path, Charset.utf_16le()) 
        |> Result.map(s -> String.charAt(0, s) == '\ufeff')


@test
def fileChar03(): Result[Char, String] & Impure =  
    let path = FilePath.new("D:\coding\flix\text-parser\samples\test-data\dir.txt");
    File.readFile(path, Charset.utf_16le()) 
        |> Result.map(s -> String.charAt(9, s))



@test
def directoryDirective01(): Result[String, String] = 
    TextParser.runs(DirListing.directoryDirective(), "    Directory: V:\PROJECTS\T0975 EDM2\7. Site Data\Site Work Sorted\STW") 


def makeString(xs: List[String]): String & Impure = 
    let sb = StringBuilder.new();
    List.foldLeft((_,s) -> StringBuilder.appendLine(sb, s), (), xs);
    StringBuilder.toString(sb)

@test
def directoryDirective02(): Result[String, String] & Impure =  
    let p1 = seqRight(DirListing.blankLines1(), DirListing.directoryDirective());
    let s = makeString("" :: "" :: "    Directory: V:\PROJECTS\T0975 EDM2\7. Site Data\Site Work Sorted\STW" :: Nil);
    TextParser.runs(p1, s)


@test
def directoryDirective03(): Result[String, String] & Impure =  
    let p1 = seqRight(
                DirListing.blankLines1(), 
                seqLeft(DirListing.directoryDirective(), DirListing.blankLines1())
            );
    let s = makeString("" :: "" :: "    Directory: V:\PROJECTS\T0975 EDM2\7. Site Data\Site Work Sorted\STW" :: "" :: "" :: Nil);
    TextParser.runs(p1, s)


@test
def directoryHeadings01(): Result[Unit, String] & Impure =  
    let p1 = seqRight(DirListing.blankLines1(), DirListing.directoryHeadings());
    let s = makeString("" :: "" :: "Mode                LastWriteTime         Length Name                            " :: "----                -------------         ------ ----                            " :: Nil);
    TextParser.runs(p1, s)

@test
def dirListingLine01(): Result[DirListingLine, String] =  
    let s = "-a----       28/02/2020     15:52              0 dir.txt                            ";
    TextParser.runs(DirListing.dirListingLine(), s)

@test
def dirListingLine02(): Result[List[DirListingLine], String] & Impure =  
    let s = makeString(
                   "-a----       28/02/2020     15:52              0 dir.txt                           "
                :: "-a----       28/02/2020     15:48           1848 dirlisting-test.flix              "
                :: Nil);
    runs(many1(DirListing.dirListingLine()), s)


@test
def parseFile02(): Unit & Impure = 
    let path = "e:\coding\flix\text-parser\testdata\long_names_dir.txt";
    let write1 = Console.printLine << DirListing.toString;
    match DirListing.parseDirListing(path) {
        case Err(e) => Console.printLine(TextParser.showParserError(e))
        case Ok(xs) => match xs { 
            case Nil => Console.printLine("empty results")
            case _ => List.foreach(write1, xs)
        }
    }   
        

@test
def temp01(): Unit & Impure = 
    let path = "e:\coding\flix\text-parser\testdata\flix_dir.txt";
    let write1 = Console.printLine << DirListing.toString;
    match DirListing.parseDirListing(path) {
        case Err(e) => Console.printLine(TextParser.showParserError(e))
        case Ok(xs) => match xs { 
            case Nil => Console.printLine("empty results")
            case _ => List.foreach(write1, xs)
        }
    } 

@test
def temp02(): Unit & Impure = 
    let path = "G:\work\Projects\edm2\prep-for-edms_feb2020\cso_sps_dir.txt";
    let write1 = Console.printLine << DirListing.toString;
    match DirListing.parseDirListing(path) {
        case Err(e) => Console.printLine(TextParser.showParserError(e))
        case Ok(xs) => match xs { 
            case Nil => Console.printLine("empty results")
            case _ => List.foreach(write1, xs)
        }
    } 