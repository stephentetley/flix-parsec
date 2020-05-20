use TextParser.ParseError;
use TextParser.{runs, return};
use TextParser/Combinators.{pipe3, seqLeft, seqRight, many1};
use TextParser/Text.{byteOrderMark, restOfLine, restOfInput};

@test
def ukTimeStamp01(): Result[String, System.Error] = 
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
        case Err(e) => Console.printLine(TextParser/Error.showParserError(e))
        case Ok(xs) => match xs { 
            case Nil => Console.printLine("empty results")
            case _ => List.foreach(write1, xs)
        }
    } 



@test
def fileLength01(): Result[Int32, System.Error] & Impure =  
    let path = System/FilePath.new("D:\coding\flix\text-parser\samples\test-data\dir.txt");
    System/File.readFile(path, Text/Charset.utf_16le()) 
        |> Result.map(s -> String.length(s))

@test
def fileChar01(): Result[Bool, System.Error] & Impure =  
    let path = System/FilePath.new("D:\coding\flix\text-parser\samples\test-data\dir.txt");
    System/File.readFile(path, Text/Charset.utf_16le()) 
        |> Result.map(s -> String.charAt(1, s) == '\r')

@test
def fileChar02(): Result[Bool, System.Error] & Impure =  
    let path = System/FilePath.new("D:\coding\flix\text-parser\samples\test-data\dir.txt");
    System/File.readFile(path, Text/Charset.utf_16le()) 
        |> Result.map(s -> String.charAt(0, s) == '\ufeff')


@test
def fileChar03(): Result[Char, System.Error] & Impure =  
    let path = System/FilePath.new("D:\coding\flix\text-parser\samples\test-data\dir.txt");
    System/File.readFile(path, Text/Charset.utf_16le()) 
        |> Result.map(s -> String.charAt(9, s))



@test
def directoryDirective01(): Result[String, System.Error] = 
    TextParser.runs(DirListing.directoryDirective(), "    Directory: V:\PROJECTS\T0975 EDM2\7. Site Data\Site Work Sorted\STW") 


def makeString(xs: List[String]): String & Impure = 
    let sb = StringBuilder.new();
    List.foldLeft((_,s) -> StringBuilder.appendLine!(sb, s), (), xs);
    StringBuilder.toString(sb)

@test
def directoryDirective02(): Result[String, System.Error] & Impure =  
    let p1 = seqRight(DirListing.blankLines1(), DirListing.directoryDirective());
    let s = makeString("" :: "" :: "    Directory: V:\PROJECTS\T0975 EDM2\7. Site Data\Site Work Sorted\STW" :: Nil);
    TextParser.runs(p1, s)


@test
def directoryDirective03(): Result[String, System.Error] & Impure =  
    let p1 = seqRight(
                DirListing.blankLines1(), 
                seqLeft(DirListing.directoryDirective(), DirListing.blankLines1())
            );
    let s = makeString("" :: "" :: "    Directory: V:\PROJECTS\T0975 EDM2\7. Site Data\Site Work Sorted\STW" :: "" :: "" :: Nil);
    TextParser.runs(p1, s)


@test
def directoryHeadings01(): Result[Unit, System.Error] & Impure =  
    let p1 = seqRight(DirListing.blankLines1(), DirListing.directoryHeadings());
    let s = makeString("" :: "" :: "Mode                LastWriteTime         Length Name                            " :: "----                -------------         ------ ----                            " :: Nil);
    TextParser.runs(p1, s)

@test
def dirListingLine01(): Result[DirListingLine, System.Error] =  
    let s = "-a----       28/02/2020     15:52              0 dir.txt                            ";
    TextParser.runs(DirListing.dirListingLine(), s)

@test
def dirListingLine02(): Result[List[DirListingLine], System.Error] & Impure =  
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
        case Err(e) => Console.printLine(TextParser/Error.showParserError(e))
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
        case Err(e) => Console.printLine(TextParser/Error.showParserError(e))
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
        case Err(e) => Console.printLine(TextParser/Error.showParserError(e))
        case Ok(xs) => match xs { 
            case Nil => Console.printLine("empty results")
            case _ => List.foreach(write1, xs)
        }
    } 