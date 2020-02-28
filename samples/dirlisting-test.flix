@test
def ukTimeStamp01(): Result[String, String] = 
    TextParser.run(DirListing.pUkTimeStamp(), "11/04/2019  08:49") 
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
def exists01(): Result[Bool,String] = 
    let path = Path.new(src());
    Path.exists(path) |> mapError(_ -> "io error")

@test
def parseLines01(): Result[List[String], String] = 
    let path = Path.new(src());
    TextParser.parseLines(TextParser.restOfInput(), path)

@test
def readLines01(): Result[List[String], String] = 
    let path = Path.new(src());
    Path.readLines( path) |> mapError(_ -> "io error")


// pub def getFiles(path: Path): Result[List[Option[DirListingRow]], String] = 
//     let parser = 
//        TextParser.optional(
//                 DirListing.pFile()       
//         );
//     TextParser.parseLines(parser, path) 

pub def showOptionFiles(xs: List[Option[DirListingLine]]): Unit = 
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
def parseFile01(): Result[Int32, String] = 
    let path = FilePath.new("D:\coding\flix\text-parser\samples\test-data\dir.txt");
    TextParser.parseFile(DirListing.directoryListing(), path, Charset.utf_16le())
        |> Result.map(x -> List.length(x))

@test
def parseFile01a(): Result[DirListingFolder, String] = 
    let path = FilePath.new("D:\coding\flix\text-parser\samples\test-data\dir.txt");
    TextParser.parseFile(DirListing.dirListingFolder(), path, Charset.utf_16le())


@test
def directoryDirective01(): Result[String, String] = 
    TextParser.run(DirListing.directoryDirective(), "    Directory: V:\PROJECTS\T0975 EDM2\7. Site Data\Site Work Sorted\STW") 


def makeString(xs: List[String]): String = 
    let sb = StringBuilder.new();
    List.foldLeft((_,s) -> StringBuilder.appendLine(sb, s), (), xs);
    StringBuilder.toString(sb)

@test
def directoryDirective02(): Result[String, String] =  
    let p1 = TextParser.seqRight(DirListing.blankLines(), DirListing.directoryDirective());
    let s = makeString("" :: "" :: "    Directory: V:\PROJECTS\T0975 EDM2\7. Site Data\Site Work Sorted\STW" :: Nil);
    TextParser.run(p1, s)


@test
def directoryDirective03(): Result[String, String] =  
    let p1 = TextParser.seqRight(DirListing.blankLines(), TextParser.seqLeft(DirListing.directoryDirective(), DirListing.blankLines()));
    let s = makeString("" :: "" :: "    Directory: V:\PROJECTS\T0975 EDM2\7. Site Data\Site Work Sorted\STW" :: "" :: "" :: Nil);
    TextParser.run(p1, s)


@test
def directoryHeadings01(): Result[Unit, String] =  
    let p1 = TextParser.seqRight(DirListing.blankLines(), DirListing.directoryHeadings());
    let s = makeString("" :: "" :: "Mode                LastWriteTime         Length Name                            " :: "----                -------------         ------ ----                            " :: Nil);
    TextParser.run(p1, s)

@test
def dirListingLine01(): Result[DirListingLine, String] =  
    let s = "-a----       28/02/2020     15:52              0 dir.txt                            ";
    TextParser.run(DirListing.dirListingLine(), s)

@test
def dirListingLine02(): Result[List[DirListingLine], String] =  
    let s = makeString(
                   "-a----       28/02/2020     15:52              0 dir.txt                           "
                :: "-a----       28/02/2020     15:48           1848 dirlisting-test.flix              "
                :: Nil);
    TextParser.run(TextParser.many1(DirListing.dirListingLine()), s)


