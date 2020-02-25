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


// pub def getFiles(path: Path): Result[List[Option[File]], String] = 
//     let parser = 
//        TextParser.optional(
//                 DirListing.pFile()       
//         );
//     TextParser.parseLines(parser, path) 

pub def showOptionFiles(xs: List[Option[File]]): Unit = 
    match xs {
        case Nil => ()
        case Some(x) :: rs => { Console.printLine(x.name); showOptionFiles(rs) }
        case None :: rs => showOptionFiles(rs)
    }

// @test
// def showFiles01(): Result[Unit, String] = 
//     getFiles(Path.new("sws.txt"))
//         |> Result.map(showOptionFiles)
