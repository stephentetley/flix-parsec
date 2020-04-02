
use LineCount.{runLineCounter};


@test
def runTests01(): Unit & Impure = 
    let path = FilePath.new("e:\coding\flix\text-parser\testdata\flix_dir.txt");
    match File.readFile(path, Charset.utf_16le()) { 
        case Err(msg) => Console.printLine(msg)
        case Ok(s) => {
            runLineCounter("lineCount01", LineCount.lineCount01, s);
            runLineCounter("lineCount02", LineCount.lineCount02, s)
        }

    }