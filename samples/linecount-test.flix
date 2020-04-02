
use LineCount.{runLineCounter};

def runTests(path: String): Unit & Impure = 
    let path1 = FilePath.new(path);
    match File.readFile(path1, Charset.utf_16le()) { 
        case Err(msg) => Console.printLine(msg)
        case Ok(s) => {
            runLineCounter("lineCount01", LineCount.lineCount01, s);
            runLineCounter("lineCount02", LineCount.lineCount02, s)
        }

    }

@test
def test01(): Unit & Impure = 
    runTests("e:\coding\flix\text-parser\testdata\long_names_dir.txt")

@test
def test02(): Unit & Impure = 
    runTests("e:\coding\flix\text-parser\testdata\flix_dir.txt")