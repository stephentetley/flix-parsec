
use LineCount.{runLineCounter};

def runlineCount01(path: String): Unit & Impure = 
    let path1 = System/FilePath.new(path);
    match System/File.readFile(path1, System/Charset.utf_16le()) { 
        case Err(msg) => Console.printLine(msg)
        case Ok(s) => {
            runLineCounter("lineCount01", LineCount.lineCount01, s)
        }

    }

def runlineCount03(path: String): Unit & Impure = 
    let path1 = System/FilePath.new(path);
    match System/File.readFile(path1, System/Charset.utf_16le()) { 
        case Err(msg) => Console.printLine(msg)
        case Ok(s) => {
            runLineCounter("lineCount03", LineCount.lineCount03, s)
        }

    }


def runTests(path: String): Unit & Impure = 
    let path1 = System/FilePath.new(path);
    match System/File.readFile(path1, System/Charset.utf_16le()) { 
        case Err(msg) => Console.printLine(msg)
        case Ok(s) => {
            runLineCounter("lineCount01", LineCount.lineCount01, s);
            runLineCounter("lineCount02", LineCount.lineCount02, s);
            runLineCounter("lineCount03", LineCount.lineCount03, s)
        }

    }

@test
def test01(): Unit & Impure = 
    runTests("e:\coding\flix\text-parser\testdata\long_names_dir.txt")


// Note - takes > 5 minutes, stoppped execution.
@test
def test02(): Unit & Impure = 
    runTests("e:\coding\flix\text-parser\testdata\flix_dir.txt")

@test
def test02a(): Unit & Impure = 
    runlineCount01("e:\coding\flix\text-parser\testdata\flix_dir.txt")

@test
def test02b(): Unit & Impure = 
    runlineCount03("e:\coding\flix\text-parser\testdata\flix_dir.txt")


pub def span(ix: Int32, test: Char -> Bool, src: String): Int32 = 
    spanHelper(test, src, String.length(src) - 1, ix)

/// TODO - CPS transform
def spanHelper(test: Char -> Bool, src: String, end: Int32, ix: Int32): Int32 = 
    if (ix > end)
        ix
    else 
        if (test(String.charAt(ix, src)) == false) 
            ix
        else 
            spanHelper(test, src, end, ix+1)

/// can we do say scanDigits without recursion in Flix?


