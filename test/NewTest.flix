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

use TextParser.ErrMsg;
use TextParser.{TextParser};


namespace LineCount {

    pub def timeOperation(thunk: Unit -> a & e): (Int64, a) & Impure = 
        import java.lang.System:nanoTime();
        let t0 = nanoTime();
        let a = thunk() as & Impure;
        let t1 = nanoTime();
        (t1 - t0, a)

    pub def runLineCounter(header: String, counter: String -> Int32 & e, s: String): Unit & Impure =         
        let (nt, a) = timeOperation(_ -> counter(s)) as & Impure;
        let msg = "Time: ${ Int64.toString(nt) }, lines: ${Int32.toString(a)}";
        Console.printLine(header);
        Console.printLine(msg)

    pub def parseLine(patt: Text/Regex.Pattern): TextParser[Unit, t] =
        use TextParser.{lookingAt};
        use TextParser/Combinators.{seqRight};
        use TextParser/Text.{newline};
        seqRight(lookingAt(patt), newline())

    pub def lineCount(): TextParser[Int32, t] = 
        use TextParser.{flatMap, map, liftResult};
        use TextParser/Combinators.{ntimes}; 
        let* patt = liftResult(Text/Regex/Pattern.compile(".*"));
        map(x -> x + 1, ntimes(parseLine(patt)))
    
    pub def runLineCount(path: String): Unit & Impure = 
        let path1 = System/FilePath.new(path);
        let runner = s -> match TextParser.runs(lineCount(), s) { 
            case Err(_) => -1
            case Ok(i) => i
        };
        match System/File.readFile(path1, Text/Charset.utf_16le()) { 
            case Err(e) => Console.printLine(System/Error.toString(e))
            case Ok(s) => runLineCounter("TextParser lineCount with regex", runner, s)
        }
        
}


// def main(): Unit & Impure = 
//     LineCount.runLineCount("e:\coding\flix\text-parser\testdata\flix_dir.txt")


// def main(): Unit & Impure = 
//     use TextParser.{runs, string, failure};
//     use TextParser/Combinators.{seqRight};
//     let p1: TextParser.TextParser[String,String] = seqRight(string("Hello"), failure("world"));
//     match runs(p1, "Hello Goodbye") { 
//         case Ok(s) => Console.printLine(s)
//         case Err(msg) => Console.printLine(msg)
//     }

// def main(): List[Char]  = 
//     use TextParser.{runs, anyChar};
//     use TextParser/Combinators.{many};
//     match runs(many(anyChar()), "123") { 
//         case Ok(s) => s
//         case Err(_) => Nil
//     }

def main(): List[String]  = 
    use TextParser.{runs, bounded, flatMap, string, return};
    use TextParser/Text.{many1Char};
    let p1 = () -> { 
        let* s1 = bounded(5, many1Char('a'));
        let* s2 = string("bb");
        return(s1 :: s2 :: Nil)
    };
    match runs(p1(), "aaaa bb") { 
        case Ok(xs) => xs
        case Err(_) => Nil
    }