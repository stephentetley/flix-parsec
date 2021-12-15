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

/// A baseline count lines that doesn't use the parser combinators.

namespace LineCount1 {

    pub def timeOperation(thunk: Unit -> a & e): (Int64, a) & Impure = 
        import java.lang.System:nanoTime();
        let t0 = nanoTime();
        let a = thunk() as & Impure;
        let t1 = nanoTime();
        (t1 - t0, a)

    pub def runLineCounter(header: String, counter: String -> Int32 & e, s: String): Unit & Impure =         
        let (nt, a) = timeOperation(_ -> counter(s)) as & Impure;
        let msg = "Time: ${ Int64.toString(nt) }, lines: ${Int32.toString(a)}";
        println(header);
        println(msg)

    pub def lineCount01(s : String): Int32 = 
        lineCount01Helper(s, String.length(s), 1, 0)

    def lineCount01Helper(s: String, len: Int32, count: Int32, ix: Int32): Int32 = 
        if (ix >= len)
            count
        else
            let ch = String.charAt(ix, s);
            if (ch == '\n') 
                lineCount01Helper(s, len, count + 1, ix + 1)
            else
                lineCount01Helper(s, len, count, ix + 1)
                
    
    pub def runLineCount1(path: String): Unit & Impure = 
        let path1 = System/FilePath.new(path);
        match System/File.readFile(path1, Text/Charset.utf_16le()) { 
            case Err(e) => println(System/Error.toString(e))
            case Ok(s) => runLineCounter("lineCount01", lineCount01, s)
        }
        
}


def main(): Unit & Impure = 
    LineCount1.runLineCount1("e:\coding\flix\text-parser\testdata\flix_dir.txt")

