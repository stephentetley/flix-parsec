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

use TextParser.Parser;
use TextParser.{runs, return, fail, map};

/// Very simple parser to count lines.

mod LineCount {


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

    pub def lineCount02(s: String): Int32 & Impure = 
        match runs(lc02(), s) {
            case Err(e) => { println(System.Error.toString(e)); -1 }
            case Ok(c) => c
        }

    pub def lc02(): Parser[Int32] = 
        use TextParser.Combinators.ntimes; 
        ntimes(parseLine2())

    pub def parseLine2(): Parser[Unit] =
        use TextParser.Combinators.skipManyTill; 
        use TextParser.Text.{anyChar, newline};
        skipManyTill(anyChar(), newline())

    pub def lineCount03(s: String): Int32 & Impure = 
        match runs(lc03(), s) {
            case Err(e) => { println(System.Error.toString(e)); -1 }
            case Ok(c) => c
        }

    pub def lc03(): Parser[Int32] = 
        use TextParser.Combinators.ntimes; 
        TextParser.map(x -> x + 1, ntimes(parseLine3()))

    def parseLine3(): Parser[Unit] =
        use TextParser.flatMap;
        use TextParser.Text.{isEof, restOfLine};
        let* end = isEof();
        if (end)
            fail("eof")
        else {
            let* _ = restOfLine(true);
            return()
        }
        
    
}