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



pub mod TestTextParser {

/*
def whenErr(x: Result[a, e], test: e -> Bool & ef): Bool & ef = match x {
    case Ok(_) => false
    case Err(e) => test(e)
}

def whenOk(x: Result[a, e], test: a -> Bool & ef): Bool & ef = match x {
    case Ok(a) => test(a)
    case Err(_) => false
}

/////////////////////////////////////////////////////////////////////////////
// return                                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def return01() : Bool & Impure =
    use TextParser.{run, return};
    run(return(1), "") |> x -> whenOk(x, v -> v == 1)


/////////////////////////////////////////////////////////////////////////////
// flatMapElse                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def ifTheElse01(): Bool & Impure =
    use TextParser.{run, char, failure, flatMapElse};
    let p1 = flatMapElse(char('a'), _ -> char('b'), failure("ifTheElse"));
    run(p1, "ab") |> x -> whenOk(x, c -> c == 'b')

@Test
def ifTheElse02(): Bool & Impure =
    use TextParser.{run, char, string, flatMapElse};
    let p1 = flatMapElse(char('x'), _ -> string("yz"), string("abc"));
    run(p1, "xyz") |> x -> whenOk(x, s -> s == "yz")

@Test
def ifTheElse03(): Bool & Impure =
    use TextParser.{run, char, string, flatMapElse};
    let p1 = flatMapElse(char('x'), _ -> string("yz"), string("abc"));
    run(p1, "abc") |> x -> whenOk(x, s -> s == "abc")


/////////////////////////////////////////////////////////////////////////////
// char                                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def char01(): Bool & Impure =
    use TextParser.{run, char};
    let p1 = char('a');
    run(p1, "a") |> x -> whenOk(x, c -> c == 'a')

@Test
def char02(): Bool & Impure =
    use TextParser.{run, char, flatMap, return};
    let p1 = {
        let* _ = char('a');
        let* b = char('b');
        return(b)
    };
    run(p1, "ab") |> x -> whenOk(x, c -> c == 'b')

@Test
def char03(): Bool & Impure =
    use TextParser.{run, char, flatMap, return};
    let p1 = {
        let* _ = char('a');
        let* _ = char('b');
        let* c = char('c');
        return(c)
    };
    run(p1, "abc") |> x -> whenOk(x, c -> c == 'c')

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////

@Test
def map01(): Bool & Impure =
    use TextParser.{run, map, return};
    run(map(x -> x + 1, return(1)), "") |> x -> whenOk(x, v -> v == 2)

@Test
def map02(): Bool & Impure =
    use TextParser.{run, map, return};
    run(map(x -> [x], return(1)), "1") |> x -> whenOk(x, v -> v[0] == 1)

@Test
def map03(): Bool & Impure =
    use TextParser.{run, map, return, anyChar, seqRight};
    let p1 = map(x -> [x], anyChar());
    let p2 = anyChar();
    run(seqRight(p1, p2), "12") |> x -> whenOk(x, v -> v == '2')

/////////////////////////////////////////////////////////////////////////////
// failure                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def failure() : Bool & Impure =
    use TextParser.{run, failure};
    run(failure("*Parse Error*"), "") |> x -> whenErr(x, v -> v.message == "*Parse Error*")

/////////////////////////////////////////////////////////////////////////////
// errorPos                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def errorPos01(): Bool & Impure =
    use TextParser.{run, string, samePosition};
    let p1 = string("a");
    run(p1, "") |> Result.mapErr(e -> e.position) |> x -> whenErr(x, samePosition({ lineNumber = 1, column = 1}))

@Test
def errorPos02(): Bool & Impure =
    use TextParser.{run, string, samePosition, seqRight};
    let p1 = string("a");
    run(seqRight(p1,p1), "ab") |> Result.mapErr(e -> e.position) |> x -> whenErr(x, samePosition({ lineNumber = 1, column = 2}))

@Test
def errorPos03(): Bool & Impure =
    use TextParser.{run, string, samePosition};
    use TextParser.Combinators.{count};
    let p1 = string("a");
    run(count(3, p1), "aab") |> Result.mapErr(e -> e.position) |> x -> whenErr(x, samePosition({ lineNumber = 1, column = 3}))


/////////////////////////////////////////////////////////////////////////////
// endOfInput                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def endOfInput01(): Bool & Impure =
    use TextParser.{runs, endOfInput};
    runs(endOfInput(), "") |> Result.isOk

@Test
def endOfInput02(): Bool & Impure =
    use TextParser.{runs, endOfInput};
    runs(endOfInput(), "a") |> Result.isErr

/////////////////////////////////////////////////////////////////////////////
// swapError                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def swapError01(): Bool & Impure =
    use TextParser.{run, string, swapError};
    let p1 = string("a") `swapError` "p1";
    run(p1, "") |> Result.mapErr(e -> e.message) == Err("p1")

@Test
def swapError02(): Bool & Impure =
    use TextParser.{run, string, seqRight, swapError};
    let p1 = string("a") `swapError` "p1";
    let p2 = string("b") `swapError` "p2";
    run(seqRight(p1, p2), "bb") |> Result.mapErr(e -> e.message) == Err("p1")

@Test
def swapError03(): Bool & Impure =
    use TextParser.{run, string, seqRight, swapError};
    let p1 = string("a") `swapError` "p1";
    let p2 = string("b") `swapError` "p2";
    run(seqRight(p1, p2), "ac") |> Result.mapErr(e -> e.message) == Err("p2")

@Test
def swapError04(): Bool & Impure =
    use TextParser.{run, string, swapError};
    let p1 = string("a") `swapError` "p1";
    run(p1, "") |> Result.mapErr(e -> e.message) == Err("p1")

@Test
def swapError05(): Bool & Impure =
    use TextParser.{run, string, seqRight, swapError};
    let p1 = string("a") `swapError` "p1";
    let p2 = string("b") `swapError` "p2";
    run(seqRight(p1, p2), "bb") |> Result.mapErr(e -> e.message) == Err("p1")

@Test
def swapError06(): Bool & Impure =
    use TextParser.{run, string, seqRight, swapError};
    let p1 = string("a") `swapError` "p1";
    let p2 = string("b") `swapError` "p2";
    run(seqRight(p1, p2), "ac") |> Result.mapErr(e -> e.message) == Err("p2")

/////////////////////////////////////////////////////////////////////////////
// parseLine                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def parseLine01(): Bool & Impure =
    use TextParser.{run, string, parseLine};
    use TextParser.Text.{alphas};
    let p1 = parseLine(alphas(), true);
    let input = "abc";
    run(p1, input).whenOk(x -> x == "abc")

@Test
def parseLine02(): Bool & Impure =
    use TextParser.{run, string, parseLine};
    use TextParser.Text.{alphas};
    let p1 = parseLine(alphas(), true);
    let input = String.unlines("abc" :: "def" :: Nil);
    run(p1, input).whenOk(x -> x == "abc")

*/
}
