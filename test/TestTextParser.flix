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



namespace TestTextParser {

def whenErr(x: Result[a, e], test: e -> Bool): Bool = match x {
    case Ok(_) => false
    case Err(e) => test(e)
}

def whenOk(x: Result[a, e], test: a -> Bool): Bool = match x {
    case Ok(a) => test(a)
    case Err(_) => false
}

/////////////////////////////////////////////////////////////////////////////
// return                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def return01() : Bool =
    use TextParser.{run, return};
    run(return(1), "") |> x -> whenOk(x, v -> v == 1)


/////////////////////////////////////////////////////////////////////////////
// ifThenElse                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def ifTheElse01(): Bool =
    use TextParser.{run, char, failure, ifThenElse};
    let p1 = ifThenElse(char('a'), _ -> char('b'), failure("ifTheElse"));
    run(p1, "ab") |> x -> whenOk(x, c -> c == 'b')

@test
def ifTheElse02(): Bool =
    use TextParser.{run, char, string, ifThenElse};
    let p1 = ifThenElse(char('x'), _ -> string("yz"), string("abc"));
    run(p1, "xyz") |> x -> whenOk(x, s -> s == "yz")

@test
def ifTheElse03(): Bool =
    use TextParser.{run, char, string, ifThenElse};
    let p1 = ifThenElse(char('x'), _ -> string("yz"), string("abc"));
    run(p1, "abc") |> x -> whenOk(x, s -> s == "abc")


/////////////////////////////////////////////////////////////////////////////
// char                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def char01(): Bool =
    use TextParser.{run, char};
    let p1 = char('a');
    run(p1, "a") |> x -> whenOk(x, c -> c == 'a')

@test
def char02(): Bool =
    use TextParser.{run, char, flatMap, return};
    let p1 = {
        let* _ = char('a');
        let* b = char('b');
        return(b)
    };
    run(p1, "ab") |> x -> whenOk(x, c -> c == 'b')

@test
def char03(): Bool =
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

@test
def map01(): Bool =
    use TextParser.{run, map, return};
    run(map(x -> x+1, return(1)), "") |> x -> whenOk(x, v -> v == 2)

/////////////////////////////////////////////////////////////////////////////
// failure                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def failure() : Bool =
    use TextParser.{run, failure};
    run(failure("*Parse Error*"), "") |> x -> whenErr(x, v -> v.message == "*Parse Error*")

/////////////////////////////////////////////////////////////////////////////
// errorPos                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def errorPos01(): Bool =
    use TextParser.{run, string, samePosition};
    let p1 = string("a");
    run(p1, "") |> Result.mapErr(e -> e.position) |> x -> whenErr(x, samePosition({ lineNumber = 1, column = 1}))

@test
def errorPos02(): Bool =
    use TextParser.{run, string, samePosition, seqRight};
    let p1 = string("a");
    run(seqRight(p1,p1), "ab") |> Result.mapErr(e -> e.position) |> x -> whenErr(x, samePosition({ lineNumber = 1, column = 2}))

@test
def errorPos03(): Bool =
    use TextParser.{run, string, samePosition};
    use TextParser/Combinators.{count};
    let p1 = string("a");
    run(count(3, p1), "aab") |> Result.mapErr(e -> e.position) |> x -> whenErr(x, samePosition({ lineNumber = 1, column = 3}))


/////////////////////////////////////////////////////////////////////////////
// endOfInput                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def endOfInput01(): Bool =
    use TextParser.{runs, endOfInput};
    runs(endOfInput(), "") |> Result.isOk

@test
def endOfInput02(): Bool =
    use TextParser.{runs, endOfInput};
    runs(endOfInput(), "a") |> Result.isErr

/////////////////////////////////////////////////////////////////////////////
// swapError                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def swapError01(): Bool =
    use TextParser.{run, string, swapError};
    let p1 = string("a") `swapError` "p1";
    run(p1, "") |> Result.mapErr(e -> e.message) == Err("p1")

@test
def swapError02(): Bool =
    use TextParser.{run, string, seqRight, swapError};
    let p1 = string("a") `swapError` "p1";
    let p2 = string("b") `swapError` "p2";
    run(seqRight(p1, p2), "bb") |> Result.mapErr(e -> e.message) == Err("p1")

@test
def swapError03(): Bool =
    use TextParser.{run, string, seqRight, swapError};
    let p1 = string("a") `swapError` "p1";
    let p2 = string("b") `swapError` "p2";
    run(seqRight(p1, p2), "ac") |> Result.mapErr(e -> e.message) == Err("p2")

@test
def swapError04(): Bool =
    use TextParser.{run, string, swapError};
    let p1 = string("a") `swapError` "p1";
    run(p1, "") |> Result.mapErr(e -> e.message) == Err("p1")

@test
def swapError05(): Bool =
    use TextParser.{run, string, seqRight, swapError};
    let p1 = string("a") `swapError` "p1";
    let p2 = string("b") `swapError` "p2";
    run(seqRight(p1, p2), "bb") |> Result.mapErr(e -> e.message) == Err("p1")

@test
def swapError06(): Bool =
    use TextParser.{run, string, seqRight, swapError};
    let p1 = string("a") `swapError` "p1";
    let p2 = string("b") `swapError` "p2";
    run(seqRight(p1, p2), "ac") |> Result.mapErr(e -> e.message) == Err("p2")


}
