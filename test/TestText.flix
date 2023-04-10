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

/*

mod TestText {

def whenOk(x: Result[a, e], test: a -> Bool): Bool = match x {
    case Ok(a) => test(a)
    case Err(_) => false
}

/////////////////////////////////////////////////////////////////////////////
// restOfLine                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def restOfLine01(): Bool & Impure =
    use TextParser.{run};
    use TextParser.Text.{restOfLine};
    run(restOfLine(true), "").whenOk(ans -> ans == "")

@test
def restOfLine02(): Bool & Impure =
    use TextParser.{run};
    use TextParser.Text.{restOfLine};
    run(restOfLine(true), "abc").whenOk(ans -> ans == "abc")

@test
def restOfLine03(): Bool & Impure =
    use TextParser.{run};
    use TextParser.Text.{restOfLine};
    let input = String.unlines("abc" :: "def" :: Nil);
    run(restOfLine(true), input).whenOk(ans -> String.trim(ans) == "abc")

/////////////////////////////////////////////////////////////////////////////
// restOfLineLength                                                        //
/////////////////////////////////////////////////////////////////////////////

@test
def restOfLineLength01(): Bool & Impure =
    use TextParser.{run};
    use TextParser.Text.{restOfLineLength};
    run(restOfLineLength(), "").whenOk(ans -> ans == 0)

@test
def restOfLineLength02(): Bool & Impure =
    use TextParser.{run};
    use TextParser.Text.{restOfLineLength};
    run(restOfLineLength(), "abc").whenOk(ans -> ans == 3)

@test
def restOfLineLength03(): Bool & Impure =
    use TextParser.{run};
    use TextParser.Text.{restOfLineLength};
    let input = String.unlines("abc" :: "def" :: Nil);
    run(restOfLineLength(), input).whenOk(ans -> ans == 3)

}

*/
