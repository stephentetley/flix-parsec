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



mod TestCombinators {
/*
    /////////////////////////////////////////////////////////////////////////////
    // many                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def many01(): Bool =
        use FlixParsec.Prim.{parsesString, endOfInput};
        use FlixParsec.Char.{char};
        use FlixParsec.Combinators.{many};
        parsesString(many(char('a')), "") |> Result.isOk

    @test
    def many02(): Bool & Impure =
        use TextParser.{runs, char, endOfInput};
        use TextParser.Combinators.{many};
        runs(many(char('a')), "a") |> Result.isOk

    @test
    def many03(): Bool & Impure =
        use TextParser.{runs, char, endOfInput};
        use TextParser.Combinators.{many};
        runs(many(char('a')), "aa") |> Result.isOk

    /////////////////////////////////////////////////////////////////////////////
    // manyTill                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def manyTill01(): Bool & Impure =
        use TextParser.{runs, char, endOfInput};
        use TextParser.Combinators.{manyTill};
        runs(manyTill(char('a'), endOfInput()), "") |> Result.isOk

    @test
    def manyTill02(): Bool & Impure =
        use TextParser.{runs, char, endOfInput};
        use TextParser.Combinators.{manyTill};
        runs(manyTill(char('a'), endOfInput()), "a") |> Result.isOk

    @test
    def manyTill03(): Bool & Impure =
        use TextParser.{runs, char, endOfInput};
        use TextParser.Combinators.{manyTill};
        runs(manyTill(char('a'), endOfInput()), "aa") |> Result.isOk

    /////////////////////////////////////////////////////////////////////////////
    // many1Till                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def many1Till01(): Bool & Impure =
        use TextParser.{runs, char, endOfInput};
        use TextParser.Combinators.{many1Till};
        runs(many1Till(char('a'), endOfInput()), "") |> Result.isErr

    @test
    def many1Till02(): Bool & Impure =
        use TextParser.{runs, char, endOfInput};
        use TextParser.Combinators.{many1Till};
        runs(many1Till(char('a'), endOfInput()), "a") |> Result.isOk

    @test
    def many1Till03(): Bool & Impure =
        use TextParser.{runs, char, endOfInput};
        use TextParser.Combinators.{many1Till};
        runs(many1Till(char('a'), endOfInput()), "aa") |> Result.isOk

*/
}
