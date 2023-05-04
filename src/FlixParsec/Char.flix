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


mod FlixParsec.Char {

    use GradedApplicative.{point}
    use GradedMonad.{>>=}
    use GradedError.{swapError}

    use FlixParsec.{GenParser}
    use FlixParsec.Prim.{fail}

    pub def anyChar(): GenParser[ka, ust, ef, Char] = FlixParsec.Prim.take1()

    pub def satisfy(test: Char -> Bool \ ef1): GenParser[ka, ust, ef and ef1, Char] = 
        anyChar()       >>= ans ->
        if (test(ans))
            point(ans)
        else
            fail("satisfy")

    pub def oneOf(cs: List[Char]): GenParser[ka, ust, ef, Char] = 
        satisfy(c -> List.memberOf(c, cs))

    pub def noneOf(cs: List[Char]): GenParser[ka, ust, ef, Char] = 
        satisfy(c -> not List.memberOf(c, cs))

    pub def char(c: Char): GenParser[ka, ust, ef, Char] = 
        swapError("char('${c}')", satisfy(x -> x == c))

    pub def string(s: String): GenParser[ka, ust, ef, String] =
        FlixParsec.Prim.take(String.length(s)) >>= s1 ->
        if (s1 == s)
            point(s)
        else
            fail("string")


    pub def upper(): GenParser[ka, ust, ef, Char] = 
        satisfy(Char.isUpperCase)

    pub def lower(): GenParser[ka, ust, ef, Char] = 
        satisfy(Char.isLowerCase)

    pub def letter(): GenParser[ka, ust, ef, Char] = 
        satisfy(Char.isLetter)

    pub def alphaNum(): GenParser[ka, ust, ef, Char] = 
        satisfy(c -> Char.isLetter(c) or Char.isAsciiDigit(c))

    pub def digit(): GenParser[ka, ust, ef, Char] = 
        satisfy(Char.isAsciiDigit)

    pub def octDigit(): GenParser[ka, ust, ef, Char] = 
        satisfy(Char.isOctDigit)

    pub def hexDigit(): GenParser[ka, ust, ef, Char] = 
        satisfy(Char.isHexDigit)


    pub def carriageReturn(): GenParser[ka, ust, ef, Char] = char('\u000D')

    pub def linefeed(): GenParser[ka, ust, ef, Char] = char('\u000A')

    pub def newline(): GenParser[ka, ust, ef, Char] = linefeed()

    pub def tab(): GenParser[ka, ust, ef, Char] = char('\u0009')

    /// [ \t\n\x0B\f\r] 
    /// \t is tab 
    /// \n is line feed
    /// \x0B is vertical tab
    /// \f is form feed
    /// \r is carriage return
    pub def space(): GenParser[ka, ust, ef, Char] = 
        satisfy(c -> c == ' ' or c == '\u0009' or c == '\u000A' or c == '\u000B' or c == '\u000C' or c == '\u000D')        

}
