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


namespace FlixParsec/Char {

    use FlixParsec.{GenParser};
    use FlixParsec/Prim.{return, flatMap, fail, map, lookahead, swapError, liftOption};
    use FlixParsec/Combinators.{ignore};
    use FlixParsec/Regex.{lookingAts, skippingAts};


    pub def satisfy(test: Char -> Bool): GenParser[t1, u, Char] = 
        let* a = anyChar();
        if (test(a))
            return(a)
        else
            fail("satisfy")

    pub def oneOf(cs: List[Char]): GenParser[t1, u, Char] = 
        satisfy(c -> List.memberOf(c, cs))

    pub def noneOf(cs: List[Char]): GenParser[t1, u, Char] = 
        satisfy(c -> not List.memberOf(c, cs))

    pub def char(c: Char): GenParser[t1, u, Char] = 
        satisfy(x -> x == c) `swapError` "char('${c}')"

    pub def string(s: String): GenParser[t1, u, String] =
        let* s1 = FlixParsec/Prim.take(String.length(s));
        if (s1 == s)
            return(s)
        else
            fail("string")

    pub def anyChar(): GenParser[t1, u, Char] = FlixParsec/Prim.take1()


    pub def octDigit(): GenParser[t1, u, Char] = 
        oneOf('0' :: '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: Nil)


    pub def carriageReturn(): GenParser[t1, u, Char] = char('\u000D')

    pub def linefeed(): GenParser[t1, u, Char] = char('\u000A')

    pub def newline(): GenParser[t1, u, Char] = linefeed()

    pub def tab(): GenParser[t1, u, Char] = char('\u0009')

    /// [ \t\n\x0B\f\r] 
    /// \t is tab 
    /// \n is line feed
    /// \x0B is vertical tab
    /// \f is form feed
    /// \r is carriage return
    pub def space(): GenParser[t1, u, Char] = 
        satisfy(c -> c == ' ' or c == '\u0009' or c == '\u000A' or c == '\u000B' or c == '\u000C' or c == '\u000D')        

    /// one or more
    pub def spaces(): GenParser[t1, u, Unit] = skippingAts("\s*")


}
