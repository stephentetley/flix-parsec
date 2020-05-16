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
use TextParser.{liftPrim, liftPrimResult, fail, return};
use TextParser/Combinators.{ignore};


namespace TextParser/Text {

    def atLeast1(p: Parser[String], errmsg: String): Parser[String] = 
        use TextParser.flatMap;
        let* s = p;
        if (s == "")
            fail(errmsg)
        else
            return(s)


    pub def whiteSpace(): Parser[Unit] =  
        liftPrim(TextParser/Internal/TextCursor.skipWhiteSpace)

    pub def whiteSpace1(): Parser[Unit] =  
        liftPrimResult(TextParser/Internal/TextCursor.whiteSpace1)


    
    pub def string(s: String): Parser[String] = 
        liftPrimResult(TextParser/Internal/TextCursor.string(s))
    
    pub def manyChar(c: Char): Parser[String] = 
        liftPrim(TextParser/Internal/TextCursor.manyChar(c))

    pub def many1Char(c: Char): Parser[String] = 
        atLeast1(manyChar(c), "many1Char")

    pub def skipChar(c: Char): Parser[Unit] = 
        use TextParser.flatMap;
        let* _ = liftPrim(TextParser/Internal/TextCursor.skipChar(c));
        return()


    pub def skip1Char(c: Char): Parser[Unit] = 
        use TextParser.flatMap;
        let* i = liftPrim(TextParser/Internal/TextCursor.skipChar(c));
        if (i <= 0)
            fail("skip1Char")
        else
            return()

    pub def spaces(): Parser[Unit] = 
        skipChar(' ')

    pub def spaces1(): Parser[Unit] = 
        skip1Char(' ')

    
    pub def manyAlphabetic(): Parser[String] =
        liftPrim(TextParser/Internal/TextCursor.manyAlphabetic)

    pub def many1Alphabetic(): Parser[String] =
        atLeast1(manyAlphabetic(), "many1Alphabetic")
        
    pub def manyLetter(): Parser[String] =
        liftPrim(TextParser/Internal/TextCursor.manyLetter)

    pub def many1Letter(): Parser[String] =
        atLeast1(manyLetter(), "many1Letter")

    pub def manyDigit(): Parser[String] =
        liftPrim(TextParser/Internal/TextCursor.manyDigit)

    pub def many1Digit(): Parser[String] =
        atLeast1(manyDigit(), "many1Digit")

    pub def manyLowerCase(): Parser[String] =
        liftPrim(TextParser/Internal/TextCursor.manyLowerCase)

    pub def many1LowerCase(): Parser[String] =
        atLeast1(manyLowerCase(), "many1LowerCase")

    pub def manyUpperCase(): Parser[String] =
        liftPrim(TextParser/Internal/TextCursor.manyUpperCase)

    pub def many1UpperCase(): Parser[String] =
        atLeast1(manyUpperCase(), "many1UpperCase")

    pub def manyLetterOrDigit(): Parser[String] =
        liftPrim(TextParser/Internal/TextCursor.manyLetterOrDigit)

    pub def many1LetterOrDigit(): Parser[String] =
        atLeast1(manyLetterOrDigit(), "many1LetterOrDigit")

}
