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

    /// Version of `atLeast1` for "skipping" (counting) parsers.
    def skipAtLeast1(p: Parser[Int32], errmsg: String): Parser[Unit] = 
        use TextParser.flatMap;
        let* i = p;
        if (i <= 0)
            fail(errmsg)
        else
            return()


    pub def whiteSpace(): Parser[Unit] =  
        liftPrim(TextParser/Internal/TextCursor.skipWhiteSpace)

    pub def whiteSpace1(): Parser[Unit] =  
        liftPrimResult(TextParser/Internal/TextCursor.whiteSpace1)


    
    pub def string(s: String): Parser[String] = 
        liftPrimResult(TextParser/Internal/TextCursor.string(s))
    
    pub def char(c: Char): Parser[Char] = 
        liftPrimResult(TextParser/Internal/TextCursor.char(c))

    pub def anyChar(): Parser[Char] = 
        liftPrimResult(TextParser/Internal/TextCursor.anyChar)

    pub def manyChar(c: Char): Parser[String] = 
        liftPrim(TextParser/Internal/TextCursor.manyChar(c))

    pub def many1Char(c: Char): Parser[String] = 
        atLeast1(manyChar(c), "many1Char")

    pub def skipChar(c: Char): Parser[Unit] = 
        use TextParser.flatMap;
        let* _ = liftPrim(TextParser/Internal/TextCursor.skipChar(c));
        return()

    pub def skip1Char(c: Char): Parser[Unit] = 
        skipAtLeast1(liftPrim(TextParser/Internal/TextCursor.skipChar(c)), "skip1Char")

    pub def countChar(n: Int32, c: Char): Parser[String] = 
        liftPrimResult(TextParser/Internal/TextCursor.countChar(n, c)) 

    pub def manyNotChar(c: Char): Parser[String] = 
        liftPrim(TextParser/Internal/TextCursor.manyNotChar(c))

    pub def many1NotChar(c: Char): Parser[String] = 
        atLeast1(manyNotChar(c), "many1NotChar")

    pub def skipNotChar(c: Char): Parser[Unit] = 
        use TextParser.flatMap;
        let* _ = liftPrim(TextParser/Internal/TextCursor.skipNotChar(c));
        return()

    pub def skip1NotChar(c: Char): Parser[Unit] = 
        skipAtLeast1(liftPrim(TextParser/Internal/TextCursor.skipNotChar(c)), "skip1NotChar")

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


    pub def manyOneOf(cs: List[Char]): Parser[String] = 
        liftPrim(TextParser/Internal/TextCursor.manyOneOf(cs))

    pub def many1OneOf(cs: List[Char]): Parser[String] = 
        atLeast1(manyOneOf(cs), "many1OneOf")

    pub def skipOneOf(cs: List[Char]): Parser[Unit] = 
        use TextParser.flatMap;
        let* _ = liftPrim(TextParser/Internal/TextCursor.skipOneOf(cs));
        return()

    pub def skip1OneOf(cs: List[Char]): Parser[Unit] =
        skipAtLeast1(liftPrim(TextParser/Internal/TextCursor.skipOneOf(cs)), "skip1OneOf")

    pub def manyNoneOf(cs: List[Char]): Parser[String] = 
        liftPrim(TextParser/Internal/TextCursor.manyNoneOf(cs))

    pub def many1NoneOf(cs: List[Char]): Parser[String] = 
        atLeast1(manyNoneOf(cs), "many1NoneOf")

    pub def skipNoneOf(cs: List[Char]): Parser[Unit] = 
        use TextParser.flatMap;
        let* _ = liftPrim(TextParser/Internal/TextCursor.skipNoneOf(cs));
        return()

    pub def skip1NoneOf(cs: List[Char]): Parser[Unit] =
        skipAtLeast1(liftPrim(TextParser/Internal/TextCursor.skipNoneOf(cs)), "skip1NoneOf")

    pub def lookingAt(patt: Regex.Pattern): Parser[String] = 
        liftPrimResult(TextParser/Internal/TextCursor.lookingAt(patt))

    pub def compilePattern(regex: String): Parser[Regex.Pattern] = 
        match Regex/Pattern.compile(regex) {
            case Ok(patt) => return(patt)
            case Err(_) => fail("PatternSyntaxException")
        }

    pub def newline(): Parser[Unit] = 
        use TextParser.flatMap;
        let* patt = compilePattern("\R");
        let* _ = lookingAt(patt);
        return()

    pub def charsTillString(needle: String): Parser[String] = 
        liftPrim(TextParser/Internal/TextCursor.charsTillString(needle))

    pub def chars1TillString(needle: String): Parser[String] = 
        atLeast1(charsTillString(needle), "chars1TillString")     

    pub def skipCharsTillString(needle: String): Parser[Unit] = 
        use TextParser.flatMap;
        let* _ = liftPrim(TextParser/Internal/TextCursor.skipCharsTillString(needle));
        return()

    pub def skip1CharsTillString(needle: String): Parser[Unit] = 
        skipAtLeast1(liftPrim(TextParser/Internal/TextCursor.skipCharsTillString(needle)), "skip1CharsTillString")

    pub def restOfLine(consumeEol: Bool): Parser[String] =
        liftPrim(TextParser/Internal/TextCursor.restOfLine(consumeEol))

    pub def skipRestOfLine(consumeEol: Bool): Parser[Unit] =
        use TextParser.flatMap;
        let* _ = liftPrim(TextParser/Internal/TextCursor.skipRestOfLine(consumeEol));
        return()

    pub def isEof(): Parser[Bool] = 
        liftPrim(TextParser/Internal/TextCursor.isEof)
}
