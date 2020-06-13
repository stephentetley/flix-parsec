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


use TextParser.TextParser;
use TextParser.{return, map, lookingAts, skippingAts};
use TextParser/Combinators.{liftResult};

namespace TextParser/Text {



    // Parse a newline recognized by the regular expression "\R".
    pub def newline(): TextParser[Unit, t] = skippingAts("\R")
        


    // Parse a newline recognized by the regular expression "\R".
    pub def restOfLine(consumeNewline: Bool): TextParser[String, t] = 
        let regex = if (consumeNewline) ".*\R?" else ".*";
        lookingAts(regex)
        
    /// zero or more
    pub def whiteSpace(): TextParser[Unit, t] = skippingAts("\s*")

    
    /// one or more
    pub def whiteSpace1(): TextParser[Unit, t] = skippingAts("\s+")

    /// space or tab, zero or more
    pub def blank(): TextParser[Unit, t] = skippingAts("\p{Blank}*")

    
    /// space or tab,  one or more
    pub def blank1(): TextParser[Unit, t] = skippingAts("\p{Blank}+")

    /// zero or more
    pub def horizontalWhiteSpace(): TextParser[Unit, t] = skippingAts("\h*")

    
    /// one or more
    pub def horizontalWhiteSpace1(): TextParser[Unit, t] = skippingAts("\h+")

    pub def manyChar(c: Char): TextParser[String, t] =
        let regex = Text/Regex/Pattern.quote(Char.toString(c)) + "*";
        lookingAts(regex)

    pub def many1Char(c: Char): TextParser[String, t] =
        let regex = Text/Regex/Pattern.quote(Char.toString(c)) + "+";
        lookingAts(regex)

        
    pub def content(n: Int32): TextParser[String, t] =
        map(String.trim, TextParser.horizon(n))

    /// One or more ASCII digits [0-9].
    pub def digits(): TextParser[String, t] = lookingAts("\d+")

    /// One or more ASCII hex digits.
    pub def hexDigits(): TextParser[String, t] = lookingAts("\p{XDigit}+")

    /// One or more.
    pub def lowerLetters(): TextParser[String, t] = lookingAts("\p{Lower}+")

    /// One or more.
    pub def upperLetters(): TextParser[String, t] = lookingAts("\p{Upper}+")

    /// One or more.
    pub def alphas(): TextParser[String, t] = lookingAts("\p{Alpha}+")

    /// One or more.
    pub def alphanums(): TextParser[String, t] = lookingAts("\p{Alnum}+")


    pub def isEof(): TextParser[Bool, t] = 
        TextParser/Combinators.succeeds(TextParser.endOfInput())
        
}
