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
use TextParser.{map, nestError, satisfy, alt};
use TextParser/Combinators.{seqLeft, optional, many1, skipMany};

///
/// Character level parsers.
///

namespace TextParser/Char {

    pub def anyChar(): Parser[Char] = 
        use TextParser.Parser.Parser;
        use TextParser.makeParseError;
        Parser((inp, pos) -> match TextParser/Internal.charAtSafe(pos, inp) {
                case None => Err(makeParseError(inp, pos, "anyChar"))
                case Some(ch) => Ok(ch, pos+1)
            })

    pub def oneOf(cs: List[Char]): Parser[Char]= 
        satisfy(ch -> List.memberOf(ch, cs)) |> nestError("oneOf")

    pub def noneOf(cs: List[Char]): Parser[Char] = 
        satisfy(ch -> !List.memberOf(ch, cs)) |> nestError("noneOf")

    pub def char(c: Char): Parser[Char] = 
        satisfy(ch -> ch == c) |> nestError("char")

    ///
    /// Byte order mark '\ufeff'
    ///
    pub def byteOrderMark(): Parser[Char] = char('\ufeff')

    ///    
    /// newline() needs testing  - to test
    ///
    pub def newline(): Parser[Unit] = 
        TextParser.map(
            _ -> (),
            TextParser.alt(char('\n'), seqLeft(char('\r'), optional(char('\n'))))            
        ) 

    pub def tab(): Parser[Char] = char('\t')

    ///
    /// Parse a space character (u0020)
    /// 
    pub def space(): Parser[Char] = char(' ')

    ///
    /// Parse a sequence space characters (u0020).
    /// 
    pub def spaces(): Parser[Unit] = skipMany(space())

    pub def upper(): Parser[Char] = satisfy(Char.isUpperCase)

    pub def lower(): Parser[Char] = satisfy(Char.isLowerCase)

    pub def letter(): Parser[Char] = satisfy(Char.isLetter)

    pub def letters(): Parser[String] = 
        many1(letter()) |> TextParser.map(List.toString)

    pub def alphaNum(): Parser[Char] = alt(letter(), digit())

    pub def digit(): Parser[Char] = satisfy(Char.isDigit)

    pub def digits(): Parser[String] = 
        many1(digit()) |> map(List.toString)

    pub def octDigit(): Parser[Char] = 
        let xs = '0' :: '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: Nil;
        oneOf(xs)

    pub def hexDigit(): Parser[Char] = 
        let xs = '0' :: '1' :: '2' :: '3' :: '4' :: '5' :: '6' :: '7' :: '8' :: '9' :: 'A' :: 'B' :: 'C' :: 'D' :: 'E' ::'F' ::'a' :: 'b' :: 'c' :: 'd' :: 'e' :: 'f' :: Nil;
        oneOf(xs)



}