/*
 * Copyright 2019 Stephen Tetley
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
use TextParser.Parser.{Parser};
use TextParser.Scanner;
use TextParser.Scanner.{Scanner};

use TextParser.{map, return, bind, applyScanner, alt, satisfy, lookahead, makeParseError, nestError, restOfInput, horizon};
use TextParser/Combinators.{optional, manyTill, manyTillWith, 
        skipMany, manyTill2, many1Till, many, many1, seqLeft, seqRight};



///
/// Text and character oriented parsers.
///

namespace TextParser/Text {
    
    pub def anyChar(): Parser[Char] = 
        satisfy(_ -> true)

    pub def oneOf(cs: List[Char]): Parser[Char]= 
        satisfy(ch -> List.memberOf(ch, cs)) |> nestError("oneOf")

    pub def noneOf(cs: List[Char]): Parser[Char] = 
        satisfy(ch -> !List.memberOf(ch, cs)) |> nestError("noneOf")

    pub def char(c: Char): Parser[Char] = 
        satisfy(ch -> ch == c) |> nestError("char")

    pub def string(needle: String): Parser[String] = 
        import flix.runtime.textparser.PrimitiveScanners:exactString(String, String, Int32);
        Scanner((inp,pos) -> 
            let ans = exactString(needle, inp, pos) as & Pure;
            Ok(ans)
        ) |> applyScanner


    ///
    /// Byte order mark '\ufeff'
    ///
    pub def byteOrderMark(): Parser[Char] = char('\ufeff')

    
    pub def tab(): Parser[Char] = char('\t')

    ///
    /// Parse a space character (u0020)
    /// 
    pub def space(): Parser[Char] = char(' ')

    
    pub def upper(): Parser[Char] = satisfy(Char.isUpperCase)

    pub def lower(): Parser[Char] = satisfy(Char.isLowerCase)

    pub def letter(): Parser[Char] = satisfy(Char.isLetter)


    pub def letters(): Parser[String] =
        import flix.runtime.textparser.PrimitiveScanners:manyAlphabetic(String, Int32);
        Scanner((inp,pos) -> 
            let ans = manyAlphabetic(inp, pos) as & Pure;
            Ok(ans)
        ) |> applyScanner


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



    ///    
    /// newline() needs testing  - to test
    ///
    pub def newline(): Parser[Unit] = 
        TextParser.map(
            _ -> (),
            TextParser.alt(char('\n'), seqLeft(char('\r'), optional(char('\n'))))            
        ) 

    ///
    /// Parse a sequence space characters (u0020).
    /// 
    pub def spaces(): Parser[Unit] = skipMany(space())

    ///
    /// Parser any characaters before the end of line (or end of input).
    ///
    pub def restOfLine(consumeEol: Bool): Parser[String] = 
        import flix.runtime.textparser.PrimitiveScanners:restOfLine(Bool, String, Int32);
        Scanner((inp,pos) -> 
            let ans = restOfLine(consumeEol, inp, pos) as & Pure;
            if (consumeEol == false)
                Ok(ans)
            else {
                // Must consume something if consumeEol is true
                if (String.length(ans) == 0) 
                    Err("restOfLine")
                else
                    Ok(ans)
            } 

        ) |> applyScanner

    
    ///
    /// Either at end-of-input or only whitespace remaining before end-of-input.
    /// 
    pub def endOfContent(): Parser[Unit] = 
        seqRight(spaces(), TextParser.endOfInput())

    /// End of input or newline.
    pub def endOfLine() : Parser[Unit] = 
        alt(TextParser.endOfInput(), newline())




    /// Right padded with spaces.
    pub def rightPadded(p: Parser[a]): Parser[a] = 
        seqLeft(p, spaces())

    /// Left padded with spaces.
    pub def leftPadded(p: Parser[a]): Parser[a] = 
        seqRight(spaces(), p)

    /// Padded with spaces - left and/or right or neither.
    pub def padded(p: Parser[a]): Parser[a] = 
        rightPadded(leftPadded(p))

    pub def manyChars(p: Parser[Char]): Parser[String] = 
        TextParser.map(TextParser/Internal.makeString, many(p))

    pub def manyChars2(p1: Parser[Char], p2: Parser[Char]): Parser[String] = 
        bind(p1, c -> bind(many(p2), cs -> return(TextParser/Internal.makeString(c :: cs))))


    pub def many1Chars(p: Parser[Char]): Parser[String] = 
        TextParser.map(TextParser/Internal.makeString, many1(p))

    pub def many1Chars2(p1: Parser[Char], p2: Parser[Char]): Parser[String] = 
        TextParser.bind(p1, c -> TextParser.bind(many1(p2), cs -> return(TextParser/Internal.makeString(c :: cs))))

    pub def manyCharsTill(p: Parser[Char], endp: Parser[a]): Parser[String] = 
        map(TextParser/Internal.makeString, manyTill(p, endp))

    pub def manyCharsTill2(p1: Parser[Char], p2: Parser[Char], endp: Parser[a]): Parser[String] = 
        map(TextParser/Internal.makeString, manyTill2(p1, p2, endp))
             
    
    // pub def manyCharsTillApply(p: Parser[Char], endp: Parser[e], f: (String, e) -> a): Parser[a] = 
    //     manyTillWith((xs,e) -> f(TextParser/Internal.makeString(xs), e), p, endp)
        

    // pub def manyCharsTillApply2(p1: Parser[Char], p2: Parser[Char], endp: Parser[e], f: (String, e) -> a): Parser[a] = 
    //     let f1 = (c, ss, e) -> f(TextParser/Internal.prefix(c, ss), e);
    //     bind(p1, c1 -> manyCharsTillApply(p2, endp, f1(c1)))


    pub def many1CharsTill(p: Parser[Char], endp: Parser[e]): Parser[String] = 
        map(TextParser/Internal.makeString, many1Till(p, endp))

    pub def many1CharsTill2(p1: Parser[Char], p2: Parser[Char], endp: Parser[e]): Parser[String] = 
        use TextParser.flatMap;
        let* c = p1;
        let* ss = many1CharsTill(p2, endp);
        return(TextParser/Internal.prefix(c, ss))

    /// 
    /// Returns the "content" (i.e the trimmed text) of the next `n` characters from 
    /// the current position.
    ///
    pub def content(n: Int): Parser[String] = 
        map(String.trim, horizon(n))

}
