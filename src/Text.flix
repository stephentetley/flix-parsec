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
use TextParser.Parser.Parser;
use TextParser.{map, return, bind, alt, makeParseError};
use TextParser/Combinators.{manyTill, manyTillWith, manyTill2, many1Till, many, many1, seqLeft, seqRight};
use TextParser.nestError;
use TextParser/Char.spaces;


///
/// Text oriented parsers.
///

namespace TextParser/Text {
    
    ///
    /// Parser any characaters before the end of line (or end of input).
    ///
    pub def restOfLine(consumeEol: Bool): Parser[String] = 
        let endp = TextParser.alt(newline1(consumeEol), TextParser.endOfInput());
        manyCharsTill(TextParser/Char.anyChar(), endp)

    def newline1(consumeEol: Bool): Parser[Unit] = 
        if (consumeEol) TextParser/Char.newline() else TextParser.lookahead(TextParser/Char.newline())

    ///
    /// Either at end-of-input or only whitespace remaining before end-of-input.
    /// 
    pub def endOfContent(): Parser[Unit] = 
        seqRight(spaces(), TextParser.endOfInput())

    /// End of input or newline.
    pub def endOfLine() : Parser[Unit] = 
        alt(TextParser.endOfInput(), TextParser/Char.newline())

    pub def restOfInput(): Parser[String] = 
        Parser((inp, pos) -> 
            let end = String.length(inp);
            match TextParser/Internal.trySlice(pos, end, inp) {
                case None => Err(makeParseError(inp, pos, "restOfInput"))
                case Some(s) => Ok(s, end)
            })



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
             
    
    pub def manyCharsTillApply(p: Parser[Char], endp: Parser[e], f: (String, e) -> a): Parser[a] = 
        manyTillWith((xs,e) -> f(TextParser/Internal.makeString(xs), e), p, endp)
        

    pub def manyCharsTillApply2(p1: Parser[Char], p2: Parser[Char], endp: Parser[e], f: (String, e) -> a): Parser[a] = 
        let f1 = (c, ss, e) -> f(TextParser/Internal.prefix(c, ss), e);
        bind(p1, c1 -> manyCharsTillApply(p2, endp, f1(c1)))


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
