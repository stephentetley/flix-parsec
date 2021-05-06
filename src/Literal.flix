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


namespace FlixParsec/Literal {

    use FlixParsec.{GenParser};
    use FlixParsec/Prim.{return, flatMap, map, swapError, liftResult};
    use FlixParsec/Combinators.{alt, choice, between};
    use FlixParsec/Combinators.{*>};
    use FlixParsec/Char.{char, satisfy};
    use FlixParsec/Regex.{LookingAts};

    pub def charLiteral(): GenParser[t1, u, Char] = 
        between( char('\''),  char('\''), chararacterChar())

    def chararacterChar(): GenParser[t1, u, Char] = alt(charLetter(), charEscape())

    def charEscape(): GenParser[t1, u, Char] =
        char('\\') *> escapeCode()

    /// Greater than "SUB"...
    def charLetter(): GenParser[t1, u, Char] = 
        satisfy(c -> (c != '\'') and (c != '\\') and (c > '\u001A'))

    def escapeCode(): GenParser[t1, u, Char] = 
        choice(
            charUnicode() :: charEsc() :: Nil
        )
        /// charEsc <|> charNum <|> charAscii <|> charControl

    def charUnicode(): GenParser[t1, u, Char] = 
        let* _      = alt(char('u'), char('U'));
        let* num    = number() |> map(Char.fromInt32);
        return(num) 


    def number(): GenParser[t1, u, Int32] =  
        use FlixParsec/Combinators.{countWith};
        use FlixParsec/Char.{hexDigit};
        let step = (ac, ch) -> match asciiDigitToInt(ch) {
            case Some(i) => ac * 16 + i
            case None => ac * 16
        };
        countWith(step, 0, 4, hexDigit())
        

    def charEsc(): GenParser[t1, u, Char] = 
        choice(
                (char('t')  *> return('\u0009'))    /// tab
            :: (char('r')   *> return('\u000D'))    /// carriage return
            :: (char('n')   *> return('\u000A'))    /// line feed
            :: Nil
        )    

    def asciiDigitToInt(c: Char): Option[Int32] = 
        match Char.toInt32(c) {
            case i if i >= 0x0030 and i <= 0x0039 => Some(i - 0x0030)        // '0'..'9'
            case i if i >= 0x0041 and i <= 0x0046 => Some(i - 0x0041)        // 'A'..'F'
            case i if i >= 0x0061 and i <= 0x0066 => Some(i - 0x0061)        // 'a'..'f'
            case _ => None
        }
}
