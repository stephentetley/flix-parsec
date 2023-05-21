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


mod FlixParsec.Literal {

    use GradedFunctor.{map}
    use GradedApplicative.{point, *>}
    use GradedMonad.{>>=}
    use GradedOr.{<|>}

    use FlixParsec.{GenParser}
    use FlixParsec.Prim.{throwError}
    use FlixParsec.Combinators.{alt, between, choice, manyWith, many1With, liftOption}
    use FlixParsec.Char.{char, string, satisfy, octDigit, hexDigit}
    use FlixParsec.Regex.{lookingAt}


    pub def stringLiteral(): GenParser[ka, ust, ef, String] = 
        let step = (ac, x) -> match x { 
            case Some(c) => "${ac}${c}"
            case None    => ac
        };
        between( char('\u0022'),  char('\u0022'), manyWith(step, "", stringChar())) 

    def stringChar(): GenParser[ka, ust, ef, Option[Char]] = 
        alt(stringLetter() |> map(Some), stringEscape())

    /// Greater than "SUB"...
    def stringLetter(): GenParser[ka, ust, ef, Char] = 
        satisfy(c -> (c != '\u0022') and (c != '\\') and (c > '\u001A'))

    def stringEscape(): GenParser[ka, ust, ef, Option[Char]] =
        char('\\') *> (escapeCode() |> map(Some))

    pub def charLiteral(): GenParser[ka, ust, ef, Char] = 
        between( char('\''),  char('\''), chararacterChar())

    def chararacterChar(): GenParser[ka, ust, ef, Char] = alt(charLetter(), charEscape())

    def charEscape(): GenParser[ka, ust, ef, Char] =
        char('\\') *> escapeCode()

    /// Greater than "SUB"...
    def charLetter(): GenParser[ka, ust, ef, Char] = 
        satisfy(c -> (c != '\'') and (c != '\\') and (c > '\u001A'))

    def escapeCode(): GenParser[ka, ust, ef, Char] = 
        choice(
            charUnicode() :: charEsc() :: Nil
        )
        /// charEsc <|> charNum <|> charAscii <|> charControl

    def charUnicode(): GenParser[ka, ust, ef, Char] = 
        (char('u') <|> char('U'))   >>= _ -> 
        hexNumber()                 >>= n -> 
        match CodePoint.toBmpChar(n) {
            case Some(ch)   => point(ch)
            case None       => throwError("charUnicode")
        }


    def hexNumber(): GenParser[ka, ust, ef, Int32] =  
        use FlixParsec.Combinators.{countWith};
        use FlixParsec.Char.{hexDigit};
        let step = (ac, ch) -> match Char.getNumericValue(ch) {
            case Some(i) => ac * 16 + i
            case None => ac * 16
        };
        countWith(step, 0, 4, hexDigit())
        

    def charEsc(): GenParser[ka, ust, ef, Char] = 
        choice(
                (char('t')  *> point('\u0009'))    /// tab
            :: (char('r')   *> point('\u000D'))    /// carriage return
            :: (char('n')   *> point('\u000A'))    /// line feed
            :: Nil
        )    



    pub def int8(): GenParser[ka, ust, ef, Int8] = 
        lookingAt(regex"-?\\d+")        >>= s ->
        liftOption("int8", Int8.fromString(s))
        
    pub def int16(): GenParser[ka, ust, ef, Int16] = 
        lookingAt(regex"-?\\d+")        >>= s ->
        liftOption("int16", Int16.fromString(s))
    
    pub def int32(): GenParser[ka, ust, ef, Int32] = 
        lookingAt(regex"-?\\d+")        >>= s -> 
        liftOption("int32", Int32.fromString(s))

    pub def int64(): GenParser[ka, ust, ef, Int64] = 
        lookingAt(regex"-?\\d+")        >>= s ->
        liftOption("int64", Int64.fromString(s))

    pub def bigInt(): GenParser[ka, ust, ef, BigInt] = 
        lookingAt(regex"-?\\d+")        >>= s ->
        liftOption("bigInt", BigInt.fromString(s))

    /// TODO - handle exponent notation...
    pub def float32(): GenParser[ka, ust, ef, Float32] = 
        lookingAt(regex"-?\\d+\\.\\d+") >>= s ->
        liftOption("float32", Float32.fromString(s))

    /// TODO - handle exponent notation...
    pub def float64(): GenParser[ka, ust, ef, Float64] = 
        lookingAt(regex"-?\\d+\\.\\d+") >>= s ->
        liftOption("float64", Float64.fromString(s))
 
    pub def hexInt32(): GenParser[ka, ust, ef, Int32] = 
        alt(string("0X"), string("0x")) *> hexInt32Base()
        
    pub def hexInt32Base(): GenParser[ka, ust, ef, Int32] = 
        let step = (ac, ch) -> match Char.getNumericValue(ch) {
            case Some(i) => ac * 16 + i
            case None => ac * 16
        };
        many1With(step, 0, hexDigit())

    pub def octInt32(): GenParser[ka, ust, ef, Int32] = 
        alt(string("0o"), string("0O")) *> octInt32Base()
        
    pub def octInt32Base(): GenParser[ka, ust, ef, Int32] = 
        let step = (ac, ch) -> match Char.digit(radix = 8, ch) {
            case Some(i) => ac * 8 + i
            case None    => ac * 8
        };
        many1With(step, 0, octDigit())        

}
