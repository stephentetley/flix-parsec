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

/// A version of Token that is not parameterized by LanguageDef.
/// At the lexical level there is no notion of comments, `lexeme` and all 
/// combinators derived with it see whitespace as the the usual space characters: 
/// space, tab, carriage return and line feed.

mod FlixParsec.TokenSimple {


    use GradedApplicative.{point, <*}

    use FlixParsec.{GenParser}
    use FlixParsec.Combinators.{between, sepBy, sepBy1, sepByWith}
    use FlixParsec.Char.{string}
    use FlixParsec.Text.{whiteSpace}

    

    pub def lexeme(p: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, a] = p <* whiteSpace()

    pub def symbol(s: String): GenParser[ka, ust, ef, String] = lexeme(string(s))

    pub def parens(p: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, a] = between(symbol("("), symbol(")"), p)

    pub def braces(p: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, a] = between(symbol("{"), symbol("}"), p)

    pub def angles(p: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, a] = between(symbol("<"), symbol(">"), p)

    pub def brackets(p: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, a] = between(symbol("["), symbol("]"), p)

    pub def squotes(p: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, a] =  
        between(symbol("'"), symbol("'"), p)

    pub def dquotes(p: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, a] = 
        let dquote = Char.toString('\u0022');
        between(symbol(dquote), symbol(dquote), p)

    pub def semi(): GenParser[ka, ust, ef, String] = symbol(";")

    pub def comma(): GenParser[ka, ust, ef, String] = symbol(",")

    pub def colon(): GenParser[ka, ust, ef, String] = symbol(":")

    pub def dot(): GenParser[ka, ust, ef, String] = symbol(".")

    pub def semiSep(p: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, List[a]] = sepBy(p, semi())

    pub def commaSep(p: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, List[a]] = sepBy(p, comma())

    pub def semiSep1(p: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, List[a]] = sepBy1(p, semi())

    pub def commaSep1(p: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, List[a]] = sepBy1(p, comma())

    /// TODO - fill out the with versions, but don't include `tupled` etc
    /// as they lead to hiding left recursion.
    pub def commaSepWith(combine: (ac, a) -> ac, ac: ac, p: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, ac] = 
        sepByWith(combine, ac, p, comma())


    pub def charLiteral(): GenParser[ka, ust, ef, Char] =  
        lexeme(FlixParsec.Literal.charLiteral())

    pub def stringLiteral(): GenParser[ka, ust, ef, String] =  
        lexeme(FlixParsec.Literal.stringLiteral())



    pub def int8(): GenParser[ka, ust, ef, Int8] = 
        lexeme(FlixParsec.Literal.int8())

    pub def int16(): GenParser[ka, ust, ef, Int16] = 
        lexeme(FlixParsec.Literal.int16())
    
    pub def int32(): GenParser[ka, ust, ef, Int32] = 
        lexeme(FlixParsec.Literal.int32())
    

    pub def int64(): GenParser[ka, ust, ef, Int64] = 
        lexeme(FlixParsec.Literal.int64())

    pub def bigInt(): GenParser[ka, ust, ef, BigInt] = 
        lexeme(FlixParsec.Literal.bigInt())

    pub def float32(): GenParser[ka, ust, ef, Float32] = 
        lexeme(FlixParsec.Literal.float32())

    pub def float64(): GenParser[ka, ust, ef, Float64] = 
        lexeme(FlixParsec.Literal.float64())


    pub def hexInt32(): GenParser[ka, ust, ef, Int32] = 
        lexeme(FlixParsec.Literal.hexInt32())

    pub def octInt32(): GenParser[ka, ust, ef, Int32] = 
        lexeme(FlixParsec.Literal.octInt32())


}
