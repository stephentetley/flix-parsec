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

namespace FlixParsec/TokenSimple {

    use FlixParsec.{GenParser};
    use FlixParsec/Prim.{flatMap, return, liftResult};
    use FlixParsec/Combinators.{between, seqLeft, sepBy, sepBy1};
    use FlixParsec/Char.{string};
    use FlixParsec/Text.{whiteSpace, digits};
    use FlixParsec/Regex.{lookingAts};
    

    pub def lexeme(p: GenParser[ka1, st, r]): GenParser[ka1, st, r] = seqLeft(p, whiteSpace())

    pub def symbol(s: String): GenParser[ka1, st, String] = lexeme(string(s))

    pub def parens(p: GenParser[ka1, st, a]): GenParser[ka1, st, a] = between(symbol("("), symbol(")"), p)

    pub def braces(p: GenParser[ka1, st, a]): GenParser[ka1, st, a] = between(symbol("{"), symbol("}"), p)

    pub def angles(p: GenParser[ka1, st, a]): GenParser[ka1, st, a] = between(symbol("<"), symbol(">"), p)

    pub def brackets(p: GenParser[ka1, st, a]): GenParser[ka1, st, a] = between(symbol("["), symbol("]"), p)

    pub def squotes(p: GenParser[ka1, st, a]): GenParser[ka1, st, a] =  
        between(symbol("'"), symbol("'"), p)

    pub def dquotes(p: GenParser[ka1, st, a]): GenParser[ka1, st, a] = 
        let dquote = Char.toString('\u0022');
        between(symbol(dquote), symbol(dquote), p)

    pub def semi(): GenParser[ka1, st, String] = symbol(";")

    pub def comma(): GenParser[ka1, st, String] = symbol(",")

    pub def colon(): GenParser[ka1, st, String] = symbol(":")

    pub def dot(): GenParser[ka1, st, String] = symbol(".")

    pub def semiSep(p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = sepBy(p, semi())

    pub def commaSep(p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = sepBy(p, comma())

    pub def semiSep1(p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = sepBy1(p, semi())

    pub def commaSep1(p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = sepBy1(p, comma())

    pub def tupled(p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = parens(commaSep(p))

    pub def tupled1(p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = parens(commaSep1(p))

    pub def commaBraces(p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = braces(commaSep(p))
    
    pub def commaBraces1(p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = braces(commaSep1(p))

    pub def commaBrackets(p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = brackets(commaSep(p))
    
    pub def commaBrackets1(p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = brackets(commaSep1(p))

    pub def semiBraces(p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = braces(semiSep(p))
    
    pub def semiBraces1(p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = braces(semiSep1(p))

    pub def semiBrackets(p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = brackets(semiSep(p))
    
    pub def semiBrackets1(p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = brackets(semiSep1(p))

    pub def charLiteral(): GenParser[ka1, st, Char] =  
        lexeme(FlixParsec/Literal.charLiteral())

    pub def stringLiteral(): GenParser[ka1, st, String] =  
        lexeme(FlixParsec/Literal.stringLiteral())



    pub def int8(): GenParser[ka1, st, Int8] = 
        lexeme(FlixParsec/Literal.int8())

    pub def int16(): GenParser[ka1, st, Int16] = 
        lexeme(FlixParsec/Literal.int16())
    
    pub def int32(): GenParser[ka1, st, Int32] = 
        lexeme(FlixParsec/Literal.int32())
    

    pub def int64(): GenParser[ka1, st, Int64] = 
        lexeme(FlixParsec/Literal.int64())

    pub def bigInt(): GenParser[ka1, st, BigInt] = 
        lexeme(FlixParsec/Literal.bigInt())

    pub def float32(): GenParser[ka1, st, Float32] = 
        lexeme(FlixParsec/Literal.float32())

    pub def float64(): GenParser[ka1, st, Float64] = 
        lexeme(FlixParsec/Literal.float64())


    pub def hexInt32(): GenParser[ka1, st, Int32] = 
        lexeme(FlixParsec/Literal.hexInt32())

    pub def octInt32(): GenParser[ka1, st, Int32] = 
        lexeme(FlixParsec/Literal.octInt32())

}
