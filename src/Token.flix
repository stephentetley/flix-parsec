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
use TextParser.{return, bind, map, throwError, alt, satisfy, string, liftM2};
use TextParser/Combinators.{sepBy, sepBy1, between, skipMany, many1, option, seqLeft, string4};
use TextParser/Char.{digit};

///
/// Token oriented parsers.
///

namespace TextParser/Token {
    /// Read many whiteSpace characters (space, tab, newline)
    pub def whiteSpace(): Parser[Unit] = skipMany(satisfy(Char.isWhiteSpace))



    pub def lexeme(p: Parser[ans]): Parser[ans] = seqLeft(p, whiteSpace())

    pub def symbol(s: String): Parser[String] = lexeme(string(s))

    pub def parens(p: Parser[ans]): Parser[ans] = between(symbol("("), symbol(")"), p)

    pub def braces(p: Parser[ans]): Parser[ans] = between(symbol("{"), symbol("}"), p)

    pub def brackets(p: Parser[ans]): Parser[ans] = between(symbol("<"), symbol(">"), p)

    pub def squares(p: Parser[ans]): Parser[ans] = between(symbol("["), symbol("]"), p)

    pub def semi(): Parser[String] = symbol(";")

    pub def comma(): Parser[String] = symbol(",")

    pub def colon(): Parser[String] = symbol(":")

    pub def dot(): Parser[String] = symbol(".")

    pub def semiSep(p: Parser[ans]): Parser[List[ans]] = sepBy(p, semi())

    pub def semiSep1(p: Parser[ans]): Parser[List[ans]] = sepBy1(p, semi())

    pub def commaSep(p: Parser[ans]): Parser[List[ans]] = sepBy(p, comma())

    pub def commaSep1(p: Parser[ans]): Parser[List[ans]] = sepBy1(p, comma())

    // TODO - charLiteral and stringLiteral
    // Number parser use lexeme which consumes trailing whitespace.
    // We may want a choice of trailing whitespace, trailing spaces, or no trailing.
    // How do we resolve this?


    def sign(): Parser[String] = 
        option("", alt(string("+"), string("-")))

    def digitString(): Parser[String] = 
        many1(digit()) |> map(List.toString)

    def intString(): Parser[String] = 
        liftM2((x,y) -> x + y, sign(), digitString())

    def primInt8(): Parser[Int8] =
        bind(intString(), v1 -> match Int8.fromString(v1) {
            case Err(_) => throwError("int8")
            case Ok(i) => return(i)
        })

    pub def int8(): Parser[Int8] = lexeme(primInt8())

    def primInt16(): Parser[Int16] =
        bind(intString(), v1 -> match Int16.fromString(v1) {
            case Err(_) => throwError("int16")
            case Ok(i) => return(i)
        })

    pub def int16(): Parser[Int16] = lexeme(primInt16())

    def primInt32(): Parser[Int32] =
        bind(intString(), v1 -> match Int32.fromString(v1) {
            case Err(_) => throwError("int32")
            case Ok(i) => return(i)
        })

    pub def int32(): Parser[Int32] = lexeme(primInt32())

    def primInt64(): Parser[Int64] =
        bind(intString(), v1 -> match Int64.fromString(v1) {
            case Err(_) => throwError("int64")
            case Ok(i) => return(i)
        })

    pub def int64(): Parser[Int64] = lexeme(primInt64())

    def primBigInt(): Parser[BigInt] =
        bind(intString(), v1 -> match BigInt.fromString(v1) {
            case Err(_) => throwError("bigInt")
            case Ok(i) => return(i)
        })

    pub def bigInt(): Parser[BigInt] = lexeme(primBigInt())
    
    def floatString(): Parser[String] = 
        string4( 
            sign(), 
            intString(),
            string("."),
            intString())
    
    def primFloat32(): Parser[Float32] =
        bind(floatString(), v1 -> match Float32.fromString(v1) {
            case Err(_) => throwError("float32")
            case Ok(i) => return(i)
        })

    pub def float32(): Parser[Float32] = lexeme(primFloat32())
    
    def primFloat64(): Parser[Float64] =
        bind(floatString(), v1 -> match Float64.fromString(v1) {
            case Err(_) => throwError("float64")
            case Ok(i) => return(i)
        })

    pub def float64(): Parser[Float64] = lexeme(primFloat64())    

    /// This is a bit limited without existentials
    pub opaque type SymbolParser = 
        { f64: Parser[Float64] 
        }

    pub def makeSymbolParser(trailing: Parser[Unit]): SymbolParser =
        let pLexeme = p -> seqLeft(p, trailing);
        let pFloat64 = pLexeme(primFloat64());
        let r1 = { f64 = pFloat64 };
        SymbolParser(r1)


}