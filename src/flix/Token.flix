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
use TextParser.{swapError};
use TextParser/Combinators.{liftResult, seqLeft, between, sepBy, sepBy1};
use TextParser/Text.{string, compilePattern, lookingAt, whiteSpace};

namespace TextParser/Token {

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


    pub def intString(): Parser[String] = 
        use TextParser.flatMap;
        let* p1 = compilePattern("\-?\d+");
        lookingAt(p1)

    pub def int8(): Parser[Int8] = 
        use TextParser.flatMap;
        let* s = intString();
        liftResult(Int8.fromString(s)) |> swapError("int8") |> lexeme

    pub def int16(): Parser[Int16] = 
        use TextParser.flatMap;
        let* s = intString();
        liftResult(Int16.fromString(s)) |> swapError("int16") |> lexeme
    
    pub def int32(): Parser[Int32] = 
        use TextParser.flatMap;
        let* s = intString();
        liftResult(Int32.fromString(s)) |> swapError("int32") |> lexeme
    
    pub def int64(): Parser[Int64] =
        use TextParser.flatMap;
        let* s = intString();
        liftResult(Int64.fromString(s)) |> swapError("int64") |> lexeme
    

}