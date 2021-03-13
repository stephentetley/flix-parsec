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



namespace FlixParsec/Token1 {

    use FlixParsec.TextParser;
    use FlixParsec/Prim.{string, liftResult, seqLeft};
    use FlixParsec/Combinators.{between, sepBy, sepBy1};
    use FlixParsec/Regex.{lookingAts};
    use FlixParsec/Text.{whiteSpace, digits};

    pub def lexeme(p: TextParser[u, t1, r]): TextParser[u, t1, r] = seqLeft(p, whiteSpace())

    pub def symbol(s: String): TextParser[u, t1, String] = lexeme(string(s))

    pub def parens(p: TextParser[u, t1, a]): TextParser[u, t1, a] = between(symbol("("), symbol(")"), p)

    pub def braces(p: TextParser[u, t1, a]): TextParser[u, t1, a] = between(symbol("{"), symbol("}"), p)

    pub def angles(p: TextParser[u, t1, a]): TextParser[u, t1, a] = between(symbol("<"), symbol(">"), p)

    pub def brackets(p: TextParser[u, t1, a]): TextParser[u, t1, a] = between(symbol("["), symbol("]"), p)

    pub def semi(): TextParser[u, t1, String] = symbol(";")

    pub def comma(): TextParser[u, t1, String] = symbol(",")

    pub def colon(): TextParser[u, t1, String] = symbol(":")

    pub def dot(): TextParser[u, t1, String] = symbol(".")

    pub def semiSep(p: TextParser[u, t1, a]): TextParser[u, t1, List[a]] = sepBy(p, semi())

    pub def semiSep1(p: TextParser[u, t1, a]): TextParser[u, t1, List[a]] = sepBy1(p, semi())

    pub def commaSep(p: TextParser[u, t1, a]): TextParser[u, t1, List[a]] = sepBy(p, comma())

    pub def commaSep1(p: TextParser[u, t1, a]): TextParser[u, t1, List[a]] = sepBy1(p, comma())



    pub def int8(): TextParser[u, t1, Int8] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(Int8.fromString(s))
        };
        lexeme(body)

    pub def int16(): TextParser[u, t1, Int16] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(Int16.fromString(s))
        };
        lexeme(body)
    
    pub def int32(): TextParser[u, t1, Int32] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(Int32.fromString(s))
        };
        lexeme(body)

    pub def int64(): TextParser[u, t1, Int64] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(Int64.fromString(s))
        };
        lexeme(body)

    pub def bigInt(): TextParser[u, t1, BigInt] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(BigInt.fromString(s))
        };
        lexeme(body)

    pub def float64(): TextParser[u, t1, Float64] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+\.\d+");
            liftResult(Float64.fromString(s))
        };
        lexeme(body)

    pub def float32(): TextParser[u, t1, Float32] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+\.\d+");
            liftResult(Float32.fromString(s))
        };
        lexeme(body)
        
}