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

    pub def lexeme(p: TextParser[u, r, t]): TextParser[u, r, t] = seqLeft(p, whiteSpace())

    pub def symbol(s: String): TextParser[u, String, t] = lexeme(string(s))

    pub def parens(p: TextParser[u, r, t]): TextParser[u, r, t] = between(symbol("("), symbol(")"), p)

    pub def braces(p: TextParser[u, r, t]): TextParser[u, r, t] = between(symbol("{"), symbol("}"), p)

    pub def angles(p: TextParser[u, r, t]): TextParser[u, r, t] = between(symbol("<"), symbol(">"), p)

    pub def brackets(p: TextParser[u, r, t]): TextParser[u, r, t] = between(symbol("["), symbol("]"), p)

    pub def semi(): TextParser[u, String, t] = symbol(";")

    pub def comma(): TextParser[u, String, t] = symbol(",")

    pub def colon(): TextParser[u, String, t] = symbol(":")

    pub def dot(): TextParser[u, String, t] = symbol(".")

    pub def semiSep(p: TextParser[u, r, t]): TextParser[u, List[r], t] = sepBy(p, semi())

    pub def semiSep1(p: TextParser[u, r, t]): TextParser[u, List[r], t] = sepBy1(p, semi())

    pub def commaSep(p: TextParser[u, r, t]): TextParser[u, List[r], t] = sepBy(p, comma())

    pub def commaSep1(p: TextParser[u, r, t]): TextParser[u, List[r], t] = sepBy1(p, comma())



    pub def int8(): TextParser[u, Int8, t] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(Int8.fromString(s))
        };
        lexeme(body)

    pub def int16(): TextParser[u, Int16, t] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(Int16.fromString(s))
        };
        lexeme(body)
    
    pub def int32(): TextParser[u, Int32, t] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(Int32.fromString(s))
        };
        lexeme(body)

    pub def int64(): TextParser[u, Int64, t] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(Int64.fromString(s))
        };
        lexeme(body)

    pub def bigInt(): TextParser[u, BigInt, t] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(BigInt.fromString(s))
        };
        lexeme(body)

    pub def float64(): TextParser[u, Float64, t] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+\.\d+");
            liftResult(Float64.fromString(s))
        };
        lexeme(body)

    pub def float32(): TextParser[u, Float32, t] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+\.\d+");
            liftResult(Float32.fromString(s))
        };
        lexeme(body)
        
}