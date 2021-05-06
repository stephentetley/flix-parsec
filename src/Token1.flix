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

    use FlixParsec.{GenParser};
    use FlixParsec/Prim.{liftResult, seqLeft};
    use FlixParsec/Combinators.{between, sepBy, sepBy1};
    use FlixParsec/Char.{string};
    use FlixParsec/Text.{whiteSpace, digits};
    use FlixParsec/Regex.{lookingAts};
    

    pub def lexeme(p: GenParser[t1, u, r]): GenParser[t1, u, r] = seqLeft(p, whiteSpace())

    pub def symbol(s: String): GenParser[t1, u, String] = lexeme(string(s))

    pub def parens(p: GenParser[t1, u, a]): GenParser[t1, u, a] = between(symbol("("), symbol(")"), p)

    pub def braces(p: GenParser[t1, u, a]): GenParser[t1, u, a] = between(symbol("{"), symbol("}"), p)

    pub def angles(p: GenParser[t1, u, a]): GenParser[t1, u, a] = between(symbol("<"), symbol(">"), p)

    pub def brackets(p: GenParser[t1, u, a]): GenParser[t1, u, a] = between(symbol("["), symbol("]"), p)

    pub def semi(): GenParser[t1, u, String] = symbol(";")

    pub def comma(): GenParser[t1, u, String] = symbol(",")

    pub def colon(): GenParser[t1, u, String] = symbol(":")

    pub def dot(): GenParser[t1, u, String] = symbol(".")

    pub def semiSep(p: GenParser[t1, u, a]): GenParser[t1, u, List[a]] = sepBy(p, semi())

    pub def semiSep1(p: GenParser[t1, u, a]): GenParser[t1, u, List[a]] = sepBy1(p, semi())

    pub def commaSep(p: GenParser[t1, u, a]): GenParser[t1, u, List[a]] = sepBy(p, comma())

    pub def commaSep1(p: GenParser[t1, u, a]): GenParser[t1, u, List[a]] = sepBy1(p, comma())



    pub def int8(): GenParser[t1, u, Int8] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(Int8.fromString(s))
        };
        lexeme(body)

    pub def int16(): GenParser[t1, u, Int16] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(Int16.fromString(s))
        };
        lexeme(body)
    
    pub def int32(): GenParser[t1, u, Int32] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(Int32.fromString(s))
        };
        lexeme(body)

    pub def int64(): GenParser[t1, u, Int64] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(Int64.fromString(s))
        };
        lexeme(body)

    pub def bigInt(): GenParser[t1, u, BigInt] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(BigInt.fromString(s))
        };
        lexeme(body)

    pub def float64(): GenParser[t1, u, Float64] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+\.\d+");
            liftResult(Float64.fromString(s))
        };
        lexeme(body)

    pub def float32(): GenParser[t1, u, Float32] = 
        use FlixParsec/Prim.{flatMap};
        let body = { 
            let* s = lookingAts("-?\d+\.\d+");
            liftResult(Float32.fromString(s))
        };
        lexeme(body)
        
        
}