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



namespace TextParser/Token1 {

    use TextParser.TextParser;
    use TextParser.{string, liftResult, lookingAts, seqLeft};
    use TextParser/Combinators.{between, sepBy, sepBy1};
    use TextParser/Text.{whiteSpace, digits};

    pub def lexeme(p: TextParser[r, t]): TextParser[r, t] = seqLeft(p, whiteSpace())

    pub def symbol(s: String): TextParser[String, t] = lexeme(string(s))

    pub def parens(p: TextParser[r, t]): TextParser[r, t] = between(symbol("("), symbol(")"), p)

    pub def braces(p: TextParser[r, t]): TextParser[r, t] = between(symbol("{"), symbol("}"), p)

    pub def angles(p: TextParser[r, t]): TextParser[r, t] = between(symbol("<"), symbol(">"), p)

    pub def brackets(p: TextParser[r, t]): TextParser[r, t] = between(symbol("["), symbol("]"), p)

    pub def semi(): TextParser[String, t] = symbol(";")

    pub def comma(): TextParser[String, t] = symbol(",")

    pub def colon(): TextParser[String, t] = symbol(":")

    pub def dot(): TextParser[String, t] = symbol(".")

    pub def semiSep(p: TextParser[r, t]): TextParser[List[r], t] = sepBy(p, semi())

    pub def semiSep1(p: TextParser[r, t]): TextParser[List[r], t] = sepBy1(p, semi())

    pub def commaSep(p: TextParser[r, t]): TextParser[List[r], t] = sepBy(p, comma())

    pub def commaSep1(p: TextParser[r, t]): TextParser[List[r], t] = sepBy1(p, comma())



    pub def int8(): TextParser[Int8, t] = 
        use TextParser.flatMap;
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(Int8.fromString(s))
        };
        lexeme(body)

    pub def int16(): TextParser[Int16, t] = 
        use TextParser.flatMap;
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(Int16.fromString(s))
        };
        lexeme(body)
    
    pub def int32(): TextParser[Int32, t] = 
        use TextParser.flatMap;
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(Int32.fromString(s))
        };
        lexeme(body)

    pub def int64(): TextParser[Int64, t] = 
        use TextParser.flatMap;
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(Int64.fromString(s))
        };
        lexeme(body)

    pub def bigInt(): TextParser[BigInt, t] = 
        use TextParser.flatMap;
        let body = { 
            let* s = lookingAts("-?\d+");
            liftResult(BigInt.fromString(s))
        };
        lexeme(body)

    pub def float64(): TextParser[Float64, t] = 
        use TextParser.flatMap;
        let body = { 
            let* s = lookingAts("-?\d+\.\d+");
            liftResult(Float64.fromString(s))
        };
        lexeme(body)

    pub def float32(): TextParser[Float32, t] = 
        use TextParser.flatMap;
        let body = { 
            let* s = lookingAts("-?\d+\.\d+");
            liftResult(Float32.fromString(s))
        };
        lexeme(body)
        
}