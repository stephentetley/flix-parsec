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

use TextParser/Error.{ParseError};
use TextParser/Internal.{TextCursor};

namespace TextParser {

    type alias ParseResult[a] = Result[a, ParseError]
        
    
    /// TODO - We could have an env with tabsize and a function trailing: Char -> Bool
    pub enum Parser[ans] {
        case Parser(TextCursor -> ParseResult[ans])
    }

    ///
    /// Alias for `parseString`.
    ///
    pub def run(parser: Parser[a], input: String): Result[a, ParseError] = parseString(parser, input)
    
    pub def parseString(parser: Parser[a], input: String): Result[a, ParseError] = 
        parseStringHelper(parser, input) as & Pure

    pub def parseStringHelper(parser: Parser[a], input: String): Result[a, ParseError] & Impure = 
        let Parser(f) = parser;
        let cursor = TextParser/Internal/TextCursor.new(input);
        f(cursor)

    ///
    /// Simplified version of `run`. On failure the ParseError is converted to a string.
    ///    
    pub def runs(parser: Parser[a], input: String): Result[a, String] = 
        match parseString(parser, input) {
            case Err(e) => Err(TextParser/Error.showParserError(e))
            case Ok(a) => Ok(a) 
        }

    pub def return(x: a): Parser[a] = Parser(_ -> Ok(x))

    pub def map(f: a -> b, p: Parser[a]): Parser[a] =
        Parser(s -> 
            let Parser(p1) = p;
            match p1(s) {
                case Ok(a) => Ok(f(a))
                case Err(e) => Err(e)
            }
        )
    
    pub def ap(pf: Parser[a -> b], p: Parser[a]): Parser[b] =
        Parser(s -> 
            let Parser(pf1) = pf;
            match pf1(s) {
                case Ok(f) => { 
                    let Parser(p1) = p;
                    match p1(s) { 
                        case Ok(a) => Ok(f(a))
                        case Err(e) => Err(e)
                    }
                }
                case Err(e) => Err(e)
            }
        )
 
    pub def bind(p: Parser[a], k: a -> Parser[b]): Parser[b] =
        Parser(s -> 
            let Parser(p1) = p;
            match p1(s) {
                case Ok(a) => { 
                    let Parser(p2) = k(a);
                    p2(s)
                }
                case Err(e) => Err(e)
            }
        )
 
    pub def flatMap(k: a -> Parser[b], p: Parser[a]): Parser[b] = bind(p, k)
        
 

}