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

use TextParser/Error.{ParseError, PrimError};
use TextParser/Internal.{TextCursor};

namespace TextParser {

    type alias ParseResult[a] = Result[a, ParseError]

    type alias Config = { tabsize: Int32 }
        
    def defaultConfig(): Config = { tabsize = 4 }
    
    /// TODO - We could have an env with tabsize and a function trailing: Char -> Bool
    pub enum Parser[ans] {
        case Parser((Config, TextCursor) -> ParseResult[ans] & Impure)
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
        f(defaultConfig(), cursor)

    ///
    /// Simplified version of `run`. On failure the ParseError is converted to a string.
    ///    
    pub def runs(parser: Parser[a], input: String): Result[a, String] = 
        match parseString(parser, input) {
            case Err(e) => Err(TextParser/Error.showParserError(e))
            case Ok(a) => Ok(a) 
        }

    pub def parseFile(parser: Parser[a], path: System.FilePath, cs: Text.Charset): Result[a, ParseError] & Impure = 
        match System/File.readFile(path, cs) {
            case Err(_) => Err(TextParser/Error.makeParseError("", 0, 0, "IO Error"))
            case Ok(text) => run(parser, text)
        }


    pub def return(x: a): Parser[a] = Parser((_,_) -> Ok(x) as & Impure)

    pub def fail(msg: String): Parser[ans] = parseError(msg)

    pub def failure(e: ParseError) : Parser[ans] = Parser((_, _) -> Err(e) as & Impure)

    pub def map(f: a -> b, p: Parser[a]): Parser[a] =
        Parser((r,s) -> 
            let Parser(p1) = p;
            match p1(r, s) {
                case Ok(a) => Ok(f(a))
                case Err(e) => Err(e)
            }
        )
    
    pub def ap(pf: Parser[a -> b], p: Parser[a]): Parser[b] =
        Parser((r,s) -> 
            let Parser(pf1) = pf;
            match pf1(r,s) {
                case Ok(f) => { 
                    let Parser(p1) = p;
                    match p1(r,s) { 
                        case Ok(a) => Ok(f(a))
                        case Err(e) => Err(e)
                    }
                }
                case Err(e) => Err(e)
            }
        )
 
    pub def bind(p: Parser[a], k: a -> Parser[b]): Parser[b] =
        Parser((r,s) -> 
            let Parser(p1) = p;
            match p1(r,s) {
                case Ok(a) => { 
                    let Parser(p2) = k(a);
                    p2(r,s)
                }
                case Err(e) => Err(e)
            }
        )
 
    pub def flatMap(k: a -> Parser[b], p: Parser[a]): Parser[b] = bind(p, k)
        
    ///
    /// Throw a parse error. 
    /// 
    /// Note errors are collected in a stack, `throwError` discards any current error stack.
    /// Users should prefer `nestError` for exploratory development.
    /// 
    pub def parseError(msg: String): Parser[ans] = 
        use TextParser/Internal/TextCursor.{getPos, getInput};
        use TextParser/Error.{makeParseError};
        Parser((r,s) -> 
            let tabsize = r.tabsize;
            let pos = s.getPos();
            let inp = s.getInput();
            Err(makeParseError(inp, tabsize, pos, msg))
        )

    pub def swapError(msg: String, p: Parser[ans]): Parser[ans] = 
        use TextParser/Error.{swapErrorMessage};
        Parser((r,s) -> 
            let Parser(p1) = p;
            match p1(r,s) { 
                case Ok(a) => Ok(a)
                case Err(e) => Err(swapErrorMessage(msg, e))
            }
        )

    pub def nestError(msg: String, p: Parser[ans]): Parser[ans] = 
        use TextParser/Error.{nestErrorMessage};
        Parser((r,s) -> 
            let Parser(p1) = p;
            match p1(r,s) { 
                case Ok(a) => Ok(a)
                case Err(e) => Err(nestErrorMessage(msg, e))
            }
        )

    /// 
    /// If parser `p` fails try parser `q`.
    ///
    pub def alt(p: Parser[ans], q: Parser[ans]): Parser[ans] =
        use TextParser/Internal/TextCursor.{setPos, getPos, getInput};
        use TextParser/Error.{makeParseError};
        Parser((r,s) -> 
            let start = s.getPos();
            let Parser(p1) = p;
            match p1(r,s) { 
                case Ok(a) => Ok(a)
                case Err(_) => { 
                    s.setPos(start);
                    let Parser(q1) = q;
                    match q1(r,s) {
                        case Ok(b) => Ok(b)
                        case Err(_) => {
                            /// TODO need a strategy to show alternatives...
                            let tabsize = r.tabsize;
                            let pos = s.getPos();
                            let inp = s.getInput();
                            Err(makeParseError(inp, tabsize, pos, "alt"))
                        }
                    }
                }
            }
        )


    ///
    /// lookahead - run the parser but don't consume input on success
    /// On fail, fail.
    /// 
    pub def lookahead(p: Parser[a]): Parser[a] =
        use TextParser/Internal/TextCursor.{setPos, getPos};
        use TextParser/Error.{nestErrorMessage};
        Parser((r,s) -> {
            let Parser(p1) = p;
            let start = s.getPos();            
            match p1(r,s) { 
                case Err(e) => Err(nestErrorMessage("lookahead", e))
                case Ok(a) => { 
                    s.setPos(start); 
                    Ok(a) 
                }
            }
        })

    pub def option(one: ans, p: Parser[a]): Parser[a] = 
        use TextParser/Internal/TextCursor.{setPos, getPos};
        use TextParser/Error.{nestErrorMessage};
        Parser((r,s) -> {
            let Parser(p1) = p;
            let start = s.getPos();            
            match p1(r,s) { 
                case Err(_) => { 
                    s.setPos(start); 
                    Ok(one)
                } 
                case Ok(a) => Ok(a)
            }
        })
        
    pub def optional(p: Parser[a]): Parser[Option[a]] = 
        use TextParser/Internal/TextCursor.{setPos, getPos};
        use TextParser/Error.{nestErrorMessage};
        Parser((r,s) -> {
            let Parser(p1) = p;
            let start = s.getPos();            
            match p1(r,s) { 
                case Err(_) => { 
                    s.setPos(start); 
                    Ok(None)
                } 
                case Ok(a) => Ok(Some(a))
            }
        })
    
    /// Resets start position before failue continuation is run...
    pub def bindEither(p: Parser[a], fk: ParseError -> Parser[b], sk: a -> Parser[b]): Parser[b] = 
        use TextParser/Internal/TextCursor.{setPos, getPos};
        Parser((r,s) -> {
            let Parser(p1) = p;
            let start = s.getPos();            
            match p1(r,s) { 
                case Err(e) => { 
                    s.setPos(start); 
                    let Parser(fk1) = fk(e);
                    fk1(r,s)
                } 
                case Ok(a) => {
                    let Parser(sk1) = sk(a);
                    sk1(r,s)
                }
            }
        })


    pub def liftPrim(f: TextCursor -> a & Impure): Parser[a] = 
        Parser((_,s) -> 
            let a = f(s);
            Ok(a)
        )

   pub def liftPrimResult(f: TextCursor -> Result[a, PrimError] & Impure): Parser[a] =       
        use TextParser/Internal/TextCursor.{setPos, getPos, getInput};
        use TextParser/Error.{makeParseError, showPrimError};
        Parser((r,s) -> 
            match f(s) {
                case Ok(a) => Ok(a)
                case Err(e) => {
                    let tabsize = r.tabsize;
                    let pos = s.getPos();
                    let inp = s.getInput();
                    Err(makeParseError(inp, tabsize, pos, showPrimError(e)))
                }
            }
        )


}