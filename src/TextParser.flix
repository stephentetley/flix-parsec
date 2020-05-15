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

namespace TextParser {
    
    type alias Index = Int32

    type alias ErrMsg = String

    type alias SrcPos = (Int32, Int32)

    pub opaque type ParseError = 
        { messages: List[String]
        , linenum: Int32
        , column: Int32 
        , justseen: String
        , pending: String 
        }

    def makeParseError(input: Input, pos: Int32, msg: String): ParseError = 
        let pair = TextParser/Internal.textPos(pos, input.tabsize, input.source) |> x -> Option.getWithDefault(x, (1,1));
        let lin = fst(pair);
        let col = snd(pair);
        let seen = TextParser/Internal.justSeen(pos, input.source);
        let todo = TextParser/Internal.pendingInput(pos, input.source);
        let ans = { messages = msg :: Nil, linenum = lin, column = col, justseen = seen, pending = todo };
        ParseError(ans)

    def nestErrorMessage(outer: String, e: ParseError): ParseError = 
        let ParseError(err) = e;
        let msgs = outer :: err.messages;
        ParseError({ messages = msgs | err })

    def swapErrorMessage(msg: String, e: ParseError): ParseError = 
        let ParseError(err) = e;
        let stack = match err.messages {
            case _ :: rs => msg :: rs
            case Nil => msg :: Nil
        };
        ParseError({ messages = stack | err })

    pub def showParserError(e: ParseError): String = 
        let ParseError(err) = e;
        let h1 = "Line: " + Int32.toString(err.linenum) + ", column: " + Int32.toString(err.column);
        let seen = err.justseen + "***";
        let todo = "***" + err.pending;
        let msg = errorMessage(err.messages);
        String.unlines(h1 :: msg :: seen :: todo :: Nil)

    def errorMessage(stk: List[String]): String = match stk {
            case Nil => "*error*"
            case x :: rs => x + errorMessageHelper(rs)
        }

    def errorMessageHelper(rest: List[String]): String = 
        match rest { 
            case Nil => ""
            case x :: Nil => "[" + x + "]"
            case x :: rs => "[" + x + errorMessageHelper(rs) + "]"
        }

    /// Defining our own `Result` type gives us shorter type signatures
    pub enum ParseResult[a] {
        case PErr(ParseError),
        case POk(a, Index)
    }

    ///
    /// The parser type passes around the input string and it's **end** (not length)
    ///
    type alias Input = { source: String, end: Int32, tabsize: Int32 }

    def makeInput(input: String): Input = 
        { source = input, end = String.length(input) - 1, tabsize = 4 }

        
    /// 
    /// This is exception safe due to bounds checking.
    ///
    pub def charAt(i: Int32, input: Input) : Option[Char] = 
        let str = input.source;
        let end = input.end;
        if (i < 0 || i > end) 
            None
        else
            Some(String.charAt(i, str))

    pub def trySlice(b: Int32, e: Int32, input: Input) : Option[String] & Pure = 
        let str = input.source;
        let end = input.end;
        if (e <= b || b < 0 || b > end || e < 0 || e > end)
            None
        else
            Some(String.slice(str, b, e))



    /// We could have an env with tabsize and a function trailing: Char -> Bool
    /// This would allow nice configuration...

    pub enum Parser[ans] {
        case Parser((Input, Index) -> ParseResult[ans])
    }

    
    pub def fail(msg: String) : Parser[ans] = Parser((inp, pos) -> PErr(makeParseError(inp, pos, msg)))

    def failure(e: ParseError) : Parser[ans] = Parser((_, _) -> PErr(e))

    pub def return(x: ans): Parser[ans] = Parser((_, pos) -> POk(x, pos))

    ///
    /// Ideally this wouldn't be exported
    ///
    def apply1(parser: Parser[ans], inp: Input, pos: Index): ParseResult[ans] = 
        let Parser(f) = parser;
        f(inp, pos)
        

    pub def bind(ma: Parser[a], k: a -> Parser[b]) : Parser[b] = 
        Parser((inp, pos) -> match apply1(ma, inp, pos) { 
                case PErr(e) => PErr(e)
                case POk(ans, pos1) => apply1(k(ans), inp, pos1)
            })


    pub def bindFS(ma: Parser[a], fk: ParseError -> Parser[b], sk: a -> Parser[b]) : Parser[b] = 
        Parser((inp, pos) -> match apply1(ma, inp, pos) { 
                case PErr(e) => apply1(fk(e), inp, pos)
                case POk(ans, pos1) => apply1(sk(ans), inp, pos1)
            })



    pub def flatMap(k: a -> Parser[b], ma: Parser[a]) : Parser[b] = bind(ma, k)

    pub def map(f: a -> b, ma: Parser[a]) : Parser[b] = 
        bindFS(ma, e -> failure(e), a -> return(f(a)))
    

    pub opaque type Scanner = (String, Int32) -> Result[String, String]
    
    pub opaque type Skipper = (String, Int32) -> Result[Int32, String]


    pub def applyScanner(scan: Scanner): Parser[String] = 
        Parser((inp, pos) -> 
            let Scanner(scanner) = scan;
            match scanner(inp.source, pos) {
                case Err(msg) => PErr(makeParseError(inp, pos, msg))
                case Ok(a) => POk(a, pos + String.length(a))
            }            
        )

    pub def applySkipper(skip: Skipper): Parser[Unit] = 
        Parser((inp, pos) -> 
            let Skipper(skipper) = skip;
            match skipper(inp.source, pos) {
                case Err(msg) => PErr(makeParseError(inp, pos, msg))
                case Ok(pos1) => POk((), pos1)
            }            
        )
    ///
    /// Alias for `parseString`.
    ///
    pub def run(parser: Parser[a], input: String): Result[a, ParseError] = parseString(parser, input)
    
    pub def parseString(parser: Parser[a], input: String): Result[a, ParseError] = 
        let Parser(f) = parser;
        match f(makeInput(input), 0) {
            case PErr(e) => Err(e)
            case POk((a, _)) => Ok(a)
        }

    ///
    /// Simplified version of `run`. On failure the ParseError is converted to a string.
    ///    
    pub def runs(parser: Parser[a], input: String): Result[a, String] = 
        match parseString(parser, input) {
            case Err(e) => Err(showParserError(e))
            case Ok(a) => Ok(a) 
        }

    pub def parseFile(parser: Parser[a], path: System.FilePath, cs: System.Charset): Result[a, ParseError] & Impure = 
        match System/File.readFile(path, cs) {
            case Err(_) => Err(makeParseError(makeInput(""), 0, "io error"))
            case Ok(text) => run(parser, text)
        }

    
        
    pub def position(): Parser[Int32] =
        Parser((_, pos) -> POk(pos, pos))


    
    ///
    /// Throw a parse error. 
    /// 
    /// Note errors are collected in a stack, `throwError` discards any current error stack.
    /// Users should prefer `nestError` for exploratory development.
    /// 
    pub def throwError(msg: String): Parser[ans] = 
        Parser((inp, pos) -> PErr(makeParseError(inp, pos, msg)))

    pub def swapError(msg: String, p: Parser[ans]): Parser[ans] = 
        bindFS(p, e -> failure(swapErrorMessage(msg,e)), return)
    
    pub def nestError(msg: String, p: Parser[ans]): Parser[ans] = 
        bindFS(p, e -> failure(nestErrorMessage(msg, e)), return)
        

    pub def liftResult(x: Result[a, String]): Parser[a] = match x {
        case Err(s) => throwError(s)
        case Ok(a) => return(a)
    }
    
    /// 
    /// If parser `p` fails try parser `q`.
    ///
    pub def alt(p: Parser[ans], q: Parser[ans]): Parser[ans] =
        Parser((inp, pos) -> match apply1(p, inp, pos) {
            case PErr(e1) => match apply1(q, inp, pos) {
                case PErr(e2) => {
                    let ParseError(err1) = e1;
                    let ParseError(err2) = e2;
                    let msg = "alt[" + errorMessage(err1.messages) + ", " + errorMessage(err2.messages) + "]";
                    PErr(makeParseError(inp, pos, msg))
                }
                case POk(ans, pos2) => POk(ans, pos2)
            }
            case POk(ans, pos1) => POk(ans, pos1)
        })

    pub def ap(pf: Parser[a -> b], pa: Parser[a]): Parser[b] =
        bind(pf, f -> bind(pa, a -> return(f(a))))


    ///
    /// Avoid using `satisfy` if possible (efficiency).
    ///
    pub def satisfy(test: Char -> Bool): Parser[Char] = 
        Parser((inp, pos) -> match charAt(pos, inp) {
            case None => PErr(makeParseError(inp, pos, "satisfy (index out of bounds)"))
            case Some(ch) => 
                if (test(ch)) 
                    POk(ch, pos+1) 
                else 
                    PErr(makeParseError(inp, pos, "satisfy"))
        }) 


    ///
    /// lookahead - run the parser but don't consume input on success
    /// On fail, fail.
    /// 
    pub def lookahead(p: Parser[a]): Parser[a] = 
        Parser((inp, pos) -> match apply1(p, inp, pos) {
            case PErr(e) => PErr(nestErrorMessage("lookahead", e))
            case POk(a,_) => POk(a, pos)
        })

    pub def string(needle: String): Parser[String] = 
        let msg = "string (expecting '" + needle + "')";
        Parser((inp, pos) -> match trySlice(pos, pos + String.length(needle), inp) {
                case None => PErr(makeParseError(inp, pos, msg))
                case Some(s) => 
                    if (s == needle) 
                        POk(needle, pos + String.length(needle)) 
                    else 
                        PErr(makeParseError(inp, pos, msg))
            })

    /// 
    /// Returns a string of the next `n` characters from the current position.
    ///
    pub def horizon(n: Int32): Parser[String] = 
        let msg = "horizon (n = " + Int32.toString(n) + ")";
        Parser((inp, pos) -> match trySlice(pos, pos+n, inp) {
                case None => PErr(makeParseError(inp, pos, msg))
                case Some(s) => POk(s, pos+n)
            })

    /// 
    /// Returns the text "skipped" by a parser if it succeeds.
    ///
    pub def skipped(p: Parser[a]): Parser[String] = 
        Parser((inp,pos) -> match apply1(p, inp, pos) {
            case PErr(e) => PErr(nestErrorMessage("skipped", e))
            case POk(_, pos1) => match trySlice(pos, pos1, inp) {
                case None => PErr(makeParseError(inp, pos, "skipped"))
                case Some(s) => POk(s, pos1)
            }
        })

    /// 
    /// `endOfInput` succeeds if all the input has been parsed.
    ///
    pub def endOfInput() : Parser[Unit] = 
        Parser((inp, pos) -> 
            if (pos > inp.end) 
                POk((), pos)
            else 
                PErr(makeParseError(inp, pos, "endOfInput"))
        )
    
    pub def restOfInput(): Parser[String] = 
        Parser((inp, pos) -> 
            let end = inp.end;
            match trySlice(pos, end, inp) {
                case None => PErr(makeParseError(inp, pos, "restOfInput"))
                case Some(s) => POk(s, end)
            })


    /// 
    /// Run parser `p` within a bounded text 
    ///
    pub def bounded(p: Parser[a], len: Int32): Parser[a] = 
        let msg = "bounded (length = " + Int32.toString(len) + ")";
        Parser((inp, pos) -> match trySlice(0, pos + len, inp) {
            case None => PErr(makeParseError(inp, pos, msg))
            case Some(s) => 
                let inp1 = makeInput(s);
                match apply1(p, inp1, pos) {
                    case PErr(e) => {
                        let ParseError(err1) = e;
                        let msg1 = errorMessage(msg :: err1.messages); 
                        PErr(makeParseError(inp, pos, msg1))
                    }
                    case POk(a, _) => POk(a, pos + len)
            }
        })

}