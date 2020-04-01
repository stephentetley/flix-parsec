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
        { message: String
        , linenum: Int32
        , column: Int32 
        , justseen: String
        , pending: String 
        }

    def makeParseError(src: String, pos: Int32, msg: String): ParseError = 
        let pair = TextParser/Internal.textPos(pos, 3, src) |> x -> Option.getWithDefault(x, (1,1));
        let lin = fst(pair);
        let col = snd(pair);
        let seen = TextParser/Internal.justSeen(pos, src);
        let todo = TextParser/Internal.pendingInput(pos, src);
        let ans = { message = msg, linenum = lin, column = col, justseen = seen, pending = todo };
        ParseError(ans)

    def nestErrorMessage(outer: String, e: ParseError): ParseError = 
        let ParseError(err) = e;
        let msg = outer + "[" + err.message + "]";
        ParseError({ message = msg | err })

    def swapErrorMessage(msg: String, e: ParseError): ParseError = 
        let ParseError(err) = e;
        ParseError({ message = msg | err })

    pub def showParserError(e: ParseError): String = 
        let ParseError(err) = e;
        let h1 = "Line: " + Int32.toString(err.linenum) + ", column: " + Int32.toString(err.column);
        let seen = err.justseen + "***";
        let todo = "***" + err.pending;
        String.unlines([h1, err.message, seen, todo]) as & Pure


    /// We could have an env with tabsize and a function trailing: Char -> Bool
    /// This would allow nice configuration...

    pub enum Parser[ans] {
        case Parser((String, Index) -> Result[(ans, Index), ParseError])
    }

    pub def fail() : Parser[ans] = Parser((inp, pos) -> Err(makeParseError(inp, pos, "fail")))

    pub def return(x: ans): Parser[ans] = Parser((_, pos) -> Ok(x, pos))

    ///
    /// Ideally this wouldn't be exported
    ///
    pub def apply1(parser: Parser[ans], inp: String, pos: Index): Result[(ans, Index), ParseError] = 
        let Parser(f) = parser;
        f(inp, pos)
        

    pub def bind(ma: Parser[a], k: a -> Parser[b]) : Parser[b] = 
        Parser((inp, pos) -> match apply1(ma, inp, pos) { 
                case Err(e) => Err(e)
                case Ok(ans, pos1) => apply1(k(ans), inp, pos1)
            })

    pub def flatMap(k: a -> Parser[b], ma: Parser[a]) : Parser[b] = bind(ma, k)

    pub def map(f: a -> b, ma: Parser[a]) : Parser[b] = 
        Parser((inp, pos) -> match apply1(ma, inp, pos) { 
                case Err(e) => Err(e)
                case Ok(ans, pos1) => Ok(f(ans), pos1)
            })

    
    pub def parseString(parser: Parser[a], input: String): Result[a, ParseError] = 
        let Parser(f) = parser;
        match f(input, 0) {
            case Err(e) => Err(e)
            case Ok((a, _)) => Ok(a)
        }

    ///
    /// Alias for `parseString`.
    ///
    pub def run(parser: Parser[a], input: String): Result[a, ParseError] = parseString(parser, input)
    
    pub def runs(parser: Parser[a], input: String): Result[a, String] = 
        match parseString(parser, input) {
            case Err(e) => Err(showParserError(e))
            case Ok(a) => Ok(a) 
        }

    pub def parseFile(parser: Parser[a], path: FilePath, cs: Charset): Result[a, ParseError] & Impure = 
        match File.readFile(path, cs) {
            case Err(_) => Err(makeParseError("", 0, "io error"))
            case Ok(text) => run(parser, text)
        }

    ///
    /// Parse each line of the input file with `parser`, collect the results in a list.
    ///
    /// All lines must parse, otherwise the parse fails.
    ///
    pub def parseLines(parser: Parser[a], path: Path): Result[List[a], ParseError] & Impure = 
        let failk = (n,e) -> Err(makeParseError("", n, "Failed at line: " + Int32.toString(n) + "; " + showParserError(e)));
        match Path.exists(path) {
            case Ok(true) => match Path.readLines(path) {
                case Ok(xs) => parseLinesHelper(parser, 1, xs, failk, vs -> Ok(vs))
                case Err(_) => Err(makeParseError("", 0, "io error"))
            }
            case Ok(false) => Err(makeParseError("", 0, "Missing file"))
            case Err(_) => Err(makeParseError("", 0, "exception"))
        }   

    ///
    /// Helper for `parseLines`
    ///
    def parseLinesHelper(parser: Parser[a], n: Int32, src: List[String], fk: (Int32, ParseError) -> Result[List[a], ParseError], sk: List[a] -> Result[List[a], ParseError]): Result[List[a], ParseError] = 
        match src {
            case Nil => sk(Nil)
            case x :: rs => match parseString(parser, x) {
                case Err(e) => fk(n, e)
                case Ok(a) => parseLinesHelper(parser, n+1, rs, fk, vs -> sk(a :: vs))
            }
        }

    ///
    /// Parse each line of the input file with `parser`, if parse produces `Some(a)` collect `a` in 
    /// the result list. If the parser produces `None`, proceed anyway.
    ///
    /// All lines must parse successfully, otherwise the parse fails, but parsing a line does not 
    /// have to generate a result. This parser can be used to skip lines.
    ///
    pub def chooseParseLines(parser: Parser[Option[a]], path: Path): Result[List[a], ParseError] & Impure = 
        let failk = (n,e) -> Err(makeParseError("", n, "Failed at line: " + Int32.toString(n) + "; " + showParserError(e)));
        match Path.exists(path) {
            case Ok(true) => match Path.readLines(path) {
                case Ok(xs) => chooseParseLinesHelper(parser, 1, xs, failk, vs -> Ok(vs))
                case Err(_) => Err(makeParseError("", 0, "readLines failed"))
            }
            case Ok(false) => Err(makeParseError("", 0, "Missing file"))
            case Err(_) => Err(makeParseError("", 0, "Exception"))
        }   

    ///
    /// Helper for `chooseParseLines`
    ///
    def chooseParseLinesHelper(parser: Parser[Option[a]], n: Int32, src: List[String], fk:(Int32, ParseError) -> Result[List[a], ParseError], sk: List[a] -> Result[List[a], ParseError]): Result[List[a], ParseError] & Impure = 
        match src {
            case Nil => sk(Nil)
            case x :: rs => match parseString(parser, x) {
                case Err(e) => fk(n, e)
                case Ok(None) => chooseParseLinesHelper(parser, n+1, rs, fk, vs -> sk(vs))
                case Ok(Some(a)) => chooseParseLinesHelper(parser, n+1, rs, fk, vs -> sk(a :: vs))
            }
        }
        
    pub def position(): Parser[Int32] =
        Parser((_, pos) -> Ok(pos, pos))

    
    pub def liftM2(f: (a, b) -> ans, p1: Parser[a], p2: Parser[b]): Parser[ans] = 
        bind(p1, v1 -> 
            bind(p2, v2 -> return(f(v1,v2))))

    pub def liftM3(f: (a, b, c) -> ans, p1: Parser[a], p2: Parser[b], p3: Parser[c]): Parser[ans] = 
        bind(p1, v1 -> 
            liftM2((v2,v3) -> f(v1,v2,v3), p2, p3))


    pub def liftM4(f: (a, b, c, d) -> ans, p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d]): Parser[ans] = 
        bind(p1, v1 -> 
            liftM3((v2,v3,v4) -> f(v1,v2,v3,v4), p2, p3, p4))
                    

    pub def liftM5(f: (a, b, c, d, e) -> ans, p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e]): Parser[ans] = 
        bind(p1, v1 -> 
            liftM4((v2,v3,v4,v5) -> f(v1,v2,v3,v4,v5), p2, p3, p4, p5))

    pub def liftM6(f: (a, b, c, d, e, f) -> ans, p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f]): Parser[ans] = 
        bind(p1, v1 -> 
            liftM5((v2,v3,v4,v5,v6) -> f(v1,v2,v3,v4,v5,v6), p2, p3, p4, p5, p6))

    pub def liftM7(f: (a, b, c, d, e, f, g) -> ans, p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f], p7: Parser[g]): Parser[ans] = 
        bind(p1, v1 -> 
            liftM6((v2,v3,v4,v5,v6,v7) -> f(v1,v2,v3,v4,v5,v6,v7), p2, p3, p4, p5, p6,p7))

    pub def liftM8(f: (a, b, c, d, e, f, g, h) -> ans, p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f], p7: Parser[g], p8: Parser[h]): Parser[ans] = 
        bind(p1, v1 -> 
            liftM7((v2,v3,v4,v5,v6,v7,v8) -> f(v1,v2,v3,v4,v5,v6,v7,v8), p2, p3, p4, p5, p6,p7,p8))

    pub def liftM9(f: (a, b, c, d, e, f, g, h, i) -> ans, p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f], p7: Parser[g], p8: Parser[h], p9: Parser[i]): Parser[ans] = 
        bind(p1, v1 -> 
            liftM8((v2,v3,v4,v5,v6,v7,v8,v9) -> f(v1,v2,v3,v4,v5,v6,v7,v8,v9), p2, p3, p4, p5, p6,p7,p8,p9))


    pub def bounded(p: Parser[a], length: Int32): Parser[a] = 
        Parser((inp, pos) -> match TextParser/Internal.trySlice(0, pos+length, inp) {
            case None => Err(makeParseError(inp, pos, "bounded"))
            case Some(s) => match apply1(p, s, pos) {
                case Err(msg) => Err(msg)
                case Ok(a, _) => Ok(a, pos+length)
            }
        })

    pub def throwError(msg: String): Parser[ans] = 
        Parser((inp, pos) -> Err(makeParseError(inp, pos, msg)))

    pub def swapError(msg: String, p: Parser[ans]): Parser[ans] = 
        Parser((inp, pos) -> match apply1(p, inp, pos) { 
                case Err(e) => Err(swapErrorMessage(msg,e))
                case Ok(ans, pos1) => Ok(ans, pos1)
            })
    
    pub def nestError(msg: String, p: Parser[ans]): Parser[ans] = 
        Parser((inp, pos) -> match apply1(p, inp, pos) { 
                case Err(e) => Err(nestErrorMessage(msg, e))
                case Ok(ans, pos1) => Ok(ans, pos1)
            })



    ///
    /// lookahead - run the parser but don't consume input on success
    /// On fail, fail.
    /// 
    pub def lookahead(p: Parser[a]): Parser[a] = 
        Parser((inp, pos) -> match apply1(p, inp, pos) {
            case Err(e) => Err(nestErrorMessage("lookahead", e))
            case Ok(a,_) => Ok(a, pos)
        })


    pub def alt(p: Parser[ans], q: Parser[ans]): Parser[ans] =
        Parser((inp, pos) -> match (apply1(p, inp, pos), apply1(q, inp, pos)) {
            case (Err(e1), Err(e2)) => { 
                let ParseError(err1) = e1;
                let ParseError(err2) = e2;
                let msg = "alt[" + err1.message + ", " + err2.message + "]";
                Err(makeParseError(inp, pos, msg))
            }
            case (Err(_), b) => b
            case (a, Err(_)) => a
            case (Ok(a, pos1a), Ok(b, pos1b)) => 
                if (pos1b > pos1a) 
                    Ok(b, pos1b)
                else 
                    Ok(a, pos1a)
        })

    pub def ap(pf: Parser[a -> b], pa: Parser[a]): Parser[b] =
        bind(pf, f -> bind(pa, a -> return(f(a))))


    pub def satisfy(test: Char -> Bool): Parser[Char] = 
        Parser((inp, pos) -> match TextParser/Internal.charAtSafe(pos, inp) {
                case None => Err(makeParseError(inp, pos, "satisfy (no input)"))
                case Some(ch) => 
                    if (test(ch)) Ok(ch, pos+1) else Err(makeParseError(inp, pos, "satisfy"))
            }) 

    pub def endOfInput() : Parser[Unit] = 
        Parser((inp, pos) -> 
            if (pos >= String.length(inp)) Ok((), pos) else Err(makeParseError(inp, pos, "endOfInput")))
    

    /// 
    /// Returns a string of the next `n` characters from the current position.
    ///
    pub def horizon(n: Int): Parser[String] = 
        Parser((inp, pos) -> match TextParser/Internal.trySlice(pos, pos+n, inp) {
                case None => Err(makeParseError(inp, pos, "horizon"))
                case Some(s) => Ok(s, pos+n)
            })

    /// 
    /// Returns the "content" (i.e the trimmed text) of the next `n` characters from 
    /// the current position.
    ///
    pub def content(n: Int): Parser[String] = 
        map(String.trim, horizon(n))

    

    pub def string(needle: String): Parser[String] = 
        let msg = "string - expecting '" + needle + "'";
        Parser((inp, pos) -> match TextParser/Internal.trySlice(pos, pos + String.length(needle), inp) {
                case None => Err(makeParseError(inp, pos, msg))
                case Some(s) => 
                    if (s == needle) Ok(needle, pos + String.length(needle)) else Err(makeParseError(inp, pos, msg))
            })




}