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

use System.Error.{IOException, Generic};

namespace TextParser {

    type alias ErrMsg = { msg: String, pos: Int32 }

    type alias NextRes[t] = Int32 -> Result[t, ErrMsg]

    type alias Parser1[t] = String -> Int32 -> Result[t, ErrMsg] 

    type alias Success[r, t] = r -> NextRes[t] -> Parser1[t]

    pub opaque type TextParser[r, t] = Success[r, t] -> NextRes[t] -> Parser1[t]

    type alias Position = { lineNumber: Int32, column: Int32 }

    type alias ParseError = 
        { message: String
        , position: Position
        , textBefore: String
        , textAfter: String
        }

    pub def showPosition(pos: Position): String = 
        "Line=" + Int32.toString(pos.lineNumber) + ", Col=" + Int32.toString(pos.column)

    pub def samePosition(pos1: Position, pos2: Position): Bool =
        pos1.lineNumber == pos2.lineNumber && pos1.column == pos2.column

    pub def showParseError(err: ParseError): String = 
        String.unlines(err.message :: showPosition(err.position) 
                            :: "Seen:" :: "-----" :: err.textBefore 
                            :: "Pending:" :: "--------" :: err.textAfter :: Nil)


    ///
    /// Alias for `parseString`.
    ///
    pub def run(p: TextParser[t, t], input: String) : Result[t, ParseError] = 
        use TextParser/Internal/Error.{textPos, getBeforeAndAfter};
        let tabsize = 4;    /// TODO make configurable
        let fk = pos -> Err({ msg = "Parse error!", pos = pos });
        let sk = (a, _, _, _) -> Ok(a);
        let TextParser(p1) = p;
        match p1(sk, fk, input, 0) {
            case Ok(a) => Ok(a)
            case Err(e1) => {
                let (l,c) = textPos(tabsize, e1.pos, input);
                let pos = { lineNumber = l, column = c };
                let (before, after) = getBeforeAndAfter(e1.pos, input);
                Err({ message = e1.msg, position = pos, textBefore = before, textAfter = after })
            }
        }



    /// TODO - convert pos into line and column...
    /// 
    pub def runs(p: TextParser[t, t], input: String) : Result[t, String] = 
        match run(p, input) { 
            case Ok(a) => Ok(a)
            case Err(e) => Err(showPosition(e.position) + ": " + e.message)
        }

    pub def parseFile(parser: TextParser[t, t], path: System.FilePath, cs: Text.Charset): Result[t, System.Error] & Impure =
        match System/File.readFile(path, cs) {
            case Err(e) => Err(e)
            case Ok(text) => match run(parser, text) {
                case Ok(a) => Ok(a)
                case Err(e1) => Err(Generic(showParseError(e1)))
            }
        }


    pub def return(x: a): TextParser[a, t] = 
        TextParser((succ, next, inp, pos) -> succ(x, next, inp, pos))

    pub def fail(): TextParser[r, t] = 
        TextParser((_, next, _, pos) -> next(pos))

    pub def failure(msg: String) : TextParser[r, t] =
        TextParser((_, _, _, pos) -> Err({ msg = msg, pos = pos}))

    pub def swapError(msg: String, p: TextParser[r,t]) : TextParser[r, t] =
        TextParser((sc, _, inp, pos) -> apply1(p, sc, _ -> Err({ msg = msg, pos = pos}), inp, pos))

    pub def <!!>(p: TextParser[r,t], msg: String) : TextParser[r, t] = swapError(msg, p)

    def apply1 (p: TextParser[r, t], sk: Success[r, t], fk: NextRes[t], inp: String, pos: Int32): Result[t, ErrMsg] = 
        let TextParser(pf) = p;
        pf(sk, fk, inp, pos)

    pub def map(f: a -> b, p: TextParser[a, t]): TextParser[b, t] =
        bind(p, x -> return(f(x)))

    pub def ap(mf: TextParser[a -> b, t], ma: TextParser[a, t]): TextParser[b, t] = 
        let* f = mf;
        let* a = ma;
        return(f(a))

    pub def bind(ma: TextParser[q, t], k: q -> TextParser[r, t]):  TextParser[r, t] =
        TextParser(succ -> apply1(ma, x -> apply1(k(x), succ)))

    pub def flatMap(k: q -> TextParser[r, t], ma: TextParser[q, t]):  TextParser[r, t] = bind(ma, k)

    // Use Clean optimization or not?
    pub def alt(p: TextParser[a, t], q: TextParser[a, t]): TextParser[a, t] = 
        TextParser((sc, nc, inp, pos) -> 
            apply1(p, (r,_) -> sc(r, nc), apply1(q, sc, nc, inp), inp, pos))


    pub def ifThenElse(cond: TextParser[r, t], thenp: r -> TextParser[s,t], elsep: TextParser[s, t]): TextParser[s, t] = 
        TextParser((sc, nc, inp, pos) -> 
            apply1(cond, x -> apply1(thenp(x),sc), apply1(elsep, sc, nc, inp), inp, pos))

    
    pub def liftOption(x: Option[r]): TextParser[r, t] =  match x {
        case Some(a) => return(a)
        case None    => fail()
    }

    pub def liftResult(x: Result[r, e]): TextParser[r, t] =  match x {
        case Ok(a) => return(a)
        case Err(_) => fail()
    }

    pub def endOfInput() : TextParser[Unit, t] =
        TextParser((sc, nc, inp, pos) ->
            let len = String.length(inp);
            if (pos >= len)
                sc((), nc, inp, pos)
            else
                nc(pos)
        )

    pub def string(s: String): TextParser[String, t] = 
        TextParser((sc, nc, inp, pos) ->
            let strlen = String.length(s);
            let s1 = String.slice(inp, pos, pos + strlen);
            if (s1 == s)
                sc(s, nc, inp, pos + strlen)
            else
                nc(pos)
        )

    pub def char(ch: Char): TextParser[Char, t] = 
        TextParser((sc, nc, inp, pos) ->
            let len = String.length(inp);            
            if (pos < len) {
                let c = String.charAt(pos, inp);
                if (c == ch)
                    sc(c, nc, inp, pos + 1)
                else 
                    nc(pos)
            }
            else
                nc(pos)
        )

    pub def anyChar(): TextParser[Char, t] = 
        TextParser((sc, nc, inp, pos) ->
            let len = String.length(inp);            
            if (pos < len) {
                let c = String.charAt(pos, inp);
                sc(c, nc, inp, pos + 1)
            }
            else
                nc(pos)
        )

    pub def lookingAt(patt: Text/Regex.Pattern): TextParser[String, t] = 
        use Text/Regex.{boundedStartsWith};
        TextParser((sc, nc, inp, pos) ->
            let ans = boundedStartsWith(inp, patt, pos, String.length(inp)) as & Pure;
            match ans { 
                case Ok(s) => sc(s, nc, inp, pos + String.length(s))
                case Err(_) => nc(pos)
            }
        )


    pub def skippingAt(patt: Text/Regex.Pattern): TextParser[Unit, t] = 
        use Text/Regex.{boundedStartsWith};
        TextParser((sc, nc, inp, pos) ->
            let ans = boundedStartsWith(inp, patt, pos, String.length(inp)) as & Pure;
            match ans { 
                case Ok(s) => sc((), nc, inp, pos + String.length(s))
                case Err(_) => nc(pos)
            }
        )

    /// Parsing fails if regex does not compile.
    pub def lookingAts(regex: String): TextParser[String, t] = 
        use Text/Regex/Pattern.{compile};
        bind(liftResult(compile(regex)), lookingAt)
        
    /// Parsing fails if regex does not compile.
    pub def skippingAts(regex: String): TextParser[Unit, t] = 
        use Text/Regex/Pattern.{compile};
        bind(liftResult(compile(regex)), skippingAt)


    pub def horizon(n: Int32): TextParser[String, t] = 
        TextParser((sc, nc, inp, pos) ->
            if (n > 0) {
                let s = String.slice(inp, pos, pos + n);
                sc(s, nc, inp, pos + String.length(s))
            } else
                sc("", nc, inp, pos)
        )


    ///
    /// lookahead - run the parser but don't consume input on success
    /// On fail, fail.
    ///
    pub def lookahead(p: TextParser[r, t]): TextParser[r, t] =
        TextParser((sc, nc, inp, pos) ->
            let sc1 = (ans, nc1, inp1, _) -> sc(ans, nc1, inp1, pos);
            apply1(p, sc1, nc, inp, pos)
        )

    ///
    /// Run parser `p` within a bounded length
    ///
    pub def bounded(n: Int32, p: TextParser[r, t]): TextParser[r, t] =
        TextParser((sc, nc, inp, pos) ->
            /// Applies parser `p` on a slice of the input, then "restores" the full input 
            /// for the success continuation.
            let str = String.slice(inp, pos, pos + n);        
            let sc1 = (ans, nc1, _, _) -> sc(ans, nc1, inp, pos + String.length(str));
            apply1(p, sc1, nc, str, 0)
        )




}
