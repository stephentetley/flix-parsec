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
use Data/Transient.DList;

namespace TextParser {

    type alias Failures = DList[String]

    type alias FailK[t] = Int32 -> Failures -> t

    type alias SuccessK[r, t] = r -> FailK[t] -> String -> Int32 -> Failures -> t

    pub opaque type TextParser[r, t] = SuccessK[r, t] -> FailK[t] -> String -> Int32 -> Failures -> t

    type alias Position = { lineNumber: Int32, column: Int32 }

    type alias Error1 = { msgs: List[String], pos: Int32 }

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

    def showErrorsList(stk: List[String]): String = match stk {
        case Nil => "*error*"
        case x :: rs => x + errorMessageHelper(rs)
    }

    def errorMessageHelper(rest: List[String]): String = match rest {
        case Nil => ""
        case x :: Nil => "[" + x + "]"
        case x :: rs => "[" + x + errorMessageHelper(rs) + "]"
    }

    ///
    /// Alias for `parseString`.
    ///
    pub def run(p: TextParser[t, Result[t, Error1]], input: String) : Result[t, ParseError] =
        use Data/Transient/DList.{toList, empty};
        use TextParser/Internal/Error.{textPos, getBeforeAndAfter};
        let tabsize = 4;    // TODO make configurable
        let fk = (pos, errs) -> Err({ msgs = toList(errs), pos = pos });
        let sk = (a, _, _, _, _) -> Ok(a);
        let TextParser(p1) = p;
        match p1(sk, fk, input, 0, empty()) {
            case Ok(a) => Ok(a)
            case Err(e1) => {
                let (l,c) = textPos(tabsize, e1.pos, input);
                let posn = { lineNumber = l, column = c };
                let (before, after) = getBeforeAndAfter(e1.pos, input);
                Err({ message = showErrorsList(e1.msgs)
                    , position = posn
                    , textBefore = before
                    , textAfter = after })
            }
        }

    ///
    pub def runs(p: TextParser[t, Result[t, Error1]], input: String) : Result[t, String] =
        match run(p, input) {
            case Ok(a) => Ok(a)
            case Err(e) => Err(showPosition(e.position) + ": " + e.message)
        }

    pub def parseFile(parser: TextParser[t, Result[t, Error1]], path: System.FilePath, cs: Text.Charset): Result[t, System.Error] & Impure =
        match System/File.readFile(path, cs) {
            case Err(e) => Err(e)
            case Ok(text) => match run(parser, text) {
                case Ok(a) => Ok(a)
                case Err(e1) => Err(Generic(showParseError(e1)))
            }
        }


    pub def return(x: a): TextParser[a, t] = TextParser(succ -> succ(x))

    pub def failure(message: String) : TextParser[r, t] =
        use Data/Transient/DList.{singleton};
        TextParser((_, fk, _, pos, _) -> fk(pos, singleton(message)))

    ///
    /// `swapError` is quite subtle. Swapping the error continuate needs to be
    /// a "local" operation which is undone (restored) afterwards (c.f. local in the Reader
    /// monad). It must not be a "stateful" operation with unlimited extent.
    ///
    pub def swapError(p: TextParser[r,t], message: String) : TextParser[r, t] =
        use Data/Transient/DList.{singleton};
        TextParser((sk, fk, inp, pos, errs) ->
            let fk1 = (pos1, _) -> fk(pos1, singleton(message));
            let sk1 = (x, _, _, pos1, _) -> sk(x, fk, inp, pos1, errs);
            apply1(p, sk1, fk1, inp, pos, errs)
        )

    def apply1 (p: TextParser[r, t], sk: SuccessK[r, t], fk: FailK[t], inp: String, pos: Int32, errs: Failures): t =
        let TextParser(pf) = p;
        pf(sk, fk, inp, pos, errs)

    pub def map(f: a -> b, p: TextParser[a, t]): TextParser[b, t] =
        TextParser(sk ->
            apply1(p, x -> sk(f(x)))
        )

    pub def ap(mf: TextParser[a -> b, t], ma: TextParser[a, t]): TextParser[b, t] = 
        TextParser(sk ->
            apply1(mf, f -> apply1(ma, x -> sk(f(x))))
        )

    pub def bind(ma: TextParser[q, t], k: q -> TextParser[r, t]):  TextParser[r, t] =
        TextParser(sk ->
            apply1(ma, x -> apply1(k(x), sk))
        )

    pub def flatMap(k: q -> TextParser[r, t], ma: TextParser[q, t]):  TextParser[r, t] = bind(ma, k)

    pub def alt(p: TextParser[a, t], q: TextParser[a, t]): TextParser[a, t] =
        use Data/Transient/DList.{append};
        TextParser((sc, nc, inp, pos, errs) ->
            let pnc = (_, perrs) -> {
                let qnc = (_, qerrs) -> nc(pos, append(perrs, qerrs));
                apply1(q, sc, qnc, inp, pos, perrs)
            };
            apply1(p, sc, pnc, inp, pos, errs)
        )


    pub def seqLeft(p: TextParser[r, t], q: TextParser[s, t]): TextParser[r, t] =
        TextParser(sc ->
            apply1(p, x -> apply1(q, _ -> sc(x)))
        )

    pub def seqRight(p: TextParser[r, t], q: TextParser[s, t]): TextParser[s, t] =
        TextParser(sc ->
            apply1(p, _ -> apply1(q, y -> sc(y)))
        )

    pub def ifThenElse(cond: TextParser[r, t], thenp: r -> TextParser[s,t], elsep: TextParser[s, t]): TextParser[s, t] = 
        TextParser((sc, nc, inp, pos) -> 
            apply1(cond, x -> apply1(thenp(x),sc), apply1(elsep, sc, nc, inp), inp, pos))

    
    pub def liftOption(x: Option[r]): TextParser[r, t] =  match x {
        case Some(a) => return(a)
        case None    => failure("None")
    }

    pub def liftResult(x: Result[r, e]): TextParser[r, t] =  match x {
        case Ok(a) => return(a)
        case Err(_) => failure("Err")
    }

    pub def endOfInput() : TextParser[Unit, t] =
        use Data/Transient/DList.{singleton};
        TextParser((sk, fk, inp, pos, errs) ->
            let len = String.length(inp);
            if (pos >= len)
                sk((), fk, inp, pos, errs)
            else
                fk(pos, singleton("eof - not at end"))
        ) 

    pub def string(s: String): TextParser[String, t] =
        use Data/Transient/DList.{singleton};
        TextParser((sk, fk, inp, pos, errs) ->
            let strlen = String.length(s);
            let s1 = String.slice(inp, pos, pos + strlen);
            if (s1 == s)
                sk(s, fk, inp, pos + strlen, errs)
            else {
                let msg = "string - expecting '" + s + "', encountered '" + s1 + "'";
                fk(pos, singleton(msg))
            }
        )

    pub def char(ch: Char): TextParser[Char, t] = 
        use Data/Transient/DList.{singleton};
        TextParser((sk, fk, inp, pos, errs) ->
            let len = String.length(inp);            
            if (pos < len) {
                let c = String.charAt(pos, inp);
                if (c == ch)
                    sk(c, fk, inp, pos + 1, errs)
                else  {
                    let msg = "char - expecting '" + Char.toString(ch) + "', encountered '" + Char.toString(c) + "'";
                    fk(pos, singleton(msg))
                }
            } else {
                let msg = "char - unexpected eof";
                fk(pos, singleton(msg))
            }
        )

    pub def anyChar(): TextParser[Char, t] =
        use Data/Transient/DList.{singleton};
        TextParser((sk, fk, inp, pos, errs) ->
            let len = String.length(inp);            
            if (pos < len) {
                let c = String.charAt(pos, inp);
                sk(c, fk, inp, pos + 1, errs)
            } else {
                let msg = "anyChar - eof";
                fk(pos, singleton(msg))
            }
        )

    pub def lookingAt(patt: Text/Regex.Pattern): TextParser[String, t] =
        use Data/Transient/DList.{singleton};
        use Text/Regex.{boundedStartsWith};
        use Text/Regex/Pattern.{toString};
        TextParser((sk, fk, inp, pos, errs) ->
            let ans = boundedStartsWith(inp, patt, pos, String.length(inp)) as & Pure;
            match ans { 
                case Ok(s) => sk(s, fk, inp, pos + String.length(s), errs)
                case Err(_) => fk(pos, singleton("lookingAt failed"))
            }
        )


    pub def skippingAt(patt: Text/Regex.Pattern): TextParser[Unit, t] = 
        use Data/Transient/DList.{singleton};
        use Text/Regex.{boundedStartsWith};
        TextParser((sk, fk, inp, pos, errs) ->
            let ans = boundedStartsWith(inp, patt, pos, String.length(inp)) as & Pure;
            match ans { 
                case Ok(s) => sk((), fk, inp, pos + String.length(s), errs)
                case Err(_) => fk(pos, singleton("skippingAt failed"))
            }
        )

    /// Parsing fails if regex does not compile.
    pub def lookingAts(regex: String): TextParser[String, t] = 
        use Text/Regex/Pattern.{compile};
        bind(liftResult(compile(regex)), lookingAt) `swapError` "lookingAts"
        
    /// Parsing fails if regex does not compile.
    pub def skippingAts(regex: String): TextParser[Unit, t] = 
        use Text/Regex/Pattern.{compile};
        bind(liftResult(compile(regex)), skippingAt) `swapError` "skippingAts"


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
