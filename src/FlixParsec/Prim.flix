/*
 * Copyright 2021 Stephen Tetley
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


namespace FlixParsec {

    type alias ParseContext = {input: String, tabSize: Int32}

    type alias ParseState[u] = {pos: Int32, userState: u}

    type alias Failures = Chain[String]

    type alias FailK[t] = Int32 -> Failures ~> t

    type alias SuccessK[u, r, t] = r -> FailK[t] -> ParseContext -> ParseState[u] -> Failures ~> t

    pub opaque type TextParser[u, r, t] = SuccessK[u, r, t] -> FailK[t] -> ParseContext -> ParseState[u] -> Failures ~> t

    /// instance Functor[TextParser[r]] {
    ///     def map(f: a -> b & ef, x: TextParser[a, t]): TextParser[b, t] & ef = map(f, x)
    /// }

}

namespace FlixParsec/Prim {

    use FlixParsec.{TextParser, ParseContext, ParseState, Failures, SuccessK, FailK};
    use FlixParsec.TextParser.{TextParser};

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
        pos1.lineNumber == pos2.lineNumber and pos1.column == pos2.column

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

    def fileNotFoundError(path: System.FilePath): ParseError = 
        { message = "File not found: ${path}"
        , position = {lineNumber = 0, column = 0}
        , textBefore = ""
        , textAfter = ""
        }

    def generalError[a: ToString](err: a): ParseError = 
        { message = "${err}"
        , position = {lineNumber = 0, column = 0}
        , textBefore = ""
        , textAfter = ""
        }

    ///
    /// This is exposed so client code can implement wrappers over `run` etc. without
    /// knowing about the internal error type `Error1`.
    ///
    type alias TextParser1[t] = TextParser[Unit, t, Result[t, Error1]]

    ///
    /// Run parser `p` on the string `input`.
    ///
    pub def run(p: TextParser1[t], input: String) : Result[t, ParseError] & Impure =
        use Chain.{toList, empty};
        use FlixParsec/Internal/ErrorUtils.{textPos, getTextBeforeAndAfter};
        let context = {input = input, tabSize = 4};
        let state = {pos = 0, userState = ()};
        let fk = (posn, errs) -> Err({ msgs = toList(errs), pos = posn }) as & Impure;
        let sk = (a, _, _, _, _) -> Ok(a) as & Impure;
        let TextParser(p1) = p;
        match p1(sk, fk, context, state, empty()) {
            case Ok(a) => Ok(a)
            case Err(e1) => {
                let (l,c) = textPos(context.tabSize, e1.pos, input);
                let posn = { lineNumber = l, column = c };
                let (before, after) = getTextBeforeAndAfter(e1.pos, input);
                Err({ message = showErrorsList(e1.msgs)
                    , position = posn
                    , textBefore = before
                    , textAfter = after })
            }
        }

    ///
    pub def runs(p: TextParser1[t], input: String) : Result[t, String] & Impure =
        match run(p, input) {
            case Ok(a) => Ok(a)
            case Err(e) => Err(showPosition(e.position) + ": " + e.message)
        }

    pub def parseFile(parser: TextParser1[t], path: System.FilePath, cs: Text.Charset): Result[t, ParseError] & Impure =
        use Result.flatMap;
        let* exists = System/File.exists(path) |> Result.mapErr(generalError);
        if (exists) {
            let* src = System/File.readFile(path, cs) |> Result.mapErr(generalError);
            run(parser, src)
        } else
            Err(fileNotFoundError(path))


    pub def parsesFile(parser: TextParser1[t], path: System.FilePath, cs: Text.Charset): Result[t, String] & Impure =
        match parseFile(parser, path, cs) {
            case Ok(a) => Ok(a)
            case Err(e) => Err(showPosition(e.position) + ": " + e.message)
        }

    pub def parseFileWithBOM(parser: TextParser1[t], path: System.FilePath, cs: Text.Charset): Result[t, ParseError] & Impure =
        use Result.flatMap;
        let* exists = System/File.exists(path) |> Result.mapErr(generalError);
        if (exists) {
            let* src = System/File.readFileWithBOM(path, cs) |> Result.mapErr(generalError);
            run(parser, src)
        } else
            Err(fileNotFoundError(path))

    pub def parsesFileWithBOM(parser: TextParser1[t], path: System.FilePath, cs: Text.Charset): Result[t, String] & Impure =
        match parseFileWithBOM(parser, path, cs) {
            case Ok(a) => Ok(a)
            case Err(e) => Err(showPosition(e.position) + ": " + e.message)
        }

    pub def return(x: a): TextParser[u, a, t] = TextParser(succ -> succ(x))

    pub def failure(message: String) : TextParser[u, r, t] =
        use Chain.{singleton};
        TextParser((_, fk, _, st, _) -> fk(st.pos, singleton(message)))

    ///
    /// `swapError` is quite subtle. Swapping the error continuate needs to be
    /// a "local" operation which is undone (restored) afterwards (c.f. local in the Reader
    /// monad). It must not be a "stateful" operation with unlimited extent.
    ///
    pub def swapError(p: TextParser[u, r,t], message: String) : TextParser[u, r, t] =
        use Chain.{singleton};
        TextParser((sk, fk, ctx, st, errs) ->
            let fk1 = (pos, _) -> fk(pos, singleton(message));
            let sk1 = (x, _, _, st1, _) -> sk(x, fk, ctx, st1, errs);
            apply1(p, sk1, fk1, ctx, st, errs)
        )

    def apply1 (p: TextParser[u, r, t], sk: SuccessK[u, r, t], fk: FailK[t], ctx: ParseContext, st: ParseState[u], errs: Failures): t & Impure =
        let TextParser(pf) = p;
        pf(sk, fk, ctx, st, errs)

    pub def map(f: a -> b & ef, p: TextParser[u, a, t]): TextParser[u, b, t] =
        TextParser((sk, fk, ctx, pos, errs) ->
            let sk1 = (x, fk1, _, pos1, errs1) -> sk(f(x) as & Impure, fk1, ctx, pos1, errs1);
            apply1(p, sk1, fk, ctx, pos, errs)
        )

    pub def ap(mf: TextParser[u, a -> b & ef, t], ma: TextParser[u, a, t]): TextParser[u, b, t] =
        let* f = mf;
        let* a = ma;
        liftAction(_ -> f(a) as & Impure)

    pub def bind(ma: TextParser[u, q, t], k: q -> TextParser[u, r, t]):  TextParser[u, r, t] =
        TextParser(sk ->
            apply1(ma, x -> apply1(k(x), sk))
        )

    pub def flatMap(k: q -> TextParser[u, r, t], ma: TextParser[u, q, t]):  TextParser[u, r, t] = bind(ma, k)

    pub def alt(p: TextParser[u, a, t], q: TextParser[u, a, t]): TextParser[u, a, t] =
        use Chain.{append};
        TextParser((sk, fk, ctx, st, errs) ->
            let pfk = (_, perrs) -> {
                let qfk = (_, qerrs) -> fk(st.pos, append(perrs, qerrs));
                apply1(q, sk, qfk, ctx, st, perrs)
            };
            apply1(p, sk, pfk, ctx, st, errs)
        )


    pub def seqLeft(p: TextParser[u, r, t], q: TextParser[u, s, t]): TextParser[u, r, t] =
        TextParser(sk ->
            apply1(p, x -> apply1(q, _ -> sk(x)))
        )

    pub def seqRight(p: TextParser[u, r, t], q: TextParser[u, s, t]): TextParser[u, s, t] =
        TextParser(sk ->
            apply1(p, _ -> apply1(q, y -> sk(y)))
        )

    pub def ifThenElse(cond: TextParser[u, r, t], thenp: r -> TextParser[u, s, t], elsep: TextParser[u, s, t]): TextParser[u, s, t] = 
        TextParser((sk, fk, ctx, st) ->
            apply1(cond, x -> apply1(thenp(x), sk), _ -> apply1(elsep, sk, fk, ctx, st), ctx, st))

    pub def choice(ps: List[TextParser[u, a, t]]): TextParser[u, a, t] =
        use Chain.{singleton};
        TextParser((sk, fk, ctx, st, errs) ->
            match ps {
                case Nil => fk(st.pos, singleton("choice"))
                case p :: rs => {
                    let fk1 = (_, _) -> apply1(choice(rs), sk, fk, ctx, st, errs);
                    apply1(p, sk, fk1, ctx, st, errs)
                }
            }
        )


    pub def liftOption(x: Option[r], errMsg: String): TextParser[u, r, t] =  match x {
        case Some(a) => return(a)
        case None    => failure(errMsg)
    }

    pub def liftResult(x: Result[r, e]): TextParser[u, r, t] =  match x {
        case Ok(a) => return(a)
        case Err(_) => failure("liftResult")
    }

    pub def liftAction(f: Unit -> r & ef): TextParser[u, r, t] =
        TextParser((sk, fk, ctx, st, errs) ->
            let a = f() as & Impure;
            sk(a, fk, ctx, st, errs)
        )

    pub def liftActionResult(f: Unit -> Result[r, e] & ef): TextParser[u, r, t] =
        use Chain.{singleton};
        TextParser((sk, fk, ctx, st, errs) ->
            let ans = f() as & Impure;
            match ans {
                case Ok(a) => sk(a, fk, ctx, st, errs)
                case Err(_) => fk(st.pos, singleton("listActionResult"))
            }
        )

    pub def getPosition(): TextParser[u, Position, t] =  
        TextParser((sk, fk, ctx, st) ->
            let (x,y) = FlixParsec/Internal/ErrorUtils.textPos(ctx.tabSize, st.pos, ctx.input);
            sk({ lineNumber = x, column = y }, fk, ctx, st)
        )


    pub def endOfInput() : TextParser[u, Unit, t] =
        use Chain.{singleton};
        TextParser((sk, fk, ctx, st, errs) ->
            let len = String.length(ctx.input);
            if (st.pos >= len)
                sk((), fk, ctx, st, errs)
            else
                fk(st.pos, singleton("eof - not at end"))
        ) 

    pub def string(s: String): TextParser[u, String, t] =
        use Chain.{singleton};
        TextParser((sk, fk, ctx, st, errs) ->
            let strlen = String.length(s);
            let s1 = String.slice(ctx.input, st.pos, st.pos + strlen);
            if (s1 == s)
                sk(s, fk, ctx, {pos = st.pos + strlen | st}, errs)
            else {
                let msg = "string - expecting '" + s + "', encountered '" + s1 + "'";
                fk(st.pos, singleton(msg))
            }
        )

    pub def char(ch: Char): TextParser[u, Char, t] = 
        use Chain.{singleton};
        TextParser((sk, fk, ctx, st, errs) ->
            let len = String.length(ctx.input);
            if (st.pos < len) {
                let c = String.charAt(st.pos, ctx.input);
                if (c == ch)
                    sk(c, fk, ctx, {pos = st.pos + 1 | st}, errs)
                else  {
                    let msg = "char - expecting '" + Char.toString(ch) + "', encountered '" + Char.toString(c) + "'";
                    fk(st.pos, singleton(msg))
                }
            } else {
                let msg = "char - unexpected eof";
                fk(st.pos, singleton(msg))
            }
        )

    pub def anyChar(): TextParser[u, Char, t] =
        use Chain.{singleton};
        TextParser((sk, fk, ctx, st, errs) ->
            let len = String.length(ctx.input);
            if (st.pos < len) {
                let c = String.charAt(st.pos, ctx.input);
                sk(c, fk, ctx, {pos = st.pos + 1 | st}, errs)
            } else {
                let msg = "anyChar - eof";
                fk(st.pos, singleton(msg))
            }
        )


    // Truncates if eof...
    pub def horizon(n: Int32): TextParser[u, String, t] = 
        use Chain.{singleton};
        TextParser((sk, fk, ctx, st, errs) ->
            if (n < 0) {
                let msg = "horizon - invalid size";
                fk(st.pos, singleton(msg))
            } else {
                let str = String.slice(ctx.input, st.pos, st.pos + n);
                sk(str, fk, ctx, {pos = st.pos + String.length(str) | st}, errs)
            } 
        )


    ///
    /// lookahead - run the parser but don't consume input on success
    /// On fail, fail.
    ///
    pub def lookahead(p: TextParser[u, r, t]): TextParser[u, r, t] =
        TextParser((sk, fk, ctx, st) ->
            let sk1 = (ans, fk1, _, _) -> sk(ans, fk1, ctx, st);
            apply1(p, sk1, fk, ctx, st)
        )

    ///
    /// Run parser `p` within a bounded length
    ///
    pub def bounded(n: Int32, p: TextParser[u, r, t]): TextParser[u, r, t] =
        use Chain.{singleton};
        TextParser((sk, fk, ctx, st, errs) ->
            /// Applies parser `p` on a slice of the input, then "restores" the full input 
            /// for the success continuation.
            let str1 = String.slice(ctx.input, st.pos, st.pos + n);
            let fk1 = (_, _) -> fk(st.pos, singleton("bounded"));
            let sk1 = (ans, _, _, st1) -> sk(ans, fk, ctx, {pos = st.pos + String.length(str1) | st1});
            apply1(p, sk1, fk1, {input = str1 | ctx}, {pos = 0 | st}, errs)
        )


}
