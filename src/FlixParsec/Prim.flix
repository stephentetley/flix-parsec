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

    type alias FailK[t1] = Int32 -> Failures ~> t1

    type alias SuccessK[t1, u, ans] = ans -> FailK[t1] -> ParseContext -> ParseState[u] -> Failures ~> t1

    pub opaque type GenParser[t1, u, ans] = SuccessK[t1, u, ans] -> FailK[t1] -> ParseContext -> ParseState[u] -> Failures ~> t1

    type alias Parser[t1, ans] = GenParser[t1, Unit, ans]

    /// instance Functor[TextParser[r]] {
    ///     def map(f: a -> b & ef, x: GenParser[t1, u, a]): GenParser[t1, u, b] & ef = map(f, x)
    /// }

}

namespace FlixParsec/Prim {

    use FlixParsec.{GenParser, Parser, ParseContext, ParseState, Failures, SuccessK, FailK};
    use FlixParsec.GenParser.{GenParser};

    type alias Position = { lineNumber: Int32, column: Int32 }

    type alias Error1 = { messages: List[String], position: Int32 }

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

    def generalError[a](err: a): ParseError with ToString[a] = 
        { message = "${err}"
        , position = {lineNumber = 0, column = 0}
        , textBefore = ""
        , textAfter = ""
        }

    ///
    /// This is exposed so client code can implement wrappers over `run` etc. without
    /// knowing about the internal error type `Error1`.
    ///
    type alias Parser1[ans] = Parser[Result[ans, Error1], ans]

    type alias GenParser1[u, ans] = GenParser[Result[ans, Error1], u, ans]

    ///
    /// Run parser `p` on the string `input`.
    ///
    pub def runParser(p: Parser1[ans], tabSize: Int32, input: String) : Result[ans, ParseError] & Impure =
        runGenParser(p, (), tabSize, input)


    ///
    /// Run parser `p` on the string `input`.
    ///
    pub def runGenParser(p: GenParser1[u, ans], initState: u, tabSize: Int32, input: String) : Result[ans, ParseError] & Impure =
        use Chain.{toList, empty};
        use FlixParsec/Internal/ErrorUtils.{textPos, getTextBeforeAndAfter};
        let context = {input = input, tabSize = tabSize};
        let state = {pos = 0, userState = initState};
        let fk = (posn, errs) -> Err({ messages = toList(errs), position = posn }) as & Impure;
        let sk = (a, _, _, _, _) -> Ok(a) as & Impure;
        let GenParser(p1) = p;
        match p1(sk, fk, context, state, empty()) {
            case Ok(a) => Ok(a)
            case Err(e1) => {
                let (l,c) = textPos(context.tabSize, e1.position, input);
                let posn = { lineNumber = l, column = c };
                let (before, after) = getTextBeforeAndAfter(e1.position, input);
                Err({ message = showErrorsList(e1.messages)
                    , position = posn
                    , textBefore = before
                    , textAfter = after })
            }
        }

    ///
    pub def parseString(p: GenParser1[u, ans], initState: u, input: String) : Result[ans, String] & Impure =
        match runGenParser(p, initState, 4, input) {
            case Ok(a) => Ok(a)
            case Err(e) => Err(showPosition(e.position) + ": " + e.message)
        }

    pub def parsesString(p: Parser1[ans], input: String) : Result[ans, String] & Impure = parseString(p, (), input)

    pub def parseFile(parser: GenParser1[u, ans], 
                        initState: u, 
                        tabSize: Int32, 
                        path: System.FilePath, 
                        cs: Text.Charset): Result[ans, ParseError] & Impure =
        use Result.flatMap;
        let* exists = System/File.exists(path) |> Result.mapErr(generalError);
        if (exists) {
            let* src = System/File.readFile(path, cs) |> Result.mapErr(generalError);
            runGenParser(parser, initState, tabSize, src)
        } else
            Err(fileNotFoundError(path))


    pub def parsesFile(parser: Parser1[ans], tabSize: Int32, path: System.FilePath, cs: Text.Charset): Result[ans, String] & Impure =
        match parseFile(parser, (), tabSize, path, cs) {
            case Ok(a) => Ok(a)
            case Err(e) => Err(showPosition(e.position) + ": " + e.message)
        }

    pub def parseFileWithBOM(parser: GenParser1[u, ans], 
                                initState: u, 
                                tabSize: Int32, 
                                path: System.FilePath, 
                                cs: Text.Charset): Result[ans, ParseError] & Impure =
        use Result.flatMap;
        let* exists = System/File.exists(path) |> Result.mapErr(generalError);
        if (exists) {
            let* src = System/File.readFileWithBOM(path, cs) |> Result.mapErr(generalError);
            runGenParser(parser, initState, tabSize, src)
        } else
            Err(fileNotFoundError(path))

    pub def parsesFileWithBOM(parser: Parser1[ans], tabSize: Int32, path: System.FilePath, cs: Text.Charset): Result[ans, String] & Impure =
        match parseFileWithBOM(parser, (), tabSize, path, cs) {
            case Ok(a) => Ok(a)
            case Err(e) => Err(showPosition(e.position) + ": " + e.message)
        }

    pub def return(x: a): GenParser[t1, u, a] = GenParser(sk -> sk(x))

    pub def failure(message: String) : GenParser[t1, u, ans] =
        use Chain.{singleton};
        GenParser((_, fk, _, st, _) -> fk(st.pos, singleton(message)))

    ///
    /// `swapError` is quite subtle. Swapping the error continuation needs to be
    /// a "local" operation which is undone (restored) afterwards (c.f. local in the Reader
    /// monad). It must not be a "stateful" operation with unlimited extent.
    ///
    pub def swapError(p: GenParser[t1, u, ans], message: String) : GenParser[t1, u, ans] =
        use Chain.{singleton};
        GenParser((sk, fk, ctx, st, errs) ->
            let fk1 = (pos, _) -> fk(pos, singleton(message));
            let sk1 = (x, _, _, st1, _) -> sk(x, fk, ctx, st1, errs);
            apply1(p, sk1, fk1, ctx, st, errs)
        )

    def apply1 (p: GenParser[t1, u, ans], 
                sk: SuccessK[t1, u, ans], 
                fk: FailK[t1], 
                ctx: ParseContext, 
                st: ParseState[u], 
                errs: Failures): t1 & Impure =
        let GenParser(pf) = p;
        pf(sk, fk, ctx, st, errs)

    pub def map(f: a -> b & ef, p: GenParser[t1, u, a]): GenParser[t1, u, b] =
        GenParser((sk, fk, ctx, pos, errs) ->
            let sk1 = (x, fk1, _, pos1, errs1) -> sk(f(x) as & Impure, fk1, ctx, pos1, errs1);
            apply1(p, sk1, fk, ctx, pos, errs)
        )

    pub def ap(mf: GenParser[t1, u, a -> b & ef], ma: GenParser[t1, u, a]): GenParser[t1, u, b] =
        let* f = mf;
        let* a = ma;
        liftAction(_ -> f(a) as & Impure)

    pub def bind(ma: GenParser[t1, u, a], k: a -> GenParser[t1, u, b]):  GenParser[t1, u, b] =
        GenParser(sk ->
            apply1(ma, x -> apply1(k(x), sk))
        )

    pub def flatMap(k: a -> GenParser[t1, u, b], ma: GenParser[t1, u, a]):  GenParser[t1, u, b] = bind(ma, k)

    pub def alt(p: GenParser[t1, u, a], q: GenParser[t1, u, a]): GenParser[t1, u, a] =
        use Chain.{append};
        GenParser((sk, fk, ctx, st, errs) ->
            let pfk = (_, perrs) -> {
                let qfk = (_, qerrs) -> fk(st.pos, append(perrs, qerrs));
                apply1(q, sk, qfk, ctx, st, perrs)
            };
            apply1(p, sk, pfk, ctx, st, errs)
        )


    pub def seqLeft(p: GenParser[t1, u, a], q: GenParser[t1, u, b]): GenParser[t1, u, a] =
        GenParser(sk ->
            apply1(p, x -> apply1(q, _ -> sk(x)))
        )

    pub def seqRight(p: GenParser[t1, u, a], q: GenParser[t1, u, b]): GenParser[t1, u, b] =
        GenParser(sk ->
            apply1(p, _ -> apply1(q, y -> sk(y)))
        )

    pub def ifThenElse(cond: GenParser[t1, u, a], 
                        thenp: a -> GenParser[t1, u, b], 
                        elsep: GenParser[t1, u, b]): GenParser[t1, u, b] = 
        GenParser((sk, fk, ctx, st) ->
            apply1(cond, x -> apply1(thenp(x), sk), _ -> apply1(elsep, sk, fk, ctx, st), ctx, st))

    pub def choice(ps: List[GenParser[t1, u, a]]): GenParser[t1, u, a] =
        use Chain.{singleton};
        GenParser((sk, fk, ctx, st, errs) ->
            match ps {
                case Nil => fk(st.pos, singleton("choice"))
                case p :: rs => {
                    let fk1 = (_, _) -> apply1(choice(rs), sk, fk, ctx, st, errs);
                    apply1(p, sk, fk1, ctx, st, errs)
                }
            }
        )


    pub def liftOption(x: Option[a], errMsg: String): GenParser[t1, u, a] =  match x {
        case Some(a) => return(a)
        case None    => failure(errMsg)
    }

    pub def liftResult(x: Result[a, e]): GenParser[t1, u, a] =  match x {
        case Ok(a) => return(a)
        case Err(_) => failure("liftResult")
    }

    pub def liftAction(f: Unit -> a & ef): GenParser[t1, u, a] =
        GenParser((sk, fk, ctx, st, errs) ->
            let a = f() as & Impure;
            sk(a, fk, ctx, st, errs)
        )

    pub def liftActionResult(f: Unit -> Result[a, e] & ef): GenParser[t1, u, a] =
        use Chain.{singleton};
        GenParser((sk, fk, ctx, st, errs) ->
            let ans = f() as & Impure;
            match ans {
                case Ok(a) => sk(a, fk, ctx, st, errs)
                case Err(_) => fk(st.pos, singleton("listActionResult"))
            }
        )

    pub def getPosition(): GenParser[t1, u, Position] =  
        GenParser((sk, fk, ctx, st) ->
            let (x,y) = FlixParsec/Internal/ErrorUtils.textPos(ctx.tabSize, st.pos, ctx.input);
            sk({ lineNumber = x, column = y }, fk, ctx, st)
        )


    pub def endOfInput() : GenParser[t1, u, Unit] =
        use Chain.{singleton};
        GenParser((sk, fk, ctx, st, errs) ->
            let len = String.length(ctx.input);
            if (st.pos >= len)
                sk((), fk, ctx, st, errs)
            else
                fk(st.pos, singleton("eof - not at end"))
        ) 

    pub def string(s: String): GenParser[t1, u, String] =
        use Chain.{singleton};
        GenParser((sk, fk, ctx, st, errs) ->
            let strlen = String.length(s);
            let s1 = String.slice(ctx.input, st.pos, st.pos + strlen);
            if (s1 == s)
                sk(s, fk, ctx, {pos = st.pos + strlen | st}, errs)
            else {
                let msg = "string - expecting '" + s + "', encountered '" + s1 + "'";
                fk(st.pos, singleton(msg))
            }
        )

    pub def char(ch: Char): GenParser[t1, u, Char] = 
        use Chain.{singleton};
        GenParser((sk, fk, ctx, st, errs) ->
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

    pub def anyChar(): GenParser[t1, u, Char] =
        use Chain.{singleton};
        GenParser((sk, fk, ctx, st, errs) ->
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
    pub def horizon(n: Int32): GenParser[t1, u, String] = 
        use Chain.{singleton};
        GenParser((sk, fk, ctx, st, errs) ->
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
    pub def lookahead(p: GenParser[t1, u, a]): GenParser[t1, u, a] =
        GenParser((sk, fk, ctx, st) ->
            let sk1 = (ans, fk1, _, _) -> sk(ans, fk1, ctx, st);
            apply1(p, sk1, fk, ctx, st)
        )

    ///
    /// Run parser `p` within a bounded length
    ///
    pub def bounded(n: Int32, p: GenParser[t1, u, a]): GenParser[t1, u, a] =
        use Chain.{singleton};
        GenParser((sk, fk, ctx, st, errs) ->
            /// Applies parser `p` on a slice of the input, then "restores" the full input 
            /// for the success continuation.
            let str1 = String.slice(ctx.input, st.pos, st.pos + n);
            let fk1 = (_, _) -> fk(st.pos, singleton("bounded"));
            let sk1 = (ans, _, _, st1) -> sk(ans, fk, ctx, {pos = st.pos + String.length(str1) | st1});
            apply1(p, sk1, fk1, {input = str1 | ctx}, {pos = 0 | st}, errs)
        )


}
