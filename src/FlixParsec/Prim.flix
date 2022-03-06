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

    type alias ParseContext = {input :: String, tabSize :: Int32}

    type alias ParseState[st] = {pos :: Int32, userState :: st}

    type alias Failures = Chain[String]

    type alias FailK[ka1] = Int32 -> Failures -> ka1 & Impure

    type alias SuccessK[ka1, st, ans] = ans -> FailK[ka1] -> ParseContext -> ParseState[st] -> Failures -> ka1 & Impure

    /// `st` is the type of user state
    /// `ans` is the type of "immediate answer"
    /// `ka1` is the continuation answer type - if Flix had user visible existentials it would 
    /// be hidden as an existential type. Generally it will always be represented with a type variable
    /// except in "run" functions.
    pub opaque type GenParser[ka1, st, ans] = SuccessK[ka1, st, ans] -> FailK[ka1] -> ParseContext -> ParseState[st] -> Failures -> ka1 & Impure

    pub type alias Parser[ka1, ans] = GenParser[ka1, Unit, ans]

    instance Functor[GenParser[ka, st]] {
        pub def map(f: a -> b & ef, x: GenParser[ka1, st, a]): GenParser[ka1, st, b] & ef = 
            FlixParsec/Prim.map(f, x)
    }

    instance Applicative[GenParser[ka, st]] {
        pub def point(x: a): GenParser[ka, st, a] = 
            FlixParsec/Prim.point(x)

        pub def ap(f: GenParser[ka, st, a -> b & ef], x: GenParser[ka, st, a]): GenParser[ka, st, b] & ef = 
            FlixParsec/Prim.ap(f, x)
    }


    instance Monad[GenParser[ka, st]] {
        pub def flatMap(f: a -> GenParser[ka, st, b] & ef, o: GenParser[ka, st, a]): GenParser[ka, st, b] & ef = 
            FlixParsec/Prim.flatMap(f, o)
    }

}

namespace FlixParsec/Prim {

    use FlixParsec.{GenParser, Parser, ParseContext, ParseState, Failures, SuccessK, FailK};
    use FlixParsec.GenParser.{GenParser};

    /// Note `SourcePosition` is derived when there is a parse error. 
    /// During parsing, the parse state tracks just an index into the input string.
    pub type alias SourcePosition = { lineNumber :: Int32, column :: Int32 }

    pub type alias InternalError = { messages :: Chain[String], position :: Int32 }

    pub type alias ParseError = 
        { message :: String
        , position :: SourcePosition
        , textBefore :: String
        , textAfter :: String
        }

    pub def showSourcePosition(pos: SourcePosition): String = 
        "Line=${pos.lineNumber}, Col=${pos.column}"

    pub def samePosition(pos1: SourcePosition, pos2: SourcePosition): Bool =
        pos1.lineNumber == pos2.lineNumber and pos1.column == pos2.column

    pub def showParseError(err: ParseError): String = 
        String.unlines(err.message :: showSourcePosition(err.position) 
                            :: "Seen:" :: "-----" :: err.textBefore 
                            :: "Pending:" :: "--------" :: err.textAfter :: Nil)

    def showErrorsChain(stk: Chain[String]): String = match Chain.toList(stk) {
        case Nil => "*error*"
        case x :: rs => x + errorMessageHelper(rs)
    }

    def errorMessageHelper(rest: List[String]): String = match rest {
        case Nil => ""
        case x :: Nil => "[${x}]"
        case x :: rs => "[" + x + errorMessageHelper(rs) + "]"
    }

    pub def fileNotFoundError(path: System.FilePath): ParseError = 
        { message = "File not found: ${path}"
        , position = {lineNumber = 0, column = 0}
        , textBefore = ""
        , textAfter = ""
        }

    pub def generalError(err: a): ParseError with ToString[a] = 
        { message = "${err}"
        , position = {lineNumber = 0, column = 0}
        , textBefore = ""
        , textAfter = ""
        }

    ///
    /// This is exposed so client code can implement wrappers over `run` etc. without
    /// knowing about the internal error type `InternalError`.
    ///
    pub type alias Parser1[ans] = Parser[Result[ans, InternalError], ans]

    pub type alias GenParser1[st, ans] = GenParser[Result[ans, InternalError], st, ans]

    ///
    /// Run parser `p` on the string `input`.
    ///
    pub def runParser(p: Parser1[ans], tabSize: Int32, input: String) : Result[ans, ParseError] & Impure =
        runGenParser(p, (), tabSize, input)


    ///
    /// Run parser `p` on the string `input`.
    ///
    pub def runGenParser(p: GenParser1[st, ans], initState: st, tabSize: Int32, input: String) : Result[ans, ParseError] & Impure =
        use FlixParsec/Internal/ErrorUtils.{textPos, getTextBeforeAndAfter};
        let context = {input = input, tabSize = tabSize};
        let state = {pos = 0, userState = initState};
        let fk = (posn, errs) -> Err({messages = errs, position = posn}) as & Impure;
        let sk = (a, _, _, _, _) -> Ok(a) as & Impure;
        let GenParser(p1) = p;
        match p1(sk, fk, context, state, Chain.empty()) {
            case Ok(a) => Ok(a)
            case Err(e1) => {
                let (l, c) = textPos(context.tabSize, e1.position, input);
                let posn = { lineNumber = l, column = c };
                let (before, after) = getTextBeforeAndAfter(e1.position, input);
                Err({ message = showErrorsChain(e1.messages)
                    , position = posn
                    , textBefore = before
                    , textAfter = after })
            }
        }

    /// parseString is Impure because the parser might have lifted impure actions (e.g. debugging / tracing)
    pub def parseString(p: GenParser1[st, ans], initState: st, input: String): Result[ans, String] & Impure =
        match runGenParser(p, initState, 4, input) {
            case Ok(a) => Ok(a)
            case Err(e) => Err(showSourcePosition(e.position) + ": " + e.message)
        }

    /// 
    pub def parsesString(p: Parser1[ans], input: String): Result[ans, String] & Impure = parseString(p, (), input)

    pub def point(x: a): GenParser[ka1, st, a] = GenParser(sk -> sk(x))

    pub def throwError(message: String) : GenParser[ka1, st, ans] =
        GenParser((_, fk, _, st, _) -> fk(st.pos, Chain.singleton(message)))

    pub def fail(message: String) : GenParser[ka1, st, ans] = throwError(message)

    ///
    /// `swapError` is quite subtle. Swapping the error continuation needs to be
    /// a "local" operation which is undone (restored) afterwards (c.f. local in the Reader
    /// monad). It must not be a "stateful" operation with unlimited extent.
    ///
    pub def swapError(p: GenParser[ka1, st, ans], message: String) : GenParser[ka1, st, ans] =
        GenParser((sk, fk, ctx, st, errs) ->
            let fk1 = (pos, _) -> fk(pos, Chain.singleton(message));
            let sk1 = (x, _, _, st1, _) -> sk(x, fk, ctx, st1, errs);
            apply1(p, sk1, fk1, ctx, st, errs)
        )

    def apply1 (p: GenParser[ka1, st, ans], 
                sk: SuccessK[ka1, st, ans], 
                fk: FailK[ka1], 
                ctx: ParseContext, 
                st: ParseState[st], 
                errs: Failures): ka1 & Impure =
        let GenParser(pf) = p;
        pf(sk, fk, ctx, st, errs)

    pub def map(f: a -> b & ef, p: GenParser[ka1, st, a]): GenParser[ka1, st, b] & ef =
        GenParser((sk, fk, ctx, pos, errs) ->
            let sk1 = (x, fk1, _, pos1, errs1) -> sk(f(x) as & Impure, fk1, ctx, pos1, errs1);
            apply1(p, sk1, fk, ctx, pos, errs)
        ) as & ef

    pub def ap(mf: GenParser[ka1, st, a -> b & ef], ma: GenParser[ka1, st, a]): GenParser[ka1, st, b] & ef =
        (let* f = mf; let* a = ma; liftAction(_ -> f(a) as & Impure)) as & ef

    pub def bind(ma: GenParser[ka1, st, a], k: a -> GenParser[ka1, st, b] & ef):  GenParser[ka1, st, b] & ef =
        GenParser(sk ->
            apply1(ma, x -> apply1(k(x) as & Pure, sk))
        ) as & ef

    pub def flatMap(k: a -> GenParser[ka1, st, b] & ef, ma: GenParser[ka1, st, a]):  GenParser[ka1, st, b] & ef = 
        bind(ma, k)


    pub def ifThenElse(cond: GenParser[ka1, st, a], 
                        thenp: a -> GenParser[ka1, st, b], 
                        elsep: GenParser[ka1, st, b]): GenParser[ka1, st, b] = 
        GenParser((sk, fk, ctx, st) ->
            apply1(cond, x -> apply1(thenp(x), sk), _ -> apply1(elsep, sk, fk, ctx, st), ctx, st))


    pub def liftOption(x: Option[a], errMsg: String): GenParser[ka1, st, a] =  match x {
        case Some(a) => point(a)
        case None    => fail(errMsg)
    }

    pub def liftResult(x: Result[a, e], makeErrMsg: e -> String): GenParser[ka1, st, a] =  match x {
        case Ok(a) => point(a)
        case Err(e) => fail(makeErrMsg(e))
    }

    pub def liftAction(f: Unit -> a & ef): GenParser[ka1, st, a] =
        GenParser((sk, fk, ctx, st, errs) ->
            let a = f() as & Impure;
            sk(a, fk, ctx, st, errs)
        )

    pub def liftActionResult(f: Unit -> Result[a, e] & ef): GenParser[ka1, st, a] =
        GenParser((sk, fk, ctx, st, errs) ->
            let ans = f() as & Impure;
            match ans {
                case Ok(a) => sk(a, fk, ctx, st, errs)
                case Err(_) => fk(st.pos, Chain.singleton("listActionResult"))
            }
        )

    pub def getPosition(): GenParser[ka1, st, SourcePosition] =  
        GenParser((sk, fk, ctx, st) ->
            let (x,y) = FlixParsec/Internal/ErrorUtils.textPos(ctx.tabSize, st.pos, ctx.input);
            sk({ lineNumber = x, column = y }, fk, ctx, st)
        )

    pub def getState(): GenParser[ka1, st, st] =  
        GenParser((sk, fk, ctx, st) ->            
            sk(st.userState, fk, ctx, st)
        )

    pub def setState(state: st): GenParser[ka1, st, Unit] =  
        GenParser((sk, fk, ctx, st) ->
            let st1 = { userState = state | st};
            sk((), fk, ctx, st1)
        )


    pub def updateState(modify: st -> st): GenParser[ka1, st, Unit] =  
        GenParser((sk, fk, ctx, st) ->
            let st1 = { userState = modify(st.userState) | st};
            sk((), fk, ctx, st1)
        )  


    pub def endOfInput() : GenParser[ka1, st, Unit] =
        GenParser((sk, fk, ctx, st, errs) ->
            let len = String.length(ctx.input);
            if (st.pos >= len)
                sk((), fk, ctx, st, errs)
            else
                fk(st.pos, Chain.singleton("eof - not at end"))
        ) 

    pub def take(len: Int32): GenParser[ka1, st, String] =
        GenParser((sk, fk, ctx, st, errs) ->
            let slen = String.length(ctx.input);
            let end = if (st.pos + len > slen) slen else st.pos + len;
            let s1 = String.slice(st.pos, end, ctx.input);
            sk(s1, fk, ctx, {pos = st.pos + len | st}, errs)
        )


    pub def take1(): GenParser[ka1, st, Char] =
        GenParser((sk, fk, ctx, st, errs) ->
            let len = String.length(ctx.input);
            if (st.pos < len) {
                let c = String.charAt(st.pos, ctx.input);
                sk(c, fk, ctx, {pos = st.pos + 1 | st}, errs)
            } else {
                let msg = "anyChar - eof";
                fk(st.pos, Chain.singleton(msg))
            }
        )


    // Truncates if eof...
    pub def horizon(n: Int32): GenParser[ka1, st, String] = 
        GenParser((sk, fk, ctx, st, errs) ->
            if (n < 0) {
                let msg = "horizon - invalid size";
                fk(st.pos, Chain.singleton(msg))
            } else {
                let str = String.slice(st.pos, st.pos + n, ctx.input);
                sk(str, fk, ctx, {pos = st.pos + String.length(str) | st}, errs)
            } 
        )


    ///
    /// lookahead - run the parser but don't consume input on success
    /// On fail, fail.
    ///
    pub def lookahead(p: GenParser[ka1, st, a]): GenParser[ka1, st, a] =
        GenParser((sk, fk, ctx, st) ->
            let sk1 = (ans, fk1, _, _) -> sk(ans, fk1, ctx, st);
            apply1(p, sk1, fk, ctx, st)
        )

    ///
    /// Run parser `p` within a bounded length
    ///
    pub def bounded(n: Int32, p: GenParser[ka1, st, a]): GenParser[ka1, st, a] =
        GenParser((sk, fk, ctx, st, errs) ->
            /// Applies parser `p` on a slice of the input, then "restores" the full input 
            /// for the success continuation.
            let str1 = String.slice(st.pos, st.pos + n, ctx.input);
            let fk1 = (_, _) -> fk(st.pos, Chain.singleton("bounded"));
            let sk1 = (ans, _, _, st1) -> sk(ans, fk, ctx, {pos = st.pos + String.length(str1) | st1});
            apply1(p, sk1, fk1, {input = str1 | ctx}, {pos = 0 | st}, errs)
        )


}