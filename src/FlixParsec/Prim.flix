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


mod FlixParsec {

    /// Parser is env x state x failure

    type alias ParseContext = {input = String, tabSize = Int32}

    type alias ParseState[ust: Type] = {pos = Int32, userState = ust}

    
    pub enum GenParser[ka: Type, ust: Type, ef: Eff, a: Type](
        Graded.EnvStateError[ka, ParseContext, ParseState[ust], ef, a]
    )

    /// No user state
    pub type alias Parser[ka: Type, ef: Eff, a: Type] = GenParser[ka, Unit, ef, a]

    instance GradedFunctor[GenParser[ka, ust]] {
        pub def map(f: a -> b \ ef1, x: GenParser[ka1, ust, ef, a]): GenParser[ka1, ust, ef + ef1, b] = 
            FlixParsec.Prim.map(f, x)
    }

    instance GradedApplicative[GenParser[ka, ust]] {
        pub def point(x: a): GenParser[ka, ust, ef, a] = 
            FlixParsec.Prim.point(x)

        pub def ap(f: GenParser[ka, ust, ef1, a -> b \ ef2], m: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef + ef1 + ef2, b] = 
            FlixParsec.Prim.ap(f, m)
    }


    instance GradedMonad[GenParser[ka, ust]] {
        pub def flatMap(f: a -> GenParser[ka, ust, ef1, b] \ ef2, m: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef + ef1 + ef2, b] = 
            FlixParsec.Prim.flatMap(f, m)
    }

    instance GradedOr[GenParser[ka, ust]] { 
        pub def orelse(ma: GenParser[ka, ust, ef1, a], mb: GenParser[ka, ust, ef2, a]): GenParser[ka, ust, ef1 + ef2, a] =
            FlixParsec.Prim.orelse(ma, mb)
    }

    instance GradedError[GenParser[ka, ust]] {
        pub def throwError(message: String): GenParser[ka, ust, ef, a] =
            FlixParsec.Prim.throwError(message)
    
        pub def mapError(f: String -> String \ ef1, ma: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef + ef1, a] = 
            FlixParsec.Prim.mapError(f, ma)

        pub def catchError(ma: GenParser[ka, ust, ef, a], 
                            hdlr: String -> GenParser[ka, ust, ef1, a] \ ef2): GenParser[ka, ust, ef + ef1 + ef2, a] = 
            FlixParsec.Prim.catchError(ma, hdlr)

        pub def reifyError(ma: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, Result[String, a]] = 
            FlixParsec.Prim.reifyError(ma)
    }

}

mod FlixParsec.Prim {

    use GradedFunctor.{<$$>}
    use GradedApplicative.{*>}
    use GradedMonad.{>>=}
    use FlixParsec.{GenParser, Parser, ParseContext, ParseState}
    use FlixParsec.GenParser.{GenParser}

    def getGenParser(m: GenParser[ka, ust, ef, a]): Graded.EnvStateError[ka, ParseContext, ParseState[ust], ef, a] = 
        let GenParser(m1) = m;
        m1

    ///
    /// Note `SourcePosition` is derived when there is a parse error. 
    /// During parsing, the parse state tracks just an index (cursor) into the input string.
    ///
    pub type alias SourcePosition = 
        { lineNumber = Int32
        , column = Int32 
        }

    // pub type alias InternalError = 
    //     { message = String
    //     , position = Int32 
    //     }

    // pub type alias ParseError = 
    //     { message = String
    //     , position = SourcePosition
    //     , textBefore = String
    //     , textAfter = String
    //     }

    // pub def showSourcePosition(pos: SourcePosition): String = 
    //     "Line=${pos.lineNumber}, Col=${pos.column}"

    // pub def samePosition(pos1: SourcePosition, pos2: SourcePosition): Bool =
    //     pos1.lineNumber == pos2.lineNumber and pos1.column == pos2.column

    // pub def showParseError(err: ParseError): String = 
    //     String.unlines(err.message :: showSourcePosition(err.position) 
    //                         :: "Seen:" :: "-----" :: err.textBefore 
    //                         :: "Pending:" :: "--------" :: err.textAfter :: Nil)

    // def showErrorsChain(stk: Chain[String]): String = match Chain.toList(stk) {
    //     case Nil => "*error*"
    //     case x :: rs => x + errorMessageHelper(rs)
    // }

    // def errorMessageHelper(rest: List[String]): String = match rest {
    //     case Nil => ""
    //     case x :: Nil => "[${x}]"
    //     case x :: rs => "[" + x + errorMessageHelper(rs) + "]"
    // }

    // pub def fileNotFoundError(path: String): ParseError = 
    //     { message = "File not found: ${path}"
    //     , position = {lineNumber = 0, column = 0}
    //     , textBefore = ""
    //     , textAfter = ""
    //     }

    // pub def generalError(err: a): ParseError with ToString[a] = 
    //     { message = "${err}"
    //     , position = {lineNumber = 0, column = 0}
    //     , textBefore = ""
    //     , textAfter = ""
    //     }

    ///
    /// This is exposed so client code can implement wrappers over `run` etc. without
    /// knowing about the internal type `ParseState`.
    ///
    pub type alias Parser1[ef: Eff, a: Type] = Parser[(ParseState[Unit], a), ef, a]

    pub type alias GenParser1[ust: Type, ef: Eff, a: Type] = GenParser[(ParseState[ust], a), ust, ef, a]

    ///
    /// Run parser `p` on the string `input`.
    ///
    pub def runParser(p: Parser1[ef, a], tabSize: Int32, input: String) : Result[String, a] \ ef =
        runGenParser(p, (), tabSize, input)


    ///
    /// Run parser `p` on the string `input`.
    ///
    pub def runGenParser(p: GenParser1[st, ef, a], initState: st, tabSize: Int32, input: String) : Result[String, a] \ ef =
        let context = {input = input, tabSize = tabSize};
        let state = {pos = 0, userState = initState};
        let GenParser(p1) = p;
        match Graded.EnvStateError.runEnvStateError(p1, context, state) {
            case Ok((_, a)) => Ok(a)
            case Err(msg)   => Err(msg)
        }

    pub def parseString(p: GenParser1[ust, ef, a], initState: ust, input: String): Result[String, a] \ ef =
        runGenParser(p, initState, 4, input)

    /// 
    pub def parsesString(p: Parser1[ef, a], input: String): Result[String, a] \ ef = parseString(p, (), input)

    
    pub def point(x: a): GenParser[ka, ust, ef, a] = 
        GenParser(Graded.EnvStateError.point(x))

    

    pub def map(f: a -> b \ ef1, ma: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef + ef1, b] =
        GenParser(Graded.EnvStateError.map(f, getGenParser(ma)))

    pub def ap(mf: GenParser[ka, ust, ef1, a -> b \ ef2], ma: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef + ef1 + ef2, b] =
        GenParser(Graded.EnvStateError.ap(getGenParser(mf), getGenParser(ma)))


    pub def flatMap(k: a -> GenParser[ka, ust, ef1, b] \ ef2, ma: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef + ef1 + ef2, b] = 
        GenParser(Graded.EnvStateError.flatMap(a -> getGenParser(k(a)), getGenParser(ma)))


    pub def orelse(ma: GenParser[ka, ust, ef1, a], mb: GenParser[ka, ust, ef2, a]): GenParser[ka, ust, ef1 + ef2, a] =
        GenParser(Graded.EnvStateError.orelse(getGenParser(ma), getGenParser(mb)))

    pub def throwError(message: String): GenParser[ka, ust, ef, a] =
        GenParser(Graded.EnvStateError.throwError(message))
    
    pub def mapError(f: String -> String \ ef1, ma: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef + ef1, a] = 
        GenParser(Graded.EnvStateError.mapError(f, getGenParser(ma)))

    pub def catchError(ma: GenParser[ka, ust, ef, a], 
                        hdlr: String -> GenParser[ka, ust, ef1, a] \ ef2): GenParser[ka, ust, ef + ef1 + ef2, a] = 
            GenParser(Graded.EnvStateError.catchError(getGenParser(ma), msg -> getGenParser(hdlr(msg))))

    pub def reifyError(ma: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, Result[String, a]] = 
        GenParser(Graded.EnvStateError.reifyError(getGenParser(ma)))
    
    pub def bind(ma: GenParser[ka, ust, ef, a], k: a -> GenParser[ka, ust, ef1, b] \ ef2): GenParser[ka, ust, ef + ef1 + ef2, b] = 
        flatMap(k, ma)

    pub def fail(message: String): GenParser[ka, ust, ef, a] = 
        throwError(message)

    /// `EnvStateError.gets` currently not sufficiently polymorphic in effect...
    pub def getState(): GenParser[ka1, ust, ef, ust] =  
        GenParser(Graded.EnvStateError.get()) <$$> s -> s.userState

    /// `EnvStateError.update` currently not sufficiently polymorphic in effect...
    pub def setState(state: ust): GenParser[ka, ust, Pure, Unit] =  
        GenParser(Graded.EnvStateError.update(s -> {userState = state | s}))


    pub def updateState(modify: ust -> ust \ ef1): GenParser[ka, ust, ef1, Unit] =  
        GenParser(Graded.EnvStateError.update(s -> let us1 = modify(s.userState); {userState = us1 | s}))

    /// Not sufficiently polymorphic in effect...
    def getCursorPosition(): GenParser[ka, st, Pure, Int32] =
        GenParser(Graded.EnvStateError.gets(s -> s.pos))

    /// Not sufficiently polymorphic in effect...
    def setCursorPosition(cpos: Int32): GenParser[ka, st, Pure, Unit] =
        GenParser(Graded.EnvStateError.update(s -> {pos = cpos | s}))

    /// Not sufficiently polymorphic in effect...
    def totalInput(): GenParser[ka, st, Pure, String] =
        GenParser(Graded.EnvStateError.asks(env -> env.input))
    
    /// Not sufficiently polymorphic in effect...
    def tabSize(): GenParser[ka, st, Pure, Int32] =
        GenParser(Graded.EnvStateError.asks(env -> env.tabSize))


    pub def getPosition(): GenParser[ka, st, ef, SourcePosition] = 
        totalInput()                >>= input   ->
        getCursorPosition()         >>= pos     ->
        tabSize()                   >>= tabw    -> {
            let (x,y) = FlixParsec.Internal.ErrorUtils.textPos(tabw, pos, input);
            point({lineNumber = x, column = y})
        }


    pub def endOfInput(): GenParser[ka, st, ef, Unit] =
        (totalInput() <$$> String.length)   >>= len ->
        getCursorPosition()                    >>= pos -> 
        if (pos >= len) point() else throwError("endOfInput - not at end")


    /// TODO - this is (likely) the wrong primitive to use with Java / UTF16 strings.
    /// Woudl it be right if it returned a codepoint(Int32)?
    pub def takeCodePoint(): GenParser[ka, st, ef, Int32] =
        totalInput()                >>= input   ->
        getCursorPosition()         >>= pos     -> {
            let len = String.length(input);
            if (pos < len) {
                match String.nthCodePoint(pos, input) {
                    case Some(cp) => {let cc = FlixParsec.Internal.CharExtras.charCount(cp); setCursorPosition(pos+cc) *> point(cp)}
                    case None     => throwError("takeCodePoint - eof")
                }
            } else {
                throwError("takeCodePoint - eof")
            }
        }

    pub def takeChar(): GenParser[ka, st, ef, Char] =
        totalInput()                >>= input   ->
        getCursorPosition()         >>= pos     -> {
            let len = String.length(input);
            if (pos < len) {
                match String.nth(pos, input) {
                    case Some(c)  => {setCursorPosition(pos+1) *> point(c)}
                    case None     => throwError("takeChar - eof")
                }
            } else {
                throwError("takeChar - eof")
            }
        }

    pub def take(n: Int32): GenParser[ka, st, ef, String] =
        totalInput()                >>= input   ->
        getCursorPosition()         >>= pos     -> {
            let maxlen = String.length(input);
            let end = if (pos + n > maxlen) maxlen else pos + n;
            let s1 = String.slice(start = pos, end = end, input);
            setCursorPosition(pos + n) *> point(s1)
        }
    
    ///
    /// lookahead - run the parser but don't consume input on success
    /// On fail, fail.
    ///
    pub def lookahead(p: GenParser[ka, st, ef, a]): GenParser[ka, st, ef, a] =
        getCursorPosition()     >>= pos ->
        p                       >>= ans ->
        setCursorPosition(pos)  >>= _   ->
        point(ans)


/*
    pub def liftOption(x: Option[a], errMsg: String): GenParser[ka, st, ef, a] =  match x {
        case Some(a) => point(a)
        case None    => fail(errMsg)
    }

    pub def liftResult(x: Result[a, String]): GenParser[ka, st, ef, a] =  match x {
        case Ok(a)    => point(a)
        case Err(msg) => fail(msg)
    }

    pub def liftResult1(x: Result[a, e], makeErrMsg: e -> String): GenParser[ka, st, ef, a] =  match x {
        case Ok(a) => point(a)
        case Err(e) => fail(makeErrMsg(e))
    }

    /// TODO - should float the effect to `GenParser[ka, st, ef, a] \ ef`
    pub def liftAction(f: Unit -> a \ ef): GenParser[ka, st, ef, a] \ ef =
        GenParser((sk, fk, ctx, st, errs) ->
            let a = unsafe_cast f() as \ {};
            sk(a, fk, ctx, st, errs)
        )

    pub def liftActionResult(f: Unit -> Result[a, e] \ ef): GenParser[ka, st, ef, a] \ ef =
        GenParser((sk, fk, ctx, st, errs) ->
            let ans = unsafe_cast f() as \ {};
            match ans {
                case Ok(a) => sk(a, fk, ctx, st, errs)
                case Err(_) => fk(st.pos, Chain.singleton("listActionResult"))
            }
        )

        




    // Truncates if eof...
    pub def horizon(n: Int32): GenParser[ka, st, ef, String] = 
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
    /// Run parser `p` within a bounded length
    ///
    pub def bounded(n: Int32, p: GenParser[ka, st, ef, a]): GenParser[ka, st, ef, a] =
        GenParser((sk, fk, ctx, st, errs) ->
            /// Applies parser `p` on a slice of the input, then "restores" the full input 
            /// for the success continuation.
            let str1 = String.slice(st.pos, st.pos + n, ctx.input);
            let fk1 = (_, _) -> fk(st.pos, Chain.singleton("bounded"));
            let sk1 = (ans, _, _, st1) -> sk(ans, fk, ctx, {pos = st.pos + String.length(str1) | st1});
            apply1(p, sk1, fk1, {input = str1 | ctx}, {pos = 0 | st}, errs)
        )

*/

}
