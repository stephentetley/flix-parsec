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

    
    pub enum GenParser[ka: Type, ust: Type, ef: Bool, a: Type](
        Graded.EnvStateError[ka, ParseContext, ParseState[ust], ef, a]
        // ParseContext -> ParseState[ust] -> (a -> ParseState[ust] -> Result[String, ka] \ ef) -> Result[String, ka] \ ef
    )

    /// No user state
    pub type alias Parser[ka: Type, ef: Bool, a: Type] = GenParser[ka, Unit, ef, a]

    instance GradedFunctor[GenParser[ka, ust]] {
        pub def map(f: a -> b \ ef1, x: GenParser[ka1, ust, ef, a]): GenParser[ka1, ust, ef and ef1, b] = 
            FlixParsec.Prim.map(f, x)
    }

    instance GradedApplicative[GenParser[ka, ust]] {
        pub def point(x: a): GenParser[ka, ust, ef, a] = 
            FlixParsec.Prim.point(x)

        pub def ap(f: GenParser[ka, ust, ef1, a -> b \ ef2], m: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef and ef1 and ef2, b] = 
            FlixParsec.Prim.ap(f, m)
    }


    instance GradedMonad[GenParser[ka, ust]] {
        pub def flatMap(f: a -> GenParser[ka, ust, ef1, b] \ ef2, m: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef and ef1 and ef2, b] = 
            FlixParsec.Prim.flatMap(f, m)
    }

}

mod FlixParsec.Prim {

    use FlixParsec.{GenParser, Parser, ParseContext, ParseState}
    use FlixParsec.GenParser.{GenParser}

    def getGenParser(m: GenParser[ka, ust, ef, a]): Graded.EnvStateError[ka, ParseContext, ParseState[ust], ef, a] = 
        let GenParser(m1) = m;
        m1

    /// Note `SourcePosition` is derived when there is a parse error. 
    /// During parsing, the parse state tracks just an index into the input string.
    // pub type alias SourcePosition = 
    //     { lineNumber = Int32
    //     , column = Int32 
    //     }

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
    pub type alias Parser1[ef: Bool, a: Type] = Parser[(ParseState[Unit], a), ef, a]

    pub type alias GenParser1[ust: Type, ef: Bool, a: Type] = GenParser[(ParseState[ust], a), ust, ef, a]

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
        match FlixParsec.Internal.GradedExtras.runEnvStateError(p1, context, state) {
            case Ok((_, a)) => Ok(a)
            case Err(msg)   => Err(msg)
        }

    pub def parseString(p: GenParser1[ust, ef, a], initState: ust, input: String): Result[String, a] \ ef =
        runGenParser(p, initState, 4, input)

    /// 
    pub def parsesString(p: Parser1[ef, a], input: String): Result[String, a] \ ef = parseString(p, (), input)

    
    ///
    pub def swapError(p: GenParser[ka, ust, ef, a], message: String) : GenParser[ka, ust, ef, a] =
        GenParser(
            FlixParsec.Internal.GradedExtras.swapError(getGenParser(p), message)
        )

    pub def point(x: a): GenParser[ka, ust, ef, a] = 
        GenParser(Graded.EnvStateError.point(x))

    

    pub def map(f: a -> b \ ef1, ma: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef and ef1, b] =
        GenParser(
            Graded.EnvStateError.map(f, getGenParser(ma))
        )

    pub def ap(mf: GenParser[ka, ust, ef1, a -> b \ ef2], ma: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef and ef1 and ef2, b] =
        GenParser(
            Graded.EnvStateError.ap(getGenParser(mf), getGenParser(ma))
        )


    pub def flatMap(k: a -> GenParser[ka, ust, ef1, b] \ ef2, ma: GenParser[ka, ust, ef, a]):  GenParser[ka, ust, ef and ef1 and ef2, b] = 
        GenParser(
            Graded.EnvStateError.flatMap(a -> getGenParser(k(a)), getGenParser(ma))
        )

    pub def throwError(message: String) : GenParser[ka, st, ef, a] =
        GenParser(Graded.EnvStateError.throwError(message))



    // pub def ifThenElse(cond: GenParser[ka1, st, ef, a], 
    //                     thenp: a -> GenParser[ka1, st, ef1, b] \ ef2, 
    //                     elsep: GenParser[ka1, st, ef3, b]): GenParser[ka1, st, ef and ef1 and ef2 and ef3, b] = 
    //     GenParser((sk, fk, ctx, st) ->
    //         apply1(cond, x -> apply1(thenp(x), sk), _ -> apply1(elsep, sk, fk, ctx, st), ctx, st))

/*
    pub def liftOption(x: Option[a], errMsg: String): GenParser[ka1, st, a] =  match x {
        case Some(a) => point(a)
        case None    => fail(errMsg)
    }

    pub def liftResult(x: Result[a, String]): GenParser[ka1, st, a] =  match x {
        case Ok(a)    => point(a)
        case Err(msg) => fail(msg)
    }

    pub def liftResult1(x: Result[a, e], makeErrMsg: e -> String): GenParser[ka1, st, a] =  match x {
        case Ok(a) => point(a)
        case Err(e) => fail(makeErrMsg(e))
    }

    /// TODO - should float the effect to `GenParser[ka1, st, a] \ ef`
    pub def liftAction(f: Unit -> a \ ef): GenParser[ka1, st, a] \ ef =
        GenParser((sk, fk, ctx, st, errs) ->
            let a = unsafe_cast f() as \ {};
            sk(a, fk, ctx, st, errs)
        )

    pub def liftActionResult(f: Unit -> Result[a, e] \ ef): GenParser[ka1, st, a] \ ef =
        GenParser((sk, fk, ctx, st, errs) ->
            let ans = unsafe_cast f() as \ {};
            match ans {
                case Ok(a) => sk(a, fk, ctx, st, errs)
                case Err(_) => fk(st.pos, Chain.singleton("listActionResult"))
            }
        )

    pub def getPosition(): GenParser[ka1, st, SourcePosition] =  
        GenParser((sk, fk, ctx, st) ->
            let (x,y) = FlixParsec.Internal.ErrorUtils.textPos(ctx.tabSize, st.pos, ctx.input);
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

*/
}
