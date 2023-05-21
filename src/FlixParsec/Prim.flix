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

    use FlixParsec.Internal.ParseError.InternalError
    
    /// Parser is env x state x failure

    type alias ParseContext = {offset = Int32, input = String, tabSize = Int32}

    type alias ParseState[ust: Type] = {pos = Int32, userState = ust}

    
    pub enum GenParser[ka: Type, ust: Type, ef: Eff, a: Type](
        FlixParsec.Internal.ParserMonad[ka, ParseContext, ParseState[ust], InternalError, ef, a]
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


}

mod FlixParsec.Prim {

    use GradedFunctor.{<$$>}
    use GradedApplicative.{*>}
    use GradedMonad.{>>=}
    use FlixParsec.{GenParser, Parser, ParseContext, ParseState}
    use FlixParsec.GenParser.{GenParser}
    use FlixParsec.Internal.ParseError.{SourcePosition, InternalError}
    use FlixParsec.Internal.ParserMonad

    def getGenParser(m: GenParser[ka, ust, ef, a]): ParserMonad[ka, ParseContext, ParseState[ust], InternalError, ef, a] = 
        let GenParser(m1) = m;
        m1



    ///
    /// This is exposed so client code can implement wrappers over `run` etc. without
    /// knowing about the internal type `ParseState`.
    ///
    pub type alias Parser1[ef: Eff, a: Type] = Parser[(ParseState[Unit], a), ef, a]

    pub type alias GenParser1[ust: Type, ef: Eff, a: Type] = GenParser[(ParseState[ust], a), ust, ef, a]

    ///
    /// Run parser `p` on the string `input`.
    ///
    pub def evalParser1(p: Parser1[ef, a], tabSize: Int32, input: String) : Result[InternalError, a] \ ef =
        evalGenParser1(p, (), tabSize, input)


        
    ///
    /// Run parser `p` on the string `input`.
    ///
    /// This allows running "nested" parsers - `offset` represents the start position
    /// of the input (sub-) String. 
    ///
    def runGenParser1(p: GenParser1[ust, ef, a], 
                        initState: ust, 
                        offset: {offset = Int32}, 
                        tabSize: {tabSize = Int32}, 
                        input: String) : Result[InternalError, (ParseState[ust], a)] \ ef =
        let context = {offset = offset.offset, input = input, tabSize = tabSize.tabSize};
        let state = {pos = 0, userState = initState};
        let GenParser(p1) = p;
        FlixParsec.Internal.ParserMonad.runParserMonad(p1, context, state)

    ///
    /// Run parser `p` on the string `input`.
    ///
    pub def evalGenParser1(p: GenParser1[ust, ef, a], initState: ust, tabSize: Int32, input: String) : Result[InternalError, a] \ ef =
        runGenParser1(p, initState, offset = 0, tabSize = tabSize, input)
            |> Result.map(snd)

    pub def parseString(p: GenParser1[ust, ef, a], initState: ust, input: String): Result[InternalError, a] \ ef =
        evalGenParser1(p, initState, 4, input)

    /// 
    pub def parsesString(p: Parser1[ef, a], input: String): Result[InternalError, a] \ ef = 
        parseString(p, (), input)

    
    pub def point(x: a): GenParser[ka, ust, ef, a] = 
        GenParser(FlixParsec.Internal.ParserMonad.point(x))

    

    pub def map(f: a -> b \ ef1, ma: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef + ef1, b] =
        GenParser(FlixParsec.Internal.ParserMonad.map(f, getGenParser(ma)))

    pub def ap(mf: GenParser[ka, ust, ef1, a -> b \ ef2], ma: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef + ef1 + ef2, b] =
        GenParser(FlixParsec.Internal.ParserMonad.ap(getGenParser(mf), getGenParser(ma)))


    pub def flatMap(k: a -> GenParser[ka, ust, ef1, b] \ ef2, ma: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef + ef1 + ef2, b] = 
        GenParser(FlixParsec.Internal.ParserMonad.flatMap(a -> getGenParser(k(a)), getGenParser(ma)))


    pub def orelse(ma: GenParser[ka, ust, ef1, a], mb: GenParser[ka, ust, ef2, a]): GenParser[ka, ust, ef1 + ef2, a] =
        GenParser(FlixParsec.Internal.ParserMonad.orelse(getGenParser(ma), getGenParser(mb)))


    pub def throwError(message: String): GenParser[ka, ust, ef, a] =
        getCursorPosition() >>= pos -> 
        GenParser(FlixParsec.Internal.ParserMonad.throwError({position = pos, message = message}))
    
    pub def mapError(f: InternalError -> InternalError \ ef1, ma: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef + ef1, a] = 
        GenParser(FlixParsec.Internal.ParserMonad.mapError(f, getGenParser(ma)))

    pub def catchError(ma: GenParser[ka, ust, ef, a], 
                        hdlr: InternalError -> GenParser[ka, ust, ef1, a] \ ef2): GenParser[ka, ust, ef + ef1 + ef2, a] = 
            GenParser(FlixParsec.Internal.ParserMonad.catchError(getGenParser(ma), msg -> getGenParser(hdlr(msg))))

    pub def reifyError(ma: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, Result[InternalError, a]] = 
        GenParser(FlixParsec.Internal.ParserMonad.reifyError(getGenParser(ma)))

    pub def swapError(err: InternalError, ma: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, a] = 
        mapError(_ -> err, ma)

    pub def swapErrorMessage(msg: String, ma: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, a] = 
        mapError(e -> {message = msg | e}, ma)


    pub def flatMapOr(start: GenParser[ka, ust, ef, a], 
                        mnext: a -> GenParser[ka, ust, ef1, b] \ ef2, 
                        melse: InternalError -> GenParser[ka, ust, ef3, b]): GenParser[ka, ust, ef + ef1 + ef2 + ef3, b] =
        GenParser(FlixParsec.Internal.ParserMonad.flatMapOr(getGenParser(start), 
                                                            a -> getGenParser(mnext(a)),
                                                            err -> getGenParser(melse(err))))

    pub def bind(ma: GenParser[ka, ust, ef, a], k: a -> GenParser[ka, ust, ef1, b] \ ef2): GenParser[ka, ust, ef + ef1 + ef2, b] = 
        flatMap(k, ma)

    pub def fail(message: String): GenParser[ka, ust, ef, a] = 
        throwError(message)


    pub def liftAction(f: Unit -> a \ ef): GenParser[ka, ust, ef, a] = 
        GenParser(FlixParsec.Internal.ParserMonad.liftAction(f))

    /// `EnvStateError.gets` currently not sufficiently polymorphic in effect...
    pub def getUserState(): GenParser[ka1, ust, ef, ust] =  
        GenParser(FlixParsec.Internal.ParserMonad.get()) <$$> s -> s.userState

    /// `EnvStateError.update` currently not sufficiently polymorphic in effect...
    pub def setUserState(state: ust): GenParser[ka, ust, ef, Unit] =  
        GenParser(FlixParsec.Internal.ParserMonad.update(s -> {userState = state | s}))


    pub def updateUserState(modify: ust -> ust \ ef1): GenParser[ka, ust, ef1, Unit] =  
        GenParser(FlixParsec.Internal.ParserMonad.update(s -> let us1 = modify(s.userState); {userState = us1 | s}))

    def ask(): GenParser[ka, ust, ef, ParseContext] =
        GenParser(FlixParsec.Internal.ParserMonad.ask())

    def get(): GenParser[ka, ust, ef, ParseState[ust]] =
        GenParser(FlixParsec.Internal.ParserMonad.get())


    def askOffset(): GenParser[ka, ust, ef, Int32] =
        GenParser(FlixParsec.Internal.ParserMonad.asks(env -> env.offset))

    def askTabSize(): GenParser[ka, ust, ef, Int32] =
        GenParser(FlixParsec.Internal.ParserMonad.asks(env -> env.tabSize))
    
    def askInput(): GenParser[ka, ust, ef, String] =
        GenParser(FlixParsec.Internal.ParserMonad.asks(env -> env.input))
    

    pub def getCursorPosition(): GenParser[ka, ust, ef, Int32] =
        GenParser(FlixParsec.Internal.ParserMonad.gets(s -> s.pos))

    pub def setCursorPosition(cpos: Int32): GenParser[ka, ust, ef, Unit] =
        GenParser(FlixParsec.Internal.ParserMonad.update(s -> {pos = cpos | s}))

    pub def incrCursorPosition(n: Int32): GenParser[ka, ust, ef, Unit] =
        GenParser(FlixParsec.Internal.ParserMonad.update(s -> {pos = s.pos + n | s}))


    pub def getSourcePosition(): GenParser[ka, ust, ef, SourcePosition] = 
        askInput()                  >>= input   ->
        askOffset()                 >>= offset  -> 
        getCursorPosition()         >>= pos     ->
        askTabSize()                >>= tabw    -> {
            let (x,y) = FlixParsec.Internal.ParseError.textPos(tabw, offset + pos, input);
            point({lineNumber = x, column = y})
        }


    pub def endOfInput(): GenParser[ka, ust, ef, Unit] =
        (askInput() <$$> String.length)         >>= len ->
        getCursorPosition()                     >>= pos -> 
        if (pos >= len) point() else throwError("endOfInput - not at end")


    pub def restOfInput(): GenParser[ka, ust, ef, String] =
        askInput()              >>= input ->
        getCursorPosition()     >>= pos -> 
        point(String.sliceRight(start = pos, input))


    /// TODO - this is (likely) the wrong primitive to use with Java / UTF16 strings.
    /// Woudl it be right if it returned a codepoint(Int32)?
    pub def takeCodePoint(): GenParser[ka, ust, ef, Int32] =
        askInput()                  >>= input   ->
        getCursorPosition()         >>= pos     -> {
            let len = String.length(input);
            if (pos < len) {
                match String.nthCodePoint(pos, input) {
                    case Some(cp) => {let cc = CodePoint.charCount(cp); setCursorPosition(pos+cc) *> point(cp)}
                    case None     => throwError("takeCodePoint - eof")
                }
            } else {
                throwError("takeCodePoint - eof")
            }
        }

    pub def takeChar(): GenParser[ka, ust, ef, Char] =
        askInput()                  >>= input   ->
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

    // `n` is "atmost", if eof is found return upto eof.
    pub def take(n: Int32): GenParser[ka, ust, ef, String] =
        if (n < 0) {
            throwError("take - invalid size")
        } else {
            askInput()                  >>= input   ->
            getCursorPosition()         >>= pos     -> {
                let maxlen = String.length(input);
                let end = if (pos + n > maxlen) maxlen else pos + n;
                let s1 = String.slice(start = pos, end = end, input);
                incrCursorPosition(String.length(s1)) *> point(s1)
            }
        }
    
    ///
    /// `lookahead` - run parser `p` but don't consume input on success
    /// On fail, fail.
    ///
    pub def lookahead(p: GenParser[ka, ust, ef, a]): GenParser[ka, ust, ef, a] =
        getCursorPosition()     >>= pos ->
        p                       >>= ans ->
        setCursorPosition(pos)  >>= _   ->
        point(ans)


    ///
    /// Run parser `p` within a bounded window of length `n`
    ///
    pub def bounded(n: Int32, p: GenParser1[ust, ef, a]): GenParser[ka, ust, ef, a] =
        ask()                       >>= ctx ->
        get()                       >>= st  -> {
            let str = String.slice(start = st.pos, end = st.pos + n, ctx.input);
            match runGenParser1(p, st.userState, offset = ctx.offset, tabSize = ctx.tabSize, str) {
                case Ok((_, a)) => {incrCursorPosition(n) *> point(a)}
                case Err(err)   => {incrCursorPosition(err.position) *> throwError(err.message)}
            }
        }

}
