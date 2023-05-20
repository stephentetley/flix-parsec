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
    pub def runParser(p: Parser1[ef, a], tabSize: Int32, input: String) : Result[InternalError, a] \ ef =
        runGenParser(p, (), tabSize, input)


        
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
                        input: String) : Result[InternalError, a] \ ef =
        let context = {offset = offset.offset, input = input, tabSize = tabSize.tabSize};
        let state = {pos = 0, userState = initState};
        let GenParser(p1) = p;
        match FlixParsec.Internal.ParserMonad.runParserMonad(p1, context, state) {
            case Ok((_, a)) => Ok(a)
            case Err(msg)   => Err(msg)
        }

    ///
    /// Run parser `p` on the string `input`.
    ///
    pub def runGenParser(p: GenParser1[ust, ef, a], initState: ust, tabSize: Int32, input: String) : Result[InternalError, a] \ ef =
        runGenParser1(p, initState, offset = 0, tabSize = tabSize, input)

    pub def parseString(p: GenParser1[ust, ef, a], initState: ust, input: String): Result[InternalError, a] \ ef =
        runGenParser(p, initState, 4, input)

    /// 
    pub def parsesString(p: Parser1[ef, a], input: String): Result[InternalError, a] \ ef = parseString(p, (), input)

    
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

    /// `EnvStateError.gets` currently not sufficiently polymorphic in effect...
    pub def getState(): GenParser[ka1, ust, ef, ust] =  
        GenParser(FlixParsec.Internal.ParserMonad.get()) <$$> s -> s.userState

    /// `EnvStateError.update` currently not sufficiently polymorphic in effect...
    pub def setState(state: ust): GenParser[ka, ust, Pure, Unit] =  
        GenParser(FlixParsec.Internal.ParserMonad.update(s -> {userState = state | s}))


    pub def updateState(modify: ust -> ust \ ef1): GenParser[ka, ust, ef1, Unit] =  
        GenParser(FlixParsec.Internal.ParserMonad.update(s -> let us1 = modify(s.userState); {userState = us1 | s}))


    def getOffset(): GenParser[ka, st, Pure, Int32] =
        GenParser(FlixParsec.Internal.ParserMonad.asks(env -> env.offset))

    /// Not sufficiently polymorphic in effect...
    def getCursorPosition(): GenParser[ka, st, Pure, Int32] =
        GenParser(FlixParsec.Internal.ParserMonad.gets(s -> s.pos))

    /// Not sufficiently polymorphic in effect...
    def setCursorPosition(cpos: Int32): GenParser[ka, st, Pure, Unit] =
        GenParser(FlixParsec.Internal.ParserMonad.update(s -> {pos = cpos | s}))

    /// Not sufficiently polymorphic in effect...
    def totalInput(): GenParser[ka, st, Pure, String] =
        GenParser(FlixParsec.Internal.ParserMonad.asks(env -> env.input))
    
    /// Not sufficiently polymorphic in effect...
    def tabSize(): GenParser[ka, st, Pure, Int32] =
        GenParser(FlixParsec.Internal.ParserMonad.asks(env -> env.tabSize))


    pub def getPosition(): GenParser[ka, st, ef, SourcePosition] = 
        totalInput()                >>= input   ->
        getOffset()                 >>= offset  -> 
        getCursorPosition()         >>= pos     ->
        tabSize()                   >>= tabw    -> {
            let (x,y) = FlixParsec.Internal.ParseError.textPos(tabw, offset + pos, input);
            point({lineNumber = x, column = y})
        }


    pub def endOfInput(): GenParser[ka, st, ef, Unit] =
        (totalInput() <$$> String.length)   >>= len ->
        getCursorPosition()                 >>= pos -> 
        if (pos >= len) point() else throwError("endOfInput - not at end")


    /// TODO - this is (likely) the wrong primitive to use with Java / UTF16 strings.
    /// Woudl it be right if it returned a codepoint(Int32)?
    pub def takeCodePoint(): GenParser[ka, st, ef, Int32] =
        totalInput()                >>= input   ->
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

    // `n` is "atmost", if eof is found return upto eof.
    pub def take(n: Int32): GenParser[ka, st, ef, String] =
        if (n < 0) {
            throwError("take - invalid size")
        } else {
            totalInput()                >>= input   ->
            getCursorPosition()         >>= pos     -> {
                let maxlen = String.length(input);
                let end = if (pos + n > maxlen) maxlen else pos + n;
                let s1 = String.slice(start = pos, end = end, input);
                setCursorPosition(pos + n) *> point(s1)
            }
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




    /// Probably to difficult / invasive to implement until GradedLib GradedError 
    /// changes to have an associated error type rather than always fixed to String

/*
    ///
    /// Run parser `p` within a bounded length
    ///
    pub def bounded(n: Int32, p: GenParser[ka, st, ef, a]): GenParser[ka, st, ef, a] =
        totalInput()                >>= input   ->
        getOffset()                 >>= offset  -> 
        getCursorPosition()         >>= pos     ->
        tabSize()                   >>= tabw    -> {
            let (x,y) = FlixParsec.Internal.ErrorUtils.textPos(tabw, offset + pos, input);
            point({lineNumber = x, column = y})
        }
            /// Applies parser `p` on a slice of the input, then "restores" the full input 
            /// for the success continuation.
            let str1 = String.slice(st.pos, st.pos + n, ctx.input);
            let fk1 = (_, _) -> fk(st.pos, Chain.singleton("bounded"));
            let sk1 = (ans, _, _, st1) -> sk(ans, fk, ctx, {pos = st.pos + String.length(str1) | st1});
            apply1(p, sk1, fk1, {input = str1 | ctx}, {pos = 0 | st}, errs)
        )

*/

}
