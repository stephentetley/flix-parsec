/*
 * Copyright 2023 Stephen Tetley
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

mod FlixParsec.Internal {

    ///
    /// The ParserMonad monad is in the codensity style to be stack safe.
    /// 
    /// ParserMonad is GradedLib's EnvStateError monad with polymorphic error type.
    /// `ka` is the continuation answer type
    ///
    pub enum ParserMonad[ka: Type, env: Type, st: Type, err: Type, ef: Eff, a: Type](
        env -> st -> (a -> st -> Result[err, ka] \ ef) -> Result[err, ka] \ ef)

    instance GradedFunctor[ParserMonad[ka, env, st, err]] { 
        pub def map(f: a -> b \ ef1, m: ParserMonad[ka, env, st, err, ef, a]): ParserMonad[ka, env, st, err, ef + ef1, b] = 
            FlixParsec.Internal.ParserMonad.map(f, m)
    }

    instance GradedApplicative[ParserMonad[ka, env, st, err]] { 
        pub def point(x: a): ParserMonad[ka, env, st, err, ef, a] = 
            FlixParsec.Internal.ParserMonad.point(x)

        pub def ap(f: ParserMonad[ka, env, st, err, ef1, a -> b \ ef2], 
                    m: ParserMonad[ka, env, st, err, ef, a]): ParserMonad[ka, env, st, err, ef + ef1 + ef2, b] = 
            FlixParsec.Internal.ParserMonad.ap(f, m)
    }

    instance GradedMonad[ParserMonad[ka, env, st, err]] { 
        pub def flatMap(k: a -> ParserMonad[ka, env, st, err, ef1, b] \ ef2, 
                        m: ParserMonad[ka, env, st, err, ef, a]): ParserMonad[ka, env, st, err, ef + ef1 + ef2, b] = 
            FlixParsec.Internal.ParserMonad.flatMap(k, m)
    }

    // Orelse available for ParserMonad because state is pure (so resetable).

    instance GradedOr[ParserMonad[ka, env, st, err]] { 
        pub def orelse(ma: ParserMonad[ka, env, st, err, ef1, a], 
                        mb: ParserMonad[ka, env, st, err, ef2, a]): ParserMonad[ka, env, st, err, ef1 + ef2, a] =
            FlixParsec.Internal.ParserMonad.orelse(ma, mb)
    }




}

mod FlixParsec.Internal.ParserMonad {


    use FlixParsec.Internal.ParserMonad
    use FlixParsec.Internal.ParserMonad.{ParserMonad}


    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def apply1(ma: ParserMonad[ka, env, st, err, ef, a], 
                e: env,
                s: st, 
                k: a -> st -> Result[err, ka] \ ef): Result[err, ka] \ ef =
        let ParserMonad(ma1) = ma;
        ma1(e, s, k)

    def apply1WithHandler(ma: ParserMonad[ka, env, st, err, ef, a], 
                            e: env,
                            s: st, 
                            k: a -> st -> Result[err, ka] \ ef, 
                            handleK: err -> Result[err, ka] \ ef): Result[err, ka] \ ef =
        let ParserMonad(ma1) = ma;
        match ma1(e, s, k) {
            case Err(err) => handleK(err)
            case Ok(a)    => Ok(a)
        }



    ///
    /// Returns the result of applying `ma` to the state `s`.
    ///
    pub def runParserMonad(ma: ParserMonad[(st, a), env, st, err, ef, a], e: env, st: st): Result[err, (st, a)] \ ef =
        let ParserMonad(f) = ma;
        let cont = (a, s1) -> checked_ecast (Ok(s1, a));
        f(e, st, cont)

    pub def point(x: a): ParserMonad[ka, env, st, err, ef, a] = 
        ParserMonad((_, s, k) -> k(x, s))

    pub def map(f: a -> b \ ef1, ma: ParserMonad[ka, env, st, err, ef, a]): ParserMonad[ka, env, st, err, ef + ef1, b] =
        ParserMonad((e, s, k) ->
            apply1(unchecked_cast(ma as ParserMonad[ka, env, st, err, ef + ef1, a]), e, s, (a, s1) -> {let b = f(a); k(b, s1)})
        )

    pub def ap(mf: ParserMonad[ka, env, st, err, ef1, a -> b \ ef2], 
                ma: ParserMonad[ka, env, st, err, ef, a]): ParserMonad[ka, env, st, err, ef + ef1 + ef2, b] =
        ParserMonad((e, s, k) ->
            apply1(unchecked_cast(mf as ParserMonad[ka, env, st, err, ef + ef1 + ef2, a -> b \ ef2]), e, s, (f, s1) ->
                apply1(unchecked_cast(ma as ParserMonad[ka, env, st, err, ef + ef1 + ef2, a]), e, s1, (a, s2) ->
                    k(f(a), s2)))
        )

    pub def flatMap(f: a -> ParserMonad[ka, env, st, err, ef1, b] \ ef2, 
                    ma: ParserMonad[ka, env, st, err, ef, a]): ParserMonad[ka, env, st, err, ef + ef1 + ef2, b] =
        ParserMonad((e, s, k) ->
            apply1(unchecked_cast(ma as ParserMonad[ka, env, st, err, ef + ef1 + ef2, a]), e, s, (a, s1) ->
                apply1(unchecked_cast(f(a) as ParserMonad[ka, env, st, err, ef + ef1 + ef2, b]), e, s1, (b, s2) ->
                    k(b, s2)))
        )

    pub def orelse(ma: ParserMonad[ka, env, st, err, ef1, a], 
                    mb: ParserMonad[ka, env, st, err, ef2, a]): ParserMonad[ka, env, st, err, ef1 + ef2, a] =
        ParserMonad((e, s, k) ->
            apply1WithHandler(unchecked_cast(ma as ParserMonad[ka, env, st, err, ef1 + ef2, a]), e, s, a ->  k(a), _ -> 
                apply1(unchecked_cast(mb as ParserMonad[ka, env, st, err, ef1 + ef2, a]), e, s, b -> k(b)))
        )

    pub def throwError(err: err): ParserMonad[ka, env, st, err, ef, a] = 
        ParserMonad((_, _, _) -> checked_ecast(Err(err)))

    pub def catchError(ma: ParserMonad[ka, env, st, err, ef, a], 
                        hdlr: err -> ParserMonad[ka, env, st, err, ef1, a] \ ef2): ParserMonad[ka, env, st, err, ef + ef1 + ef2, a] = 
        ParserMonad((e, s, k) -> 
            let ParserMonad(ma1) = unchecked_cast(ma as ParserMonad[ka, env, st, err, ef + ef1 + ef2, a]);
            match ma1(e, s, k) {
                case Ok(a)    => Ok(a)
                case Err(err) => { 
                    let ParserMonad(hdlr1) = unchecked_cast(hdlr(err) as ParserMonad[ka, env, st, err, ef + ef1 + ef2, a]); 
                    hdlr1(e, s, k)
                }
            }
        )

    pub def mapError(f: err -> err \ ef1, ma: ParserMonad[ka, env, st, err, ef, a]): ParserMonad[ka, env, st, err, ef + ef1, a] = 
        ParserMonad((e, s, k) -> 
            apply1WithHandler(unchecked_cast(ma as ParserMonad[ka, env, st, err, ef + ef1, a]), e, s, k, err -> checked_ecast(Err(f(err))))
        )
   
    pub def reifyError(ma: ParserMonad[ka, env, st, err, ef, a]): ParserMonad[ka, env, st, err, ef, Result[err, a]] = 
        ParserMonad((e, s, k) -> 
            apply1WithHandler(ma, e, s, a -> k(Ok(a)), err -> k(Err(err), s))
        )


    // Failure is explicity trapped in `start` - if `start` is successful apply `mnext` to its answer ala `flatMap`.
    // If `start` fails apply `melse` to the failure value.
    pub def flatMapOr(start: ParserMonad[ka, env, st, err, ef, a], 
                        mnext: a -> ParserMonad[ka, env, st, err, ef1, b] \ ef2, 
                        melse: err -> ParserMonad[ka, env, st, err, ef3, b]): ParserMonad[ka, env, st, err, ef + ef1 + ef2 + ef3, b] =
       ParserMonad((e, s, k) -> 
            apply1WithHandler(unchecked_cast(start as ParserMonad[ka, env, st, err, ef + ef1 + ef2 + ef3, a]), e, s, 
                (a, s1) -> apply1(unchecked_cast(mnext(a) as ParserMonad[ka, env, st, err, ef + ef1 + ef2 + ef3, b]), e, s1, k),
                err -> apply1(unchecked_cast(melse(err) as ParserMonad[ka, env, st, err, ef + ef1 + ef2 + ef3, b]), e, s, k)
            )
       )


    pub def liftAction(f: Unit -> a \ ef): ParserMonad[ka, env, st, err, ef, a] = 
        ParserMonad((_, s, k) ->
            let a = f(); 
            k(a, s)  
        )


    pub def update(f: st -> st \ ef1): ParserMonad[ka, env, st, err, ef + ef1, Unit] = 
        ParserMonad((_, s, k) -> 
            k((), f(s))
        )


    pub def put(s: st): ParserMonad[ka, env, st, err, ef, Unit] = 
        ParserMonad((_, _, k) -> 
            k((), s)
        )

    // polymorphic `ef`...
    pub def get(): ParserMonad[ka, env, st, err, ef, st] = 
        ParserMonad((_, s, k) -> 
            k(s, s)
        )

    
    pub def gets(proj: st -> a \ ef1): ParserMonad[ka, env, st, err, ef + ef1, a] = 
        ParserMonad((_, s, k) -> 
            k(proj(s), s)
        )

    pub def ask(): ParserMonad[ka, env, st, err, ef, env] = 
        ParserMonad((e, s, k) -> 
            k(e, s)
        )

    pub def asks(proj: env -> a \ ef1): ParserMonad[ka, env, st, err, ef + ef1, a] = 
        ParserMonad((e, s, k) -> 
            k(proj(e), s)
        )

    pub def local(f: env -> env1 \ ef1, ma: ParserMonad[ka, env1, st, err, ef, a]): ParserMonad[ka, env, st, err, ef + ef1, a] = 
        ParserMonad((e, s, k) -> 
            apply1(unchecked_cast(ma as ParserMonad[ka, env1, st, err, ef + ef1, a]), f(e), s, (a1, s1) -> 
                k(a1, s1)
            )
        )
}
