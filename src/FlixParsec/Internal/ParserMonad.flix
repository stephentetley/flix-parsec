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



mod FlixParsec.Internal.ParserMonad {



    pub enum ParserMonad[ka: Type, env: Type, st: Type, err: Type, a: Type](
        SuccessK[ka, env, st, err, a] -> FailK[ka, st, err] -> env -> st -> ka
    )

    type alias FailK[ka: Type, st: Type, err: Type] = err -> st -> ka

    type alias SuccessK[ka: Type, env: Type, st: Type, err: Type, a: Type] = a -> FailK[ka, st, err] -> env -> st -> ka

    instance Functor[ParserMonad[ka, env, st, err]] {
        pub def map(f: a -> b \ ef, 
                    ma: ParserMonad[ka, env, st, err, a]): ParserMonad[ka, env, st, err, b] \ ef = 
            mapImpl(f, ma)    
    }

    instance Applicative[ParserMonad[ka, env, st, err]] {
        pub def point(x: a): ParserMonad[ka, env, st, err, a] = pointImpl(x)
        pub def ap(pf: ParserMonad[ka, env, st, err, a -> b \ ef], 
                pa: ParserMonad[ka, env, st, err, a]): ParserMonad[ka, env, st, err, b] \ ef =
            apImpl(pf, pa)
    }
    
    instance Monad[ParserMonad[ka, env, st, err]] {
        pub def flatMap(k: a -> ParserMonad[ka, env, st, err, b] \ ef, 
                    pa: ParserMonad[ka, env, st, err, a]):  ParserMonad[ka, env, st, err, b] \ ef =
            flatMapImpl(k, pa)
    }


    pub def runParserMonad(p: ParserMonad[Result[(st, err), (st, a)], env, st, err, a], 
                            env: env, 
                            st: st): Result[(st, err), (st, a)] =
        def fk(err, st2) = Err((st2, err));
        def sk(a, _, _, st1) = Ok((st1, a));
        let ParserMonad(p1) = p;
        p1(sk, fk, env, st)


    def pointImpl(x: a): ParserMonad[ka, env, st, err, a] = 
        ParserMonad((sk, fk, env, st) -> sk(x, fk, env, st))


    def mapImpl(f: a -> b \ ef, ma: ParserMonad[ka, env, st, err, a]): ParserMonad[ka, env, st, err, b] \ ef =
        let ParserMonad(g) = ma;
        unchecked_cast(ParserMonad((sk, fk, env, st) -> {
            def sk1(x, fk1, env1, st1) = unchecked_cast(sk(f(x), fk1, env1, st1) as _ \ {});
            g(sk1, fk, env, st)
        }) as _ \ ef)


    def apImpl(pf: ParserMonad[ka, env, st, err, a -> b \ ef], 
                pa: ParserMonad[ka, env, st, err, a]): ParserMonad[ka, env, st, err, b] \ ef =
        let ParserMonad(g) = pf;
        let ParserMonad(h) = pa;
        unchecked_cast(ParserMonad((sk, fk, env, st) -> {
            def sk1(f, fk1, env1, st1) = {
                def sk2(x, fk2, env2, st2) = unchecked_cast(sk(f(x), fk2, env2, st2) as _ \ {});
                h(sk2, fk1, env1, st1)
            };
            g(sk1, fk, env, st)
        }) as _ \ ef)


    def flatMapImpl(k: a -> ParserMonad[ka, env, st, err, b] \ ef, 
                    pa: ParserMonad[ka, env, st, err, a]):  ParserMonad[ka, env, st, err, b] \ ef =
        let ParserMonad(g) = pa;
        unchecked_cast(ParserMonad((sk, fk, env, st) -> {
            def sk1(x, fk1, env1, st1) = {                
                let ParserMonad(h) = unchecked_cast(k(x) as _ \ {});
                h(sk, fk1, env1, st1)
            };
            g(sk1, fk, env, st)
        }) as _ \ ef)

    pub def alt(pa: ParserMonad[ka, env, st, err, a], pb: ParserMonad[ka, env, st, err, a]): ParserMonad[ka, env, st, err, a] = 
        let ParserMonad(g) = pa;
        let ParserMonad(h) = pb;
        unchecked_cast(ParserMonad((sk, fk, env, st) -> {
            def fk1(_, _) = h(sk, fk, env, st);
            g(sk, fk1, env, st)
        }))

    pub def throwError(err: err) : ParserMonad[ka, env, st, err, a] =
        ParserMonad((_, fk, _, st) -> fk(err, st))

    pub def catchError(pa: ParserMonad[ka, env, st, err, a], 
                        hf: err -> ParserMonad[ka, env, st, err, a] \ ef):  ParserMonad[ka, env, st, err, a] \ ef =
        let ParserMonad(g) = pa;
        unchecked_cast(ParserMonad((sk, fk, env, st) -> {
            def fk1(err, st1) = {
                let ParserMonad(h) = unchecked_cast(hf(err) as _ \ {});
                h(sk, fk, env, st1)
            };
            g(sk, fk1, env, st)
        }) as _ \ ef)

    pub def mapError(f: err -> err \ ef, pa: ParserMonad[ka, env, st, err, a]): ParserMonad[ka, env, st, err, a] \ ef = 
        let ParserMonad(g) = pa;
        unchecked_cast(ParserMonad((sk, fk, env, st) -> {
            def fk1(err, st2) = fk(unchecked_cast(f(err) as _ \ {}), st2);
            g(sk, fk1, env, st)
        }) as _ \ ef)

    pub def reifyError(pa: ParserMonad[ka, env, st, err, a]): ParserMonad[ka, env, st, err, Result[err, a]] = 
        let ParserMonad(g) = pa;
        ParserMonad((sk, fk, env, st) -> {
            let fk2 = (err, st2) -> sk(Err(err), fk, env, st2);
            let sk1 = (x, fk1, env1, st1) -> sk(Ok(x), fk1, env1, st1); 
            g(sk1, fk2, env, st)
        })


    pub def flatMapOr(pa: ParserMonad[ka, env, st, err, a], 
                        pnext: a -> ParserMonad[ka, env, st, err, b] \ ef1, 
                        pelse: err -> ParserMonad[ka, env, st, err, b] \ ef2): ParserMonad[ka, env, st, err, b] \ ef1 + ef2 =
        let ParserMonad(g) = pa;
        unchecked_cast(ParserMonad((sk, fk, env, st) -> {
            def fk2(err, st2) = {
                let ParserMonad(elsef) = unchecked_cast(pelse(err) as _ \ {});
                elsef(sk, fk, env, st2)
            }; 
            def sk1(x, fk1, env1, st1) = {
                let ParserMonad(nextf) = unchecked_cast(pnext(x) as _ \ {});
                nextf(sk, fk1, env1, st1)
            };
            g(sk1, fk2, env, st)
        }) as _ \ ef1 + ef2)


    // Restores state updated by first ParserMonad (Okasaki)
    pub def lookahead(pa: ParserMonad[ka, env, st, err, a], 
                        k: a -> ParserMonad[ka, env, st, err, b] \ ef): ParserMonad[ka, env, st, err, b] \ ef = 
        let ParserMonad(g) = pa;
        unchecked_cast(ParserMonad((sk, fk, env, st) -> {
            let sk1 = (x, fk1, env1, _) -> { 
                let ParserMonad(h) = unchecked_cast(k(x) as _ \ {});
                h(sk, fk1, env1, st)
            };
            g(sk1, fk, env, st)
        }) as _ \ ef)

    // restore state on failure (Leijen)
    pub def tryParse(pa: ParserMonad[ka, env, st, err, a]): ParserMonad[ka, env, st, err, a] =  
        let ParserMonad(g) = pa;
        ParserMonad((sk, fk, env, st) -> {
            def fk1(err, _) = fk(err, st);
            g(sk, fk1, env, st)
        })


    pub def ask(): ParserMonad[ka, env, st, err, env] = 
        ParserMonad((sk, fk, env, st) -> sk(env, fk, env, st))


    pub def asks(f: env -> a \ ef): ParserMonad[ka, env, st, err, a] \ ef = 
        unchecked_cast(ParserMonad((sk, fk, env, st) -> 
            sk(unchecked_cast(f(env) as _ \ {}), fk, env, st)
        ) as _ \ ef)

    pub def local(f: env -> env \ef, pa: ParserMonad[ka, env, st, err, a]): ParserMonad[ka, env, st, err, a] \ ef =
        let ParserMonad(g) = pa;
        unchecked_cast(ParserMonad((sk, fk, env, st) -> {
            def sk1(x, fk1, _, st1) = sk(x, fk1, env, st1);
            g(sk1, fk, unchecked_cast(f(env) as _ \ {}), st)
        }) as _ \ ef)

    pub def get(): ParserMonad[ka, env, st, err, st] = 
        ParserMonad((sk, fk, env, st) -> sk(st, fk, env, st))

    pub def gets(f: st -> a \ ef): ParserMonad[ka, env, st, err, a] \ ef = 
        unchecked_cast(ParserMonad((sk, fk, env, st) -> 
            sk(unchecked_cast(f(st) as _ \ {}), fk, env, st)
        ) as _ \ ef)

    pub def put(st: st): ParserMonad[ka, env, st, err, Unit] = 
        ParserMonad((sk, fk, env, _) -> sk((), fk, env, st))

    pub def puts(f: st -> st \ ef): ParserMonad[ka, env, st, err, Unit] \ ef = 
        unchecked_cast(ParserMonad((sk, fk, env, st) -> 
            unchecked_cast(sk((), fk, env, f(st)) as _ \ {})
        ) as _ \ ef)


}

