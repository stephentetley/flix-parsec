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



    type alias FailK[ka: Type, st: Type, err: Type, _ef: Eff] = err -> st -> ka \ IO

    type alias SuccessK[ka: Type, env: Type, st: Type, err: Type, ef: Eff, a: Type] = a -> FailK[ka, st, err, ef] -> env -> st -> ka \ IO

    pub enum ParserMonad[ka: Type, env: Type, st: Type, err: Type, ef: Eff, a: Type](
        SuccessK[ka, env, st, err, ef, a] -> FailK[ka, st, err, ef] -> env -> st -> ka \ IO
    )

}

mod FlixParsec.Internal.ParserMonad {

    use FlixParsec.Internal.ParserMonad.{ParserMonad}

    pub def runParserMonad(p: ParserMonad[Result[(st, err), (st, a)], env, st, err, ef, a], 
                            env: env, 
                            st: st): Result[(st, err), (st, a)] \ ef =
        let fk = (err, st2) -> checked_ecast(Err((st2, err)));
        let sk = (a, _, _, st1) -> checked_ecast(Ok((st1, a)));
        let ParserMonad(p1) = p;
        unchecked_cast(p1(sk, fk, env, st) as _ \ ef)


    pub def point(x: a): ParserMonad[ka, env, st, err, ef, a] = 
        ParserMonad((sk, fk, env, st) -> sk(x, fk, env, st))


    pub def map(f: a -> b \ ef1, ma: ParserMonad[ka, env, st, err, ef, a]): ParserMonad[ka, env, st, err, ef + ef1, b] =
        let ParserMonad(g) = ma;
        def run(sk, fk, env, st) = {
            let sk1 = (x, fk1, env1, st1) -> sk(f(x), fk1, env1, st1);
            g(sk1, fk, env, st)
        };
        ParserMonad(run)


    pub def ap(pf: ParserMonad[ka, env, st, err, ef1, a -> b \ ef2], 
                pa: ParserMonad[ka, env, st, err, ef, a]): ParserMonad[ka, env, st, err, ef + ef1 + ef2, b] =
        let ParserMonad(g) = pf;
        let ParserMonad(h) = pa;
        def run(sk, fk, env, st) = {
            let sk1 = (f, fk1, env1, st1) -> {
                let sk2 = (x, fk2, env2, st2) -> sk(f(x), fk2, env2, st2);
                h(sk2, fk1, env1, st1)
            };
            g(sk1, fk, env, st)
        };
        ParserMonad(run)


    pub def flatMap(k: a -> ParserMonad[ka, env, st, err, ef1, b] \ ef2, 
                    pa: ParserMonad[ka, env, st, err, ef, a]):  ParserMonad[ka, env, st, err, ef + ef1 + ef2, b] =
        let ParserMonad(g) = pa;
        def run(sk, fk, env, st) = {
            let sk1 = (x, fk1, env1, st1) -> {                
                let ParserMonad(h) = k(x);
                h(sk, fk1, env1, st1)
            };
            g(sk1, fk, env, st)
        };
    ParserMonad(run)

    pub def alt(pa: ParserMonad[ka, env, st, err, ef1, a], pb: ParserMonad[ka, env, st, err, ef2, a]): ParserMonad[ka, env, st, err, ef1 + ef2, a] = 
        let ParserMonad(g) = pa;
        let ParserMonad(h) = pb;
        def run(sk, fk, env, st) = {
            let fk1 = (_, _) -> h(sk, fk, env, st);
            g(sk, fk1, env, st)
        };
        ParserMonad(run)

    pub def throwError(err: err) : ParserMonad[ka, env, st, err, ef, a] =
        def run(_, fk, _, st) = fk(err, st);
        ParserMonad(run)

    pub def catchError(pa: ParserMonad[ka, env, st, err, ef, a], 
                        handler: err -> ParserMonad[ka, env, st, err, ef1, a] \ ef2):  ParserMonad[ka, env, st, err, ef + ef1 + ef2, a] =
        let ParserMonad(g) =pa;
        def run(sk, fk, env, st) = {
            let fk1 = (err, st1) -> {
                let ParserMonad(h) = handler(err);
                h(sk, fk, env, st1)
            };
            g(sk, fk1, env, st)
        };
        ParserMonad(run)

    pub def mapError(f: err -> err \ ef1, pa: ParserMonad[ka, env, st, err, ef, a]): ParserMonad[ka, env, st, err, ef + ef1, a] = 
        let ParserMonad(g) = pa;
        def run(sk, fk, env, st) = {
            let fk1 = (err, st2) -> fk(f(err), st2);
            g(sk, fk1, env, st)
        };
        ParserMonad(run)

    pub def reifyError(pa: ParserMonad[ka, env, st, err, ef, a]): ParserMonad[ka, env, st, err, ef, Result[err, a]] = 
        let ParserMonad(g) = pa;
        def run(sk, fk, env, st) = {
            let fk2 = (err, st2) -> sk(Err(err), fk, env, st2);
            let sk1 = (x, fk1, env1, st1) -> sk(Ok(x), fk1, env1, st1); 
            g(sk1, fk2, env, st)
        };
        ParserMonad(run)


    pub def flatMapOr(pa: ParserMonad[ka, env, st, err, ef, a], 
                        pnext: a -> ParserMonad[ka, env, st, err, ef1, b] \ ef2, 
                        pelse: err -> ParserMonad[ka, env, st, err, ef3, b] \ ef4): ParserMonad[ka, env, st, err, ef + ef1 + ef2 + ef3 + ef4, b] =
        let ParserMonad(g) = pa;
        def run(sk, fk, env, st) = {
            let fk2 = (err, st2) -> {
                let ParserMonad(elsef) = pelse(err);
                elsef(sk, fk, env, st2)
            }; 
            let sk1 = (x, fk1, env1, st1) ->  {
                let ParserMonad(nextf) = pnext(x);
                nextf(sk, fk1, env1, st1)
            };
            g(sk1, fk2, env, st)
        };
        ParserMonad(run)


    // Restores state updated by first ParserMonad (Okasaki)
    pub def lookahead(pa: ParserMonad[ka, env, st, err, ef, a], 
                        k: a -> ParserMonad[ka, env, st, err, ef1, b] \ ef2): ParserMonad[ka, env, st, err, ef + ef1 + ef2, b] = 
        let ParserMonad(g) = pa;
        def run(sk, fk, env, st) = {
            let sk1 = (x, fk1, env1, _) -> { 
                let ParserMonad(h) = k(x);
                h(sk, fk1, env1, st)
            };
            g(sk1, fk, env, st)
        };
        ParserMonad(run)

    // restore state on failure (Leijen)
    pub def tryParse(pa: ParserMonad[ka, env, st, err, ef, a]): ParserMonad[ka, env, st, err, ef, a] =  
        let ParserMonad(g) = pa;
        def run(sk, fk, env, st) = {
            let fk1 = (err, _) -> fk(err, st);
            g(sk, fk1, env, st)
        };
        ParserMonad(run)


    pub def ask(): ParserMonad[ka, env, st, err, ef, env] = 
        def run(sk, fk, env, st) = sk(env, fk, env, st);
        ParserMonad(run)


    pub def asks(f: env -> a \ ef): ParserMonad[ka, env, st, err, ef, a] = 
        def run (sk, fk, env, st) = sk(f(env), fk, env, st);
        ParserMonad(run)

    pub def local(f: env -> env \ef1, pa: ParserMonad[ka, env, st, err, ef, a]): ParserMonad[ka, env, st, err, ef + ef1, a] =
        let ParserMonad(g) = pa;
        def run (sk, fk, env, st) = {
            let sk1 = (x, fk1, _, st1) -> sk(x, fk1, env, st1);     /// restore original env
            g(sk1, fk, f(env), st)
        };
        ParserMonad(run)

    pub def get(): ParserMonad[ka, env, st, err, ef, st] = 
        def run(sk, fk, env, st) = sk(st, fk, env, st);
        ParserMonad(run)

    pub def gets(f: st -> a \ ef): ParserMonad[ka, env, st, err, ef, a] = 
        def run(sk, fk, env, st) = sk(f(st), fk, env, st);
        ParserMonad(run)

    pub def put(st: st): ParserMonad[ka, env, st, err, ef, Unit] = 
        def run(sk, fk, env, _) = sk((), fk, env, st);
        ParserMonad(run)

    pub def puts(f: st -> st \ ef): ParserMonad[ka, env, st, err, ef, Unit] = 
        def run (sk, fk, env, st) = sk((), fk, env, f(st));
        ParserMonad(run)

    pub def liftAction(f: Unit -> a \ ef): ParserMonad[ka, env, st, err, ef, a] = 
        def run(sk, fk, env, st) = {
            let a = f(); 
            sk(a, fk, env, st)  
        };
        ParserMonad(run)
}
