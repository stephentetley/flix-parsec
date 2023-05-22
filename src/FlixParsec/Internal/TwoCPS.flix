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



mod FlixParsec.Internal.TwoCPS {

    use FlixParsec.Internal.TwoCPS.PM2.{PM2}

    type alias FailK[ka: Type, st: Type, err: Type, ef: Eff] = st -> err -> ka \ ef

    type alias SuccessK[ka: Type, env: Type, st: Type, err: Type, ef: Eff, a: Type] = a -> FailK[ka, st, err, ef] -> env -> st -> ka \ ef

    pub enum PM2[ka: Type, env: Type, st: Type, err: Type, ef: Eff, a: Type](
        SuccessK[ka, env, st, err, ef, a] -> FailK[ka, st, err, ef] -> env -> st -> ka \ ef
    )


    def apply1 (p: PM2[ka, env, st, err, ef, a], 
                sk: SuccessK[ka, env, st, err, ef, a], 
                fk: FailK[ka, st, err, ef], 
                env: env, 
                st: st): ka \ ef =
        let PM2(pf) = p;
        pf(sk, fk, env, st)


    pub def runPM2(p: PM2[Result[(st, err), (st, a)], env, st, err, ef, a], st: st, env: env) : Result[(st, err), (st, a)] \ ef =
        let fk = (st1, err) -> checked_ecast(Err((st1, err)));
        let sk = (a, _, _, st1) -> checked_ecast(Ok((st1, a)));
        let PM2(p1) = p;
        p1(sk, fk, env, st)


    pub def point(x: a): PM2[ka, env, st, err, ef, a] = 
        PM2((sk, fk, env, st) -> sk(x, fk, env, st))

    pub def map(f: a -> b \ ef1, p: PM2[ka, env, st, err, ef, a]): PM2[ka, env, st, err, ef + ef1, b] =
        PM2((sk, fk, env, st) ->
            let sk1 = (x, fk1, _, st1) -> sk(f(x), fk1, env, st1);
            apply1(unchecked_cast(p as PM2[ka, env, st, err, ef + ef1, b]), 
                    unchecked_cast(sk1 as SuccessK[ka, env, st, err, ef + ef1, b]), 
                    unchecked_cast(fk as FailK[ka, st, err, ef + ef1]), 
                    env, 
                    st)
        )


    pub def throwError(err: err) : PM2[ka, env, st, err, ef, a] =
        PM2((_, fk, _, st) -> fk(st, err))

}
