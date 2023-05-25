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

    use FlixParsec.Internal.TwoCPS.Parser.{Parser}

    type alias FailK[ka: Type, st: Type, err: Type, ef: Eff] = err -> st -> ka \ ef

    type alias SuccessK[ka: Type, env: Type, st: Type, err: Type, ef: Eff, a: Type] = a -> FailK[ka, st, err, ef] -> env -> st -> ka \ ef

    pub enum Parser[ka: Type, env: Type, st: Type, err: Type, ef: Eff, a: Type](
        SuccessK[ka, env, st, err, ef, a] -> FailK[ka, st, err, ef] -> env -> st -> ka \ ef
    )


    pub def runParser(p: Parser[Result[(st, err), (st, a)], env, st, err, ef, a], 
                        st: st, 
                        env: env): Result[(st, err), (st, a)] \ ef =
        let fk = (err, st2) -> checked_ecast(Err((st2, err)));
        let sk = (a, _, _, st1) -> checked_ecast(Ok((st1, a)));
        let Parser(p1) = p;
        p1(sk, fk, env, st)


    pub def point(x: a): Parser[ka, env, st, err, ef, a] = 
        Parser((sk, fk, env, st) -> sk(x, fk, env, st))


    pub def map(f: a -> b \ ef1, pa: Parser[ka, env, st, err, ef, a]): Parser[ka, env, st, err, ef + ef1, b] =
        Parser((sk, fk, env, st) -> {
            let Parser(paf) = unchecked_cast(pa as Parser[ka, env, st, err, ef + ef1, a]);
            let sk1 = (x, fk1, env1, st1) -> sk(f(x), fk1, env1, st1);
            paf(sk1, fk, env, st)
        })


    pub def ap(pf: Parser[ka, env, st, err, ef1, a -> b \ ef2], 
                pa: Parser[ka, env, st, err, ef, a]): Parser[ka, env, st, err, ef + ef1 + ef2, b] =
        Parser((sk, fk, env, st) -> { 
            let Parser(pff) = unchecked_cast(pf as Parser[ka, env, st, err, ef + ef1 + ef2, a -> b \ ef2]);
            let Parser(paf) = unchecked_cast(pa as Parser[ka, env, st, err, ef + ef1 + ef2, a]);
            let sk1 = (f, fk1, env1, st1) -> {
                let sk2 = (x, fk2, env2, st2) -> sk(f(x), fk2, env2, st2);
                paf(sk2, fk1, env1, st1)
            };
            pff(sk1, fk, env, st)
        })


    pub def flatMap(k: a -> Parser[ka, env, st, err, ef1, b] \ ef2, 
                    pa: Parser[ka, env, st, err, ef, a]):  Parser[ka, env, st, err, ef + ef1 + ef2, b] =
        Parser((sk, fk, env, st) ->
            let Parser(paf) = unchecked_cast(pa as Parser[ka, env, st, err, ef + ef1 + ef2, a]);
            let sk1 = (x, fk1, env1, st1) -> {                
                let Parser(kf) = unchecked_cast(k(x) as Parser[ka, env, st, err, ef + ef1 + ef2, b]);
                kf(sk, fk1, env1, st1)
            };
            paf(sk1, fk, env, st)
        )

    pub def alt(pa: Parser[ka, env, st, err, ef1, a], pb: Parser[ka, env, st, err, ef2, a]): Parser[ka, env, st, err, ef1 + ef2, a] = 
        Parser((sk, fk, env, st) ->        
            let Parser(paf) = unchecked_cast(pa as Parser[ka, env, st, err, ef1 + ef2, a]);
            let Parser(pbf) = unchecked_cast(pb as Parser[ka, env, st, err, ef1 + ef2, a]);
            let fk1 = (_, _) -> pbf(sk, fk, env, st);
            paf(sk, fk1, env, st)
        )

    pub def throwError(err: err) : Parser[ka, env, st, err, ef, a] =
        Parser((_, fk, _, st) -> fk(err, st))

    pub def catchError(pa: Parser[ka, env, st, err, ef, a], 
                        handler: err -> Parser[ka, env, st, err, ef1, a] \ ef2):  Parser[ka, env, st, err, ef + ef1 + ef2, a] =
        Parser((sk, fk, env, st) ->
            let Parser(paf) = unchecked_cast(pa as Parser[ka, env, st, err, ef + ef1 + ef2, a]);
            let fk1 = (err, st1) -> {
                let Parser(kf) = unchecked_cast(handler(err) as Parser[ka, env, st, err, ef + ef1 + ef2, a]);
                kf(sk, fk, env, st1)
            };
            paf(sk, fk1, env, st)
        )

    pub def mapError(f: err -> err \ ef1, pa: Parser[ka, env, st, err, ef, a]): Parser[ka, env, st, err, ef + ef1, a] = 
        Parser((sk, fk, env, st) ->
            let Parser(paf) = unchecked_cast(pa as Parser[ka, env, st, err, ef + ef1, a]);
            let fk1 = (err, st2) -> fk(f(err), st2);
            paf(sk, fk1, env, st)
        )

    pub def reifyError(pa: Parser[ka, env, st, err, ef, a]): Parser[ka, env, st, err, ef, Result[err, a]] = 
        Parser((sk, fk, env, st) ->
            let Parser(paf) = pa;
            let fk2 = (err, st2) -> sk(Err(err), fk, env, st2);
            let sk1 = (x, fk1, env1, st1) -> sk(Ok(x), fk1, env1, st1); 
            paf(sk1, fk2, env, st)
        )


    pub def flatMapOr(pa: Parser[ka, env, st, err, ef, a], 
                        pnext: a -> Parser[ka, env, st, err, ef1, b] \ ef2, 
                        pelse: err -> Parser[ka, env, st, err, ef3, b] \ ef4): Parser[ka, env, st, err, ef + ef1 + ef2 + ef3 + ef4, b] =
        Parser((sk, fk, env, st) ->
            let Parser(paf) = unchecked_cast(pa as Parser[ka, env, st, err, ef + ef1 + ef2 + ef3 + ef4, a]);
            let fk2 = (err, st2) -> {
                let Parser(elsef) = unchecked_cast(pelse(err) as Parser[ka, env, st, err, ef + ef1 + ef2 + ef3 + ef4, b]);
                elsef(sk, fk, env, st2)
            }; 
            let sk1 = (x, fk1, env1, st1) ->  {
                let Parser(nextf) = unchecked_cast(pnext(x) as Parser[ka, env, st, err, ef + ef1 + ef2 + ef3 + ef4, b]);
                nextf(sk, fk1, env1, st1)
            };
            paf(sk1, fk2, env, st)
        )


}
