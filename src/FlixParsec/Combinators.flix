/*
 * Copyright 2020 Stephen Tetley
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


mod FlixParsec.Combinators {

    use GradedFunctor.{map}
    use GradedApplicative.{point, <*, *>}
    use GradedMonad.{>>=}
    use GradedOr.{<|>}
    use GradedError.{flatMapOr}

    use FlixParsec.{GenParser}
    use FlixParsec.Prim.{bind, fail, lookahead}

    
    // pub def <!>(p1: GenParser[ka, st, ef, a], p2: GenParser[ka, st, ef, a]): GenParser[ka, st, ef, a] =
    //     alt(p1, p2)

    

    /// `alt` rolls back the parser state (including the position) if `p` fails.
    pub def alt(p: GenParser[ka, st, ef, a], q: GenParser[ka, st, ef, a]): GenParser[ka, st, ef, a] =
        p <|> q


    pub def notFollowedBy(p: GenParser[ka, st, ef, a]): GenParser[ka, st, ef, Unit] =
        flatMapOr(p, _ -> fail("notFollowedBy"), _ -> point()) 
        

    pub def choice(ps: List[GenParser[ka, st, ef, a]]): GenParser[ka, st, ef, a] =
        match ps {
            case p1 :: rs => flatMapOr(p1, point, _ -> choice(rs))
            case Nil      => fail("choice")
        }
/*
    pub def optional(p: GenParser[ka, st, ef, a]): GenParser[ka, st, ef, Option[a]] = 
        alt(map(Some, p), point(None))

    pub def optionalWithDefault(p: GenParser[ka, st, ef, a], d: a): GenParser[ka, st, ef, a] = 
        alt(p, point(d))
*/


    pub def between(openp: GenParser[ka, st, ef1, open], 
                    closep: GenParser[ka, st, ef2, close], 
                    body: GenParser[ka, st, ef, a]): GenParser[ka, st, ef and ef1 and ef2, a] = 
        (openp *> body) <* closep

    pub def satisfies(p: GenParser[ka, st, ef, a], test: a -> Bool \ ef1): GenParser[ka, st, ef and ef1, a] = 
        p >>= ans ->
        if (test(ans))
            point(ans)
        else
            fail("satisfies")


    /// `p` is run with `lookahead`, no text is consumed
    pub def succeeds(p: GenParser[ka, st, ef, a]): GenParser[ka, st, ef, Bool] = 
        alt(lookahead(p) *> point(true), point(false))


    pub def countWith(combine: (ac, a) -> ac, acc: ac, n: Int32, p: GenParser[ka, st, ef, a]): GenParser[ka, st, ef, ac] = 
        if (n <= 0)
            point(acc)
        else {
            p >>= ans ->
            countWith(combine, combine(acc, ans), n-1, p)
        }

    pub def count(n: Int32, p: GenParser[ka, st, ef, a]): GenParser[ka, st, ef, List[a]] = 
        use Chain.{empty, snoc, toList};
        countWith(snoc, empty(), n, p) |> map(toList)
    


    pub def manyWith(combine: (ac, a) -> ac \ ef1, 
                        acc: ac, 
                        p: GenParser[ka, st, ef, a]): GenParser[ka, st, ef and ef1, ac] = 
        flatMapOr(p, a -> manyWith(combine, combine(acc, a), p), _ -> point(acc))


    pub def many(p: GenParser[ka, st, ef, a]): GenParser[ka, st, ef, List[a]] = 
        use Chain.{empty, snoc, toList};
        manyWith(snoc, empty(), p) |> map(toList)
        
    pub def many1With(combine: (ac, a) -> ac \ ef1, 
                        acc: ac, 
                        p: GenParser[ka, st, ef, a]): GenParser[ka, st, ef and ef1, ac] = 
        bind(p, x -> manyWith(combine, combine(acc, x), p))
        
    pub def many1(p: GenParser[ka, st, ef, a]): GenParser[ka, st, ef, List[a]] = 
        use Chain.{empty, snoc, toList};
        many1With(snoc, empty(), p) |> map(toList)


    /// Count how many times parser `p` succeeds
    pub def ntimes(p: GenParser[ka, st, ef, a]): GenParser[ka, st, ef, Int32] = 
        manyWith((ac, _) -> ac + 1, 0, p)


    pub def manyTillWith(combine: (ac, a) -> ac \ ef2, 
                            acc: ac, 
                            p: GenParser[ka, st, ef, a], 
                            endp: GenParser[ka, st, ef1, end]): GenParser[ka, st, ef and ef1 and ef2, ac] = 
        flatMapOr(endp, _ -> point(acc), _ -> bind(p, a -> manyTillWith(combine, combine(acc, a), p, endp)))


    pub def manyTill(p: GenParser[ka, st, ef, a], endp: GenParser[ka, st, ef1, end]): GenParser[ka, st, ef and ef1, List[a]] = 
        use Chain.{empty, snoc, toList};
        manyTillWith(snoc, empty(), p, endp) |> map(toList)


    pub def many1TillWith(combine: (ac, a) -> ac \ ef2, 
                            acc: ac, 
                            p: GenParser[ka, st, ef, a], 
                            endp: GenParser[ka, st, ef1, end]): GenParser[ka, st, ef and ef1 and ef2, ac] = 
        bind(p, x -> manyTillWith(combine, combine(acc, x), p, endp))

    pub def many1Till(p: GenParser[ka, st, ef, a], endp: GenParser[ka, st, ef1, end]): GenParser[ka, st, ef and ef1, List[a]] = 
        use Chain.{empty, snoc, toList};
        many1TillWith(snoc, empty(), p, endp) |> map(toList)
    

    pub def sepByWith(combine: (ac, a) -> ac \ ef2, 
                        acc: ac, 
                        p: GenParser[ka, st, ef, a], 
                        sep: GenParser[ka, st, ef1, sep]): GenParser[ka, st, ef and ef1 and ef2, ac] = 
        flatMapOr(p, x -> sepByWithHelper(combine, combine(acc, x), p, sep), _ -> point(acc))
    
    // Initial p has been consumed
    def sepByWithHelper(combine: (ac, a) -> ac \ ef2, 
                        acc: ac, 
                        p: GenParser[ka, st, ef, a], 
                        sep: GenParser[ka, st, ef1, sep]): GenParser[ka, st, ef and ef1 and ef2, ac] = 
        flatMapOr((sep *> p), x -> sepByWithHelper(combine, combine(acc, x), p, sep), _ -> point(acc))

    pub def sepBy(p: GenParser[ka, st, ef, a], sep: GenParser[ka, st, ef1, sep]): GenParser[ka, st, ef and ef1, List[a]] = 
        use Chain.{empty, snoc, toList};
        sepByWith(snoc, empty(), p, sep) |> map(toList)


    pub def sepBy1With(combine: (ac, a) -> ac \ ef2, 
                        acc: ac, 
                        p: GenParser[ka, st, ef, a], 
                        sep: GenParser[ka, st, ef1, sep]): GenParser[ka, st, ef and ef1 and ef2, ac] = 
        bind(p, x -> sepByWithHelper(combine, combine(acc, x), p, sep))

    pub def sepBy1(p: GenParser[ka, st, ef, a], sep: GenParser[ka, st, ef1, sep]): GenParser[ka, st, ef and ef1, List[a]] = 
        use Chain.{empty, snoc, toList};
        sepBy1With(snoc, empty(), p, sep) |> map(toList)

    pub def endByWith(combine: (ac, a) -> ac \ ef2, 
                        acc: ac, 
                        p: GenParser[ka, st, ef, a], 
                        sep: GenParser[ka, st, ef1, sep]): GenParser[ka, st, ef and ef1 and ef2, ac] = 
        manyWith(combine, acc, (p <* sep))

    pub def endBy(p: GenParser[ka, st, ef, a], sep: GenParser[ka, st, ef1, sep]): GenParser[ka, st, ef and ef1, List[a]] = 
        use Chain.{empty, snoc, toList};
        endByWith(snoc, empty(), p, sep) |> map(toList)

    pub def endBy1With(combine: (ac, a) -> ac \ ef2, 
                        acc: ac, 
                        p: GenParser[ka, st, ef, a], 
                        sep: GenParser[ka, st, ef1, sep]): GenParser[ka, st, ef and ef1 and ef2, ac] = 
        bind(p <* sep, x -> endByWith(combine, combine(acc, x), p, sep))

    pub def endBy1(p: GenParser[ka, st, ef, a], sep: GenParser[ka, st, ef1, sep]): GenParser[ka, st, ef and ef1, List[a]] = 
        use Chain.{empty, snoc, toList};
        endBy1With(snoc, empty(), p, sep) |> map(toList)


    // seperated and optionally terminated with sep.
    pub def sepEndByWith(combine: (ac, a) -> ac \ ef2, 
                            acc: ac, 
                            p: GenParser[ka, st, ef, a], 
                            sep: GenParser[ka, st, ef1, sep]): GenParser[ka, st, ef and ef1 and ef2, ac] = 
        sepByWith(combine, acc, p, sep) >>= xs ->
        flatMapOr(sep, _ -> point(xs), _ -> point(xs))

    // seperated and optionally terminated with sep.
    pub def sepEndBy(p: GenParser[ka, st, ef, a], sep: GenParser[ka, st, ef1, sep]): GenParser[ka, st, ef and ef1, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, sepEndByWith(snoc, empty(), p, sep))

    // seperated and optionally terminated with sep.
    pub def sepEndBy1With(combine: (ac, a) -> ac \ ef2, 
                            acc: ac, 
                            p: GenParser[ka, st, ef, a], 
                            sep: GenParser[ka, st, ef1, sep]): GenParser[ka, st, ef and ef1 and ef2, ac] = 
        sepBy1With(combine, acc, p, sep)    >>= xs ->
        flatMapOr(sep, _ -> point(xs), _ -> point(xs))

    // seperated and optionally terminated with sep.
    pub def sepEndBy1(p: GenParser[ka, st, ef, a], sep: GenParser[ka, st, ef1, sep]): GenParser[ka, st, ef and ef1, List[a]] = 
        use Chain.{empty, snoc, toList};
        sepEndBy1With(snoc, empty(), p, sep) |> map(toList)

    /// Count how many times parser `p` succeeds
    pub def ntimesSepBy(p: GenParser[ka, st, ef, a], 
                        sep: GenParser[ka, st, ef1, sep]): GenParser[ka, st, ef and ef1, Int32] = 
        sepByWith((ac, _) -> ac + 1, 0, p, sep)

    /// Count how many times parser `p` succeeds
    pub def ntimesEndBy(p: GenParser[ka, st, ef, a], 
                        sep: GenParser[ka, st, ef1, sep]): GenParser[ka, st, ef and ef1, Int32] = 
        endByWith((ac, _) -> ac + 1, 0, p, sep)

    /// Count how many times parser `p` succeeds
    pub def ntimesSepEndBy(p: GenParser[ka, st, ef, a], 
                            sep: GenParser[ka, st, ef1, sep]): GenParser[ka, st, ef and ef1, Int32] = 
        sepEndByWith((ac, _) -> ac + 1, 0, p, sep)

    /// p1 must succeed, p2 parsers zero-or-more
    pub def manyTill2With(combine: (ac, a) -> ac \ ef3, 
                            acc: ac, 
                            p1: GenParser[ka, st, ef, a], 
                            p2: GenParser[ka, st, ef1, a], 
                            endp: GenParser[ka, st, ef2, end]): GenParser[ka, st, ef and ef1 and ef2 and ef3, ac] = 
        bind(p1, x -> manyTillWith(combine, combine(acc, x), p2, endp))
        
    /// p1 must succeed, p2 parsers zero-or-more
    pub def manyTill2(p1: GenParser[ka, st, ef, a], 
                      p2: GenParser[ka, st, ef1, a], 
                      endp: GenParser[ka, st, ef2, end]): GenParser[ka, st, ef and ef1 and ef2, List[a]] = 
        use Chain.{empty, snoc, toList};
        manyTill2With(snoc, empty(), p1, p2, endp) |> map(toList)

    /// p1 must succeed, p2 parsers one-or-more
    pub def many1Till2With(combine: (ac, a) -> ac \ ef3, 
                            acc: ac, 
                            p1: GenParser[ka, st, ef, a], 
                            p2: GenParser[ka, st, ef1, a], 
                            endp: GenParser[ka, st, ef2, end]): GenParser[ka, st, ef and ef1 and ef2 and ef3, ac] = 
        bind(p1, x -> many1TillWith(combine, combine(acc, x), p2, endp))
        
    /// p1 must succeed, p2 parsers one-or-more
    pub def many1Till2(p1: GenParser[ka, st, ef, a], 
                        p2: GenParser[ka, st, ef1, a], 
                        endp: GenParser[ka, st, ef2, end]): GenParser[ka, st, ef and ef1 and ef2, List[a]] = 
        use Chain.{empty, snoc, toList};
        many1Till2With(snoc, empty(), p1, p2, endp) |> map(toList)



    pub def skipMany(p: GenParser[ka, st, ef, a]): GenParser[ka, st, ef, Unit] = 
        flatMapOr(p, _ -> skipMany(p), _ -> point())

    pub def skipMany1(p: GenParser[ka, st, ef, a]): GenParser[ka, st, ef, Unit] = 
        p *> skipMany(p)


    pub def skipManyTill(p: GenParser[ka, st, ef, a], 
                            endp: GenParser[ka, st, ef1, end]): GenParser[ka, st, ef and ef1, Unit] = 
        flatMapOr(endp, _ -> point(), _ -> (p *> skipManyTill(p, endp)))

    pub def skipMany1Till(p: GenParser[ka, st, ef, a], 
                            endp: GenParser[ka, st, ef1, end]): GenParser[ka, st, ef and ef1, Unit] = 
        p *> skipManyTill(p, endp)

    /// Chain parsers

    pub def chainl1(p: GenParser[ka, st, ef, a], 
                    op: GenParser[ka, st, ef1, (a, a) -> a \ ef2]): GenParser[ka, st, ef and ef1 and ef2, a] = 
        bind(p, v1 -> chainl1Helper(p, op, v1))
        
    def chainl1Helper(p: GenParser[ka, st, ef, a], 
                        op: GenParser[ka, st, ef1, (a, a) -> a \ ef2], x: a): GenParser[ka, st, ef and ef1 and ef2, a] = 
        alt(bind(op, f -> bind(p, v1 -> chainl1Helper(p, op, f(x, v1)))), 
            point(x))

    pub def chainl(p: GenParser[ka, st, ef, a], 
                    op: GenParser[ka, st, ef1, (a, a) -> a \ ef2], x: a): GenParser[ka, st, ef and ef1 and ef2, a] = 
        alt(chainl1(p,op), point(x))


    def chainr1Helper(p: GenParser[ka, st, ef, a], 
                        op: GenParser[ka, st, ef1, (a, a) -> a \ ef2], x: a): GenParser[ka, st, ef and ef1 and ef2, a] = 
        alt(chainr1Step(p, op, x), point(x))

    def chainr1Step(p: GenParser[ka, st, ef, a], 
                        op: GenParser[ka, st, ef1, (a, a) -> a \ ef2], x: a): GenParser[ka, st, ef and ef1 and ef2, a] = 
        bind(op, f -> 
            bind(bind(p, v1 -> chainr1Helper(p, op, v1)), y -> 
                bind(chainr1Helper(p,op, f(x,y)), ans -> point(ans))))

    pub def chainr1(p: GenParser[ka, st, ef, a], 
                    op: GenParser[ka, st, ef1, (a, a) -> a \ ef2]): GenParser[ka, st, ef and ef1 and ef2, a] = 
        bind(p, v1 -> chainr1Helper(p, op, v1))

    pub def chainr(p: GenParser[ka, st, ef, a], 
                    op: GenParser[ka, st, ef1, (a, a) -> a \ ef2], x: a): GenParser[ka, st, ef and ef1 and ef2, a] = 
        alt(chainr1(p, op), point(x))


}
