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


namespace FlixParsec/Combinators {

    use FlixParsec.{GenParser};
    use FlixParsec/Prim.{return, fail, failure, map, ap, alt, bind, 
        ifThenElse, lookahead, seqLeft, seqRight};

    /// Alias for `seqRight`
    pub def *>(p1: GenParser[t1, u, a], p2: GenParser[t1, u, b]): GenParser[t1, u, b] = seqRight(p1, p2)

    /// Alias for `seqLeft`
    pub def <*(p1: GenParser[t1, u, a], p2: GenParser[t1, u, b]): GenParser[t1, u, a] = seqLeft(p1, p2)

    pub def <!>(p1: GenParser[t1, u, a], p2: GenParser[t1, u, a]): GenParser[t1, u, a] =
        alt(p1, p2)

    // Haskell's <$>
    pub def <&>(f: a -> b, p2: GenParser[t1, u, a]): GenParser[t1, u, b] =
        map(f, p2)

    pub def <&(x: b, p2: GenParser[t1, u, a]): GenParser[t1, u, b] =
        p2 *> return(x)


    // Haskell's ap <*>
    pub def <*>(mf: GenParser[t1, u, a -> b], p2: GenParser[t1, u, a]): GenParser[t1, u, b] =
        ap(mf, p2)


    pub def optional(p: GenParser[t1, u, a]): GenParser[t1, u, Option[a]] = 
        alt(map(Some,p), return(None))

    pub def optionalWithDefault(p: GenParser[t1, u, a], d: a): GenParser[t1, u, a] = 
        alt(p, return(d))



    pub def ignore(p: GenParser[t1, u, a]): GenParser[t1, u, Unit] =
        map(constant(), p)


    pub def between(openp: GenParser[t1, u, open], closep: GenParser[t1, u, close], p: GenParser[t1, u, a]): GenParser[t1, u, a] = 
        use FlixParsec/Prim.{flatMap};
        let* _ = openp;
        let* a = p;
        let* _ = closep;
        return(a)

    pub def satisfies(p: GenParser[t1, u, a], test: a -> Bool): GenParser[t1, u, a] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p;
        if (test(a))
            return(a)
        else
            failure("satisfies")
            
    /// `p` is run with `lookahead`, no text is consumed
    pub def succeeds(p: GenParser[t1, u, a]): GenParser[t1, u, Bool] = 
        alt(seqRight(lookahead(p), return(true)), return(false))


    pub def countWith(append: (a, ac) -> ac, acc: ac, n: Int32, p: GenParser[t1, u, a]): GenParser[t1, u, ac] = 
        use FlixParsec/Prim.{flatMap};
        if (n <= 0)
            return(acc)
        else {
            let* a = p;
            countWith(append, append(a, acc), n-1, p)
        }

    pub def count(n: Int32, p: GenParser[t1, u, a]): GenParser[t1, u, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, countWith((x, xs) -> snoc(xs, x), empty(), n, p))
    


    pub def manyWith(append: (a, ac) -> ac, acc: ac, p: GenParser[t1, u, a]): GenParser[t1, u, ac] = 
        ifThenElse(p, a -> manyWith(append, append(a, acc), p), return(acc))


    pub def many(p: GenParser[t1, u, a]): GenParser[t1, u, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, manyWith((x, xs) -> snoc(xs, x), empty(), p))
        
    pub def many1With(append: (a, ac) -> ac, acc: ac, p: GenParser[t1, u, a]): GenParser[t1, u, ac] = 
        bind(p, x -> manyWith(append, append(x, acc), p))
        
    pub def many1(p: GenParser[t1, u, a]): GenParser[t1, u, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, many1With((x, xs) -> snoc(xs, x), empty(), p))


    /// Count how many times parser `p` succeeds
    pub def ntimes(p: GenParser[t1, u, a]): GenParser[t1, u, Int32] = 
        manyWith((_,ac) -> ac + 1, 0, p)


    pub def manyTillWith(append: (a, ac) -> ac, 
                            acc: ac, 
                            p: GenParser[t1, u, a], 
                            endp: GenParser[t1, u, end]): GenParser[t1, u, ac] = 
        ifThenElse(endp, _ -> return(acc), bind(p, a -> manyTillWith(append, append(a, acc), p, endp)))

    pub def manyTill(p: GenParser[t1, u, a], endp: GenParser[t1, u, end]): GenParser[t1, u, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, manyTillWith((x, xs) -> snoc(xs, x), empty(), p, endp))
        
    pub def many1TillWith(append: (a, ac) -> ac, 
                            acc: ac, 
                            p: GenParser[t1, u, a], 
                            endp: GenParser[t1, u, end]): GenParser[t1, u, ac] = 
        bind(p, x -> manyTillWith(append, append(x, acc), p, endp))
        
    pub def many1Till(p: GenParser[t1, u, a], endp: GenParser[t1, u, end]): GenParser[t1, u, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, many1TillWith((x, xs) -> snoc(xs, x), empty(), p, endp))
    

    pub def sepByWith(append: (a, ac) -> ac, 
                        acc: ac, 
                        p: GenParser[t1, u, a], 
                        sep: GenParser[t1, u, sep]): GenParser[t1, u, ac] = 
        ifThenElse(p, x -> sepByWithHelper(append, append(x, acc), p, sep), return(acc))
    
    // Initial p has been consumed
    def sepByWithHelper(append: (a, ac) -> ac, 
                        acc: ac, 
                        p: GenParser[t1, u, a], 
                        sep: GenParser[t1, u, sep]): GenParser[t1, u, ac] = 
        ifThenElse(seqRight(sep, p), x -> sepByWithHelper(append, append(x, acc), p, sep), return(acc))

    pub def sepBy(p: GenParser[t1, u, a], sep: GenParser[t1, u, sep]): GenParser[t1, u, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, sepByWith((x, xs) -> snoc(xs, x), empty(), p, sep))

    pub def sepBy1With(append: (a, ac) -> ac, 
                        acc: ac, 
                        p: GenParser[t1, u, a], 
                        sep: GenParser[t1, u, sep]): GenParser[t1, u, ac] = 
        bind(p, x -> sepByWithHelper(append, append(x, acc), p, sep))
    
    pub def sepBy1(p: GenParser[t1, u, a], sep: GenParser[t1, u, sep]): GenParser[t1, u, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, sepBy1With((x, xs) -> snoc(xs, x), empty(), p, sep))

    pub def endByWith(append: (a, ac) -> ac, 
                        acc: ac, 
                        p: GenParser[t1, u, a], 
                        sep: GenParser[t1, u, sep]): GenParser[t1, u, ac] = 
        manyWith(append, acc, seqLeft(p, sep))

    pub def endBy(p: GenParser[t1, u, a], sep: GenParser[t1, u, sep]): GenParser[t1, u, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, endByWith((x, xs) -> snoc(xs, x), empty(), p, sep))

    pub def endBy1With(append: (a, ac) -> ac, 
                        acc: ac, 
                        p: GenParser[t1, u, a], 
                        sep: GenParser[t1, u, sep]): GenParser[t1, u, ac] = 
        bind(seqLeft(p, sep), x -> endByWith(append, append(x, acc), p, sep))

    pub def endBy1(p: GenParser[t1, u, a], sep: GenParser[t1, u, sep]): GenParser[t1, u, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, endBy1With((x, xs) -> snoc(xs, x), empty(), p, sep))

    // seperated and optionally terminated with sep.
    pub def sepEndByWith(append: (a, ac) -> ac, 
                            acc: ac, 
                            p: GenParser[t1, u, a], 
                            sep: GenParser[t1, u, sep]): GenParser[t1, u, ac] = 
        use FlixParsec/Prim.{flatMap};
        let* xs = sepByWith(append, acc, p, sep);
        ifThenElse(sep, _ -> return(xs), return(xs))

    // seperated and optionally terminated with sep.
    pub def sepEndBy(p: GenParser[t1, u, a], sep: GenParser[t1, u, sep]): GenParser[t1, u, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, sepEndByWith((x, xs) -> snoc(xs, x), empty(), p, sep))

    // seperated and optionally terminated with sep.
    pub def sepEndBy1With(append: (a, ac) -> ac, 
                            acc: ac, 
                            p: GenParser[t1, u, a], 
                            sep: GenParser[t1, u, sep]): GenParser[t1, u, ac] = 
        use FlixParsec/Prim.{flatMap};
        let* xs = sepBy1With(append, acc, p, sep);
        ifThenElse(sep, _ -> return(xs), return(xs))

    // seperated and optionally terminated with sep.
    pub def sepEndBy1(p: GenParser[t1, u, a], sep: GenParser[t1, u, sep]): GenParser[t1, u, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, sepEndBy1With((x, xs) -> snoc(xs, x), empty(), p, sep))

    /// Count how many times parser `p` succeeds
    pub def ntimesSepBy(p: GenParser[t1, u, a], sep: GenParser[t1, u, sep]): GenParser[t1, u, Int32] = 
        sepByWith((_, ac) -> ac + 1, 0, p, sep)

    /// Count how many times parser `p` succeeds
    pub def ntimesEndBy(p: GenParser[t1, u, a], sep: GenParser[t1, u, sep]): GenParser[t1, u, Int32] = 
        endByWith((_, ac) -> ac + 1, 0, p, sep)

    /// Count how many times parser `p` succeeds
    pub def ntimesSepEndBy(p: GenParser[t1, u, a], sep: GenParser[t1, u, sep]): GenParser[t1, u, Int32] = 
        sepEndByWith((_, ac) -> ac + 1, 0, p, sep)

    /// p1 must succeed, p2 parsers zero-or-more
    pub def manyTill2With(append: (a, ac) -> ac, 
                            acc: ac, 
                            p1: GenParser[t1, u, a], 
                            p2: GenParser[t1, u, a], 
                            endp: GenParser[t1, u, end]): GenParser[t1, u, ac] = 
        bind(p1, x -> manyTillWith(append, append(x,acc), p2, endp))
        
    /// p1 must succeed, p2 parsers zero-or-more
    pub def manyTill2(p1: GenParser[t1, u, a], 
                      p2: GenParser[t1, u, a], 
                      endp: GenParser[t1, u, end]): GenParser[t1, u, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, manyTill2With((x, xs) -> snoc(xs, x), empty(), p1, p2, endp))

    /// p1 must succeed, p2 parsers one-or-more
    pub def many1Till2With(append: (a, ac) -> ac, acc: ac, 
                            p1: GenParser[t1, u, a], 
                            p2: GenParser[t1, u, a], 
                            endp: GenParser[t1, u, end]): GenParser[t1, u, ac] = 
        bind(p1, x -> many1TillWith(append, append(x,acc), p2, endp))
        
    /// p1 must succeed, p2 parsers one-or-more
    pub def many1Till2(p1: GenParser[t1, u, a], 
                        p2: GenParser[t1, u, a], 
                        endp: GenParser[t1, u, end]): GenParser[t1, u, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, many1Till2With((x, xs) -> snoc(xs, x), empty(), p1, p2, endp))



    pub def skipMany(p: GenParser[t1, u, a]): GenParser[t1, u, Unit] = 
        ifThenElse(p, _ -> skipMany(p), return())

    pub def skipMany1(p: GenParser[t1, u, a]): GenParser[t1, u, Unit] = 
        bind(p, _ -> skipMany(p))


    pub def skipManyTill(p: GenParser[t1, u, a], endp: GenParser[t1, u, end]): GenParser[t1, u, Unit] = 
        ifThenElse(endp, _ -> return(), bind(p, _ -> skipManyTill(p, endp)))

    pub def skipMany1Till(p: GenParser[t1, u, a], endp: GenParser[t1, u, end]): GenParser[t1, u, Unit] = 
        bind(p, _ -> skipManyTill(p, endp))


    pub def chainl1(p: GenParser[t1, u, a], op: GenParser[t1, u, (a, a) -> a]): GenParser[t1, u, a] = 
        bind(p, v1 -> chainl1Helper(p, op, v1))
        
    def chainl1Helper(p: GenParser[t1, u, a], op: GenParser[t1, u, (a, a) -> a], x: a): GenParser[t1, u, a] = 
        alt(bind(op, f -> bind(p, v1 -> chainl1Helper(p, op, f(x, v1)))), 
            return(x))

    pub def chainl(p: GenParser[t1, u, a], op: GenParser[t1, u, (a, a) -> a], x: a): GenParser[t1, u, a] = 
        alt(chainl1(p,op), return(x))


    def chainr1Helper(p: GenParser[t1, u, a], op: GenParser[t1, u, (a, a) -> a], x: a): GenParser[t1, u, a] = 
        alt(chainr1Step(p, op, x), return(x))

    def chainr1Step(p: GenParser[t1, u, a], op: GenParser[t1, u, (a, a) -> a], x: a): GenParser[t1, u, a] = 
        bind(op, f -> 
            bind(bind(p, v1 -> chainr1Helper(p, op, v1)), y -> 
                bind(chainr1Helper(p,op, f(x,y)), ans -> return(ans))))

    pub def chainr1(p: GenParser[t1, u, a], op: GenParser[t1, u, (a, a) -> a]): GenParser[t1, u, a] = 
        bind(p, v1 -> chainr1Helper(p, op, v1))

    pub def chainr(p: GenParser[t1, u, a], op: GenParser[t1, u, (a, a) -> a], x: a): GenParser[t1, u, a] = 
        alt(chainr1(p, op), return(x))

    pub def tuple2(p1: GenParser[t1, u, a], p2: GenParser[t1, u, b]): GenParser[t1, u, (a, b)] =
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        return((a, b))

    pub def tuple3(p1: GenParser[t1, u, a], 
                    p2: GenParser[t1, u, b], 
                    p3: GenParser[t1, u, c]): GenParser[t1, u, (a, b, c)] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        return((a, b, c))

    pub def tuple4(p1: GenParser[t1, u, a], 
                    p2: GenParser[t1, u, b], 
                    p3: GenParser[t1, u, c], 
                    p4: GenParser[t1, u, d]): GenParser[t1, u, (a, b, c, d)] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        return((a, b, c, d))


    pub def tuple5(p1: GenParser[t1, u, a], 
                    p2: GenParser[t1, u, b], 
                    p3: GenParser[t1, u, c], 
                    p4: GenParser[t1, u, d], 
                    p5: GenParser[t1, u, e]): GenParser[t1, u, (a, b, c, d, e)] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        return((a, b, c, d, e))

    pub def tuple6(p1: GenParser[t1, u, a], 
                    p2: GenParser[t1, u, b], 
                    p3: GenParser[t1, u, c], 
                    p4: GenParser[t1, u, d], 
                    p5: GenParser[t1, u, e], 
                    p6: GenParser[t1, u, f]): 
                    GenParser[t1, u, (a, b, c, d, e, f)] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        let* f = p6;
        return((a, b, c, d, e, f))

    pub def tuple7(p1: GenParser[t1, u, a], 
                    p2: GenParser[t1, u, b], 
                    p3: GenParser[t1, u, c], 
                    p4: GenParser[t1, u, d], 
                    p5: GenParser[t1, u, e], 
                    p6: GenParser[t1, u, f], 
                    p7: GenParser[t1, u, g]): GenParser[t1, u, (a, b, c, d, e, f, g)] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        let* f = p6;
        let* g = p7;
        return((a, b, c, d, e, f, g))

    pub def tuple8(p1: GenParser[t1, u, a], 
                    p2: GenParser[t1, u, b], 
                    p3: GenParser[t1, u, c], 
                    p4: GenParser[t1, u, d], 
                    p5: GenParser[t1, u, e], 
                    p6: GenParser[t1, u, f], 
                    p7: GenParser[t1, u, g], 
                    p8: GenParser[t1, u, h]): GenParser[t1, u, (a, b, c, d, e, f, g, h)] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        let* f = p6;
        let* g = p7;
        let* h = p8;
        return((a, b, c, d, e, f, g, h))

    pub def tuple9(p1: GenParser[t1, u, a], 
                    p2: GenParser[t1, u, b], 
                    p3: GenParser[t1, u, c], 
                    p4: GenParser[t1, u, d], 
                    p5: GenParser[t1, u, e], 
                    p6: GenParser[t1, u, f], 
                    p7: GenParser[t1, u, g], 
                    p8: GenParser[t1, u, h], 
                    p9: GenParser[t1, u, i]): GenParser[t1, u, (a, b, c, d, e, f, g, h, i)] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        let* f = p6;
        let* g = p7;
        let* h = p8;
        let* i = p9;
        return((a, b, c, d, e, f, g, h, i))

    pub def tuple10(p1: GenParser[t1, u, a], 
                    p2: GenParser[t1, u, b], 
                    p3: GenParser[t1, u, c], 
                    p4: GenParser[t1, u, d], 
                    p5: GenParser[t1, u, e], 
                    p6: GenParser[t1, u, f], 
                    p7: GenParser[t1, u, g], 
                    p8: GenParser[t1, u, h], 
                    p9: GenParser[t1, u, i], 
                    p10: GenParser[t1, u, j]): GenParser[t1, u, (a, b, c, d, e, f, g, h, i, j)] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        let* f = p6;
        let* g = p7;
        let* h = p8;
        let* i = p9;
        let* j = p10;
        return((a, b, c, d, e, f, g, h, i, j))


}
