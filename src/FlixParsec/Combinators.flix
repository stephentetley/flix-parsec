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

    use FlixParsec.{TextParser};
    use FlixParsec/Prim.{return, fail, failure, map, ap, alt, bind, 
        ifThenElse, lookahead, seqLeft, seqRight};

    /// Alias for `seqRight`
    pub def *>(p1: TextParser[u, t1, a], p2: TextParser[u, t1, b]): TextParser[u, t1, b] = seqRight(p1, p2)

    /// Alias for `seqLeft`
    pub def <*(p1: TextParser[u, t1, a], p2: TextParser[u, t1, b]): TextParser[u, t1, a] = seqLeft(p1, p2)

    pub def <!>(p1: TextParser[u, t1, a], p2: TextParser[u, t1, a]): TextParser[u, t1, a] =
        alt(p1, p2)

    // Haskell's <$>
    pub def <&>(f: a -> b, p2: TextParser[u, t1, a]): TextParser[u, t1, b] =
        map(f, p2)

    pub def <&(x: b, p2: TextParser[u, t1, a]): TextParser[u, t1, b] =
        p2 *> return(x)


    // Haskell's ap <*>
    pub def <*>(mf: TextParser[u, t1, a -> b], p2: TextParser[u, t1, a]): TextParser[u, t1, b] =
        ap(mf, p2)


    pub def optional(p: TextParser[u, t1, a]): TextParser[u, t1, Option[a]] = 
        alt(map(Some,p), return(None))

    pub def optionalWithDefault(p: TextParser[u, t1, a], d: a): TextParser[u, t1, a] = 
        alt(p, return(d))



    pub def ignore(p: TextParser[u, t1, a]): TextParser[u, t1, Unit] =
        map(constant(), p)


    pub def between(openp: TextParser[u, t1, open], closep: TextParser[u, t1, close], p: TextParser[u, t1, a]): TextParser[u, t1, a] = 
        use FlixParsec/Prim.{flatMap};
        let* _ = openp;
        let* a = p;
        let* _ = closep;
        return(a)

    pub def satisfies(p: TextParser[u, t1, a], test: a -> Bool): TextParser[u, t1, a] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p;
        if (test(a))
            return(a)
        else
            failure("satisfies")
            
    /// `p` is run with `lookahead`, no text is consumed
    pub def succeeds(p: TextParser[u, t1, a]): TextParser[u, t1, Bool] = 
        alt(seqRight(lookahead(p), return(true)), return(false))


    pub def countWith(append: (a, ac) -> ac, acc: ac, n: Int32, p: TextParser[u, t1, a]): TextParser[u, t1, ac] = 
        use FlixParsec/Prim.{flatMap};
        if (n <= 0)
            return(acc)
        else {
            let* a = p;
            countWith(append, append(a, acc), n-1, p)
        }

    pub def count(n: Int32, p: TextParser[u, t1, a]): TextParser[u, t1, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, countWith((x, xs) -> snoc(xs, x), empty(), n, p))
    


    pub def manyWith(append: (a, ac) -> ac, acc: ac, p: TextParser[u, t1, a]): TextParser[u, t1, ac] = 
        ifThenElse(p, a -> manyWith(append, append(a, acc), p), return(acc))


    pub def many(p: TextParser[u, t1, a]): TextParser[u, t1, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, manyWith((x, xs) -> snoc(xs, x), empty(), p))
        
    pub def many1With(append: (a, ac) -> ac, acc: ac, p: TextParser[u, t1, a]): TextParser[u, t1, ac] = 
        bind(p, x -> manyWith(append, append(x, acc), p))
        
    pub def many1(p: TextParser[u, t1, a]): TextParser[u, t1, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, many1With((x, xs) -> snoc(xs, x), empty(), p))


    /// Count how many times parser `p` succeeds
    pub def ntimes(p: TextParser[u, t1, a]): TextParser[u, t1, Int32] = 
        manyWith((_,ac) -> ac + 1, 0, p)


    pub def manyTillWith(append: (a, ac) -> ac, 
                            acc: ac, 
                            p: TextParser[u, t1, a], 
                            endp: TextParser[u, t1, end]): TextParser[u, t1, ac] = 
        ifThenElse(endp, _ -> return(acc), bind(p, a -> manyTillWith(append, append(a, acc), p, endp)))

    pub def manyTill(p: TextParser[u, t1, a], endp: TextParser[u, t1, end]): TextParser[u, t1, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, manyTillWith((x, xs) -> snoc(xs, x), empty(), p, endp))
        
    pub def many1TillWith(append: (a, ac) -> ac, 
                            acc: ac, 
                            p: TextParser[u, t1, a], 
                            endp: TextParser[u, t1, end]): TextParser[u, t1, ac] = 
        bind(p, x -> manyTillWith(append, append(x, acc), p, endp))
        
    pub def many1Till(p: TextParser[u, t1, a], endp: TextParser[u, t1, end]): TextParser[u, t1, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, many1TillWith((x, xs) -> snoc(xs, x), empty(), p, endp))
    

    pub def sepByWith(append: (a, ac) -> ac, 
                        acc: ac, 
                        p: TextParser[u, t1, a], 
                        sep: TextParser[u, t1, sep]): TextParser[u, t1, ac] = 
        ifThenElse(p, x -> sepByWithHelper(append, append(x, acc), p, sep), return(acc))
    
    // Initial p has been consumed
    def sepByWithHelper(append: (a, ac) -> ac, 
                        acc: ac, 
                        p: TextParser[u, t1, a], 
                        sep: TextParser[u, t1, sep]): TextParser[u, t1, ac] = 
        ifThenElse(seqRight(sep, p), x -> sepByWithHelper(append, append(x, acc), p, sep), return(acc))

    pub def sepBy(p: TextParser[u, t1, a], sep: TextParser[u, t1, sep]): TextParser[u, t1, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, sepByWith((x, xs) -> snoc(xs, x), empty(), p, sep))

    pub def sepBy1With(append: (a, ac) -> ac, 
                        acc: ac, 
                        p: TextParser[u, t1, a], 
                        sep: TextParser[u, t1, sep]): TextParser[u, t1, ac] = 
        bind(p, x -> sepByWithHelper(append, append(x, acc), p, sep))
    
    pub def sepBy1(p: TextParser[u, t1, a], sep: TextParser[u, t1, sep]): TextParser[u, t1, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, sepBy1With((x, xs) -> snoc(xs, x), empty(), p, sep))

    pub def endByWith(append: (a, ac) -> ac, 
                        acc: ac, 
                        p: TextParser[u, t1, a], 
                        sep: TextParser[u, t1, sep]): TextParser[u, t1, ac] = 
        manyWith(append, acc, seqLeft(p, sep))

    pub def endBy(p: TextParser[u, t1, a], sep: TextParser[u, t1, sep]): TextParser[u, t1, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, endByWith((x, xs) -> snoc(xs, x), empty(), p, sep))

    pub def endBy1With(append: (a, ac) -> ac, 
                        acc: ac, 
                        p: TextParser[u, t1, a], 
                        sep: TextParser[u, t1, sep]): TextParser[u, t1, ac] = 
        bind(seqLeft(p, sep), x -> endByWith(append, append(x, acc), p, sep))

    pub def endBy1(p: TextParser[u, t1, a], sep: TextParser[u, t1, sep]): TextParser[u, t1, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, endBy1With((x, xs) -> snoc(xs, x), empty(), p, sep))

    // seperated and optionally terminated with sep.
    pub def sepEndByWith(append: (a, ac) -> ac, 
                            acc: ac, 
                            p: TextParser[u, t1, a], 
                            sep: TextParser[u, t1, sep]): TextParser[u, t1, ac] = 
        use FlixParsec/Prim.{flatMap};
        let* xs = sepByWith(append, acc, p, sep);
        ifThenElse(sep, _ -> return(xs), return(xs))

    // seperated and optionally terminated with sep.
    pub def sepEndBy(p: TextParser[u, t1, a], sep: TextParser[u, t1, sep]): TextParser[u, t1, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, sepEndByWith((x, xs) -> snoc(xs, x), empty(), p, sep))

    // seperated and optionally terminated with sep.
    pub def sepEndBy1With(append: (a, ac) -> ac, 
                            acc: ac, 
                            p: TextParser[u, t1, a], 
                            sep: TextParser[u, t1, sep]): TextParser[u, t1, ac] = 
        use FlixParsec/Prim.{flatMap};
        let* xs = sepBy1With(append, acc, p, sep);
        ifThenElse(sep, _ -> return(xs), return(xs))

    // seperated and optionally terminated with sep.
    pub def sepEndBy1(p: TextParser[u, t1, a], sep: TextParser[u, t1, sep]): TextParser[u, t1, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, sepEndBy1With((x, xs) -> snoc(xs, x), empty(), p, sep))

    /// Count how many times parser `p` succeeds
    pub def ntimesSepBy(p: TextParser[u, t1, a], sep: TextParser[u, t1, sep]): TextParser[u, t1, Int32] = 
        sepByWith((_, ac) -> ac + 1, 0, p, sep)

    /// Count how many times parser `p` succeeds
    pub def ntimesEndBy(p: TextParser[u, t1, a], sep: TextParser[u, t1, sep]): TextParser[u, t1, Int32] = 
        endByWith((_, ac) -> ac + 1, 0, p, sep)

    /// Count how many times parser `p` succeeds
    pub def ntimesSepEndBy(p: TextParser[u, t1, a], sep: TextParser[u, t1, sep]): TextParser[u, t1, Int32] = 
        sepEndByWith((_, ac) -> ac + 1, 0, p, sep)

    /// p1 must succeed, p2 parsers zero-or-more
    pub def manyTill2With(append: (a, ac) -> ac, 
                            acc: ac, 
                            p1: TextParser[u, t1, a], 
                            p2: TextParser[u, t1, a], 
                            endp: TextParser[u, t1, end]): TextParser[u, t1, ac] = 
        bind(p1, x -> manyTillWith(append, append(x,acc), p2, endp))
        
    /// p1 must succeed, p2 parsers zero-or-more
    pub def manyTill2(p1: TextParser[u, t1, a], 
                      p2: TextParser[u, t1, a], 
                      endp: TextParser[u, t1, end]): TextParser[u, t1, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, manyTill2With((x, xs) -> snoc(xs, x), empty(), p1, p2, endp))

    /// p1 must succeed, p2 parsers one-or-more
    pub def many1Till2With(append: (a, ac) -> ac, acc: ac, 
                            p1: TextParser[u, t1, a], 
                            p2: TextParser[u, t1, a], 
                            endp: TextParser[u, t1, end]): TextParser[u, t1, ac] = 
        bind(p1, x -> many1TillWith(append, append(x,acc), p2, endp))
        
    /// p1 must succeed, p2 parsers one-or-more
    pub def many1Till2(p1: TextParser[u, t1, a], 
                        p2: TextParser[u, t1, a], 
                        endp: TextParser[u, t1, end]): TextParser[u, t1, List[a]] = 
        use Chain.{empty, snoc, toList};
        map(toList, many1Till2With((x, xs) -> snoc(xs, x), empty(), p1, p2, endp))



    pub def skipMany(p: TextParser[u, t1, a]): TextParser[u, t1, Unit] = 
        ifThenElse(p, _ -> skipMany(p), return())

    pub def skipMany1(p: TextParser[u, t1, a]): TextParser[u, t1, Unit] = 
        bind(p, _ -> skipMany(p))


    pub def skipManyTill(p: TextParser[u, t1, a], endp: TextParser[u, t1, end]): TextParser[u, t1, Unit] = 
        ifThenElse(endp, _ -> return(), bind(p, _ -> skipManyTill(p, endp)))

    pub def skipMany1Till(p: TextParser[u, t1, a], endp: TextParser[u, t1, end]): TextParser[u, t1, Unit] = 
        bind(p, _ -> skipManyTill(p, endp))


    pub def chainl1(p: TextParser[u, t1, a], op: TextParser[u, t1, (a, a) -> a]): TextParser[u, t1, a] = 
        bind(p, v1 -> chainl1Helper(p, op, v1))
        
    def chainl1Helper(p: TextParser[u, t1, a], op: TextParser[u, t1, (a, a) -> a], x: a): TextParser[u, t1, a] = 
        alt(bind(op, f -> bind(p, v1 -> chainl1Helper(p, op, f(x, v1)))), 
            return(x))

    pub def chainl(p: TextParser[u, t1, a], op: TextParser[u, t1, (a, a) -> a], x: a): TextParser[u, t1, a] = 
        alt(chainl1(p,op), return(x))


    def chainr1Helper(p: TextParser[u, t1, a], op: TextParser[u, t1, (a, a) -> a], x: a): TextParser[u, t1, a] = 
        alt(chainr1Step(p, op, x), return(x))

    def chainr1Step(p: TextParser[u, t1, a], op: TextParser[u, t1, (a, a) -> a], x: a): TextParser[u, t1, a] = 
        bind(op, f -> 
            bind(bind(p, v1 -> chainr1Helper(p, op, v1)), y -> 
                bind(chainr1Helper(p,op, f(x,y)), ans -> return(ans))))

    pub def chainr1(p: TextParser[u, t1, a], op: TextParser[u, t1, (a, a) -> a]): TextParser[u, t1, a] = 
        bind(p, v1 -> chainr1Helper(p, op, v1))

    pub def chainr(p: TextParser[u, t1, a], op: TextParser[u, t1, (a, a) -> a], x: a): TextParser[u, t1, a] = 
        alt(chainr1(p, op), return(x))

    pub def tuple2(p1: TextParser[u, t1, a], p2: TextParser[u, t1, b]): TextParser[u, t1, (a, b)] =
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        return((a, b))

    pub def tuple3(p1: TextParser[u, t1, a], 
                    p2: TextParser[u, t1, b], 
                    p3: TextParser[u, t1, c]): TextParser[u, t1, (a, b, c)] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        return((a, b, c))

    pub def tuple4(p1: TextParser[u, t1, a], 
                    p2: TextParser[u, t1, b], 
                    p3: TextParser[u, t1, c], 
                    p4: TextParser[u, t1, d]): TextParser[u, t1, (a, b, c, d)] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        return((a, b, c, d))


    pub def tuple5(p1: TextParser[u, t1, a], 
                    p2: TextParser[u, t1, b], 
                    p3: TextParser[u, t1, c], 
                    p4: TextParser[u, t1, d], 
                    p5: TextParser[u, t1, e]): TextParser[u, t1, (a, b, c, d, e)] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        return((a, b, c, d, e))

    pub def tuple6(p1: TextParser[u, t1, a], 
                    p2: TextParser[u, t1, b], 
                    p3: TextParser[u, t1, c], 
                    p4: TextParser[u, t1, d], 
                    p5: TextParser[u, t1, e], 
                    p6: TextParser[u, t1, f]): 
                    TextParser[u, t1, (a, b, c, d, e, f)] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        let* f = p6;
        return((a, b, c, d, e, f))

    pub def tuple7(p1: TextParser[u, t1, a], 
                    p2: TextParser[u, t1, b], 
                    p3: TextParser[u, t1, c], 
                    p4: TextParser[u, t1, d], 
                    p5: TextParser[u, t1, e], 
                    p6: TextParser[u, t1, f], 
                    p7: TextParser[u, t1, g]): TextParser[u, t1, (a, b, c, d, e, f, g)] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        let* f = p6;
        let* g = p7;
        return((a, b, c, d, e, f, g))

    pub def tuple8(p1: TextParser[u, t1, a], 
                    p2: TextParser[u, t1, b], 
                    p3: TextParser[u, t1, c], 
                    p4: TextParser[u, t1, d], 
                    p5: TextParser[u, t1, e], 
                    p6: TextParser[u, t1, f], 
                    p7: TextParser[u, t1, g], 
                    p8: TextParser[u, t1, h]): TextParser[u, t1, (a, b, c, d, e, f, g, h)] = 
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

    pub def tuple9(p1: TextParser[u, t1, a], 
                    p2: TextParser[u, t1, b], 
                    p3: TextParser[u, t1, c], 
                    p4: TextParser[u, t1, d], 
                    p5: TextParser[u, t1, e], 
                    p6: TextParser[u, t1, f], 
                    p7: TextParser[u, t1, g], 
                    p8: TextParser[u, t1, h], 
                    p9: TextParser[u, t1, i]): TextParser[u, t1, (a, b, c, d, e, f, g, h, i)] = 
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

    pub def tuple10(p1: TextParser[u, t1, a], 
                    p2: TextParser[u, t1, b], 
                    p3: TextParser[u, t1, c], 
                    p4: TextParser[u, t1, d], 
                    p5: TextParser[u, t1, e], 
                    p6: TextParser[u, t1, f], 
                    p7: TextParser[u, t1, g], 
                    p8: TextParser[u, t1, h], 
                    p9: TextParser[u, t1, i], 
                    p10: TextParser[u, t1, j]): TextParser[u, t1, (a, b, c, d, e, f, g, h, i, j)] = 
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
