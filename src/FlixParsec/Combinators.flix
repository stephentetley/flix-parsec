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
    pub def *>(p1: TextParser[u, a, t], p2: TextParser[u, b, t]): TextParser[u, b, t] = seqRight(p1, p2)

    /// Alias for `seqLeft`
    pub def <*(p1: TextParser[u, a, t], p2: TextParser[u, b, t]): TextParser[u, a, t] = seqLeft(p1, p2)

    pub def <!>(p1: TextParser[u, a, t], p2: TextParser[u, a, t]): TextParser[u, a, t] =
        alt(p1, p2)

    // Haskell's <$>
    pub def <&>(f: a -> b, p2: TextParser[u, a, t]): TextParser[u, b, t] =
        map(f, p2)

    pub def <&(x: b, p2: TextParser[u, a, t]): TextParser[u, b, t] =
        p2 *> return(x)


    // Haskell's ap <*>
    pub def <*>(mf: TextParser[u, a -> b, t], p2: TextParser[u, a, t]): TextParser[u, b, t] =
        ap(mf, p2)


    pub def optional(p: TextParser[u, a, t]): TextParser[u, Option[a], t] = 
        alt(map(Some,p), return(None))

    pub def optionalWithDefault(p: TextParser[u, a, t], d: a): TextParser[u, a, t] = 
        alt(p, return(d))



    pub def ignore(p: TextParser[u, r, t]): TextParser[u, Unit, t] =
        map(constant(), p)


    pub def between(openp: TextParser[u, o, t], closep: TextParser[u, c, t], p: TextParser[u, r, t]): TextParser[u, r, t] = 
        use FlixParsec/Prim.{flatMap};
        let* _ = openp;
        let* a = p;
        let* _ = closep;
        return(a)

    pub def satisfies(p: TextParser[u, r, t], test: r -> Bool): TextParser[u, r, t] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p;
        if (test(a))
            return(a)
        else
            failure("satisfies")
            
    /// `p` is run with `lookahead`, no text is consumed
    pub def succeeds(p: TextParser[u, r, t]): TextParser[u, Bool, t] = 
        alt(seqRight(lookahead(p), return(true)), return(false))


    pub def countWith(append: (a, ac) -> ac, acc: ac, n: Int32, p: TextParser[u, a, t]): TextParser[u, ac, t] = 
        use FlixParsec/Prim.{flatMap};
        if (n <= 0)
            return(acc)
        else {
            let* a = p;
            countWith(append, append(a, acc), n-1, p)
        }

    pub def count(n: Int32, p: TextParser[u, r, t]): TextParser[u, List[r], t] = 
        use Chain.{empty, snoc, toList};
        map(toList, countWith((x, xs) -> snoc(xs, x), empty(), n, p))
    


    pub def manyWith(append: (a, ac) -> ac, acc: ac, p: TextParser[u, a, t]): TextParser[u, ac, t] = 
        ifThenElse(p, a -> manyWith(append, append(a, acc), p), return(acc))


    pub def many(p: TextParser[u, r, t]): TextParser[u, List[r], t] = 
        use Chain.{empty, snoc, toList};
        map(toList, manyWith((x, xs) -> snoc(xs, x), empty(), p))
        
    pub def many1With(append: (a, ac) -> ac, acc: ac, p: TextParser[u, a, t]): TextParser[u, ac, t] = 
        bind(p, x -> manyWith(append, append(x, acc), p))
        
    pub def many1(p: TextParser[u, r, t]): TextParser[u, List[r], t] = 
        use Chain.{empty, snoc, toList};
        map(toList, many1With((x, xs) -> snoc(xs, x), empty(), p))


    /// Count how many times parser `p` succeeds
    pub def ntimes(p: TextParser[u, r, t]): TextParser[u, Int32, t] = 
        manyWith((_,ac) -> ac + 1, 0, p)


    pub def manyTillWith(append: (a, ac) -> ac, acc: ac, p: TextParser[u, a, t], endp: TextParser[u, end, t]): TextParser[u, ac, t] = 
        ifThenElse(endp, _ -> return(acc), bind(p, a -> manyTillWith(append, append(a, acc), p, endp)))

    pub def manyTill(p: TextParser[u, r, t], endp: TextParser[u, end, t]): TextParser[u, List[r], t] = 
        use Chain.{empty, snoc, toList};
        map(toList, manyTillWith((x, xs) -> snoc(xs, x), empty(), p, endp))
        
    pub def many1TillWith(append: (a, ac) -> ac, acc: ac, p: TextParser[u, a, t], endp: TextParser[u, end, t]): TextParser[u, ac, t] = 
        bind(p, x -> manyTillWith(append, append(x, acc), p, endp))
        
    pub def many1Till(p: TextParser[u, r, t], endp: TextParser[u, end, t]): TextParser[u, List[r], t] = 
        use Chain.{empty, snoc, toList};
        map(toList, many1TillWith((x, xs) -> snoc(xs, x), empty(), p, endp))
    

    pub def sepByWith(append: (a, ac) -> ac, acc: ac, p: TextParser[u, a, t], sep: TextParser[u, sep,t]): TextParser[u, ac, t] = 
        ifThenElse(p, x -> sepByWithHelper(append, append(x, acc), p, sep), return(acc))
    
    // Initial p has been consumed
    def sepByWithHelper(append: (a, ac) -> ac, acc: ac, p: TextParser[u, a, t], sep: TextParser[u, sep,t]): TextParser[u, ac, t] = 
        ifThenElse(seqRight(sep, p), x -> sepByWithHelper(append, append(x, acc), p, sep), return(acc))

    pub def sepBy(p: TextParser[u, r, t], sep: TextParser[u, sep, t]): TextParser[u, List[r], t] = 
        use Chain.{empty, snoc, toList};
        map(toList, sepByWith((x, xs) -> snoc(xs, x), empty(), p, sep))

    pub def sepBy1With(append: (a, ac) -> ac, acc: ac, p: TextParser[u, a, t], sep: TextParser[u, sep, t]): TextParser[u, ac, t] = 
        bind(p, x -> sepByWithHelper(append, append(x, acc), p, sep))
    
    pub def sepBy1(p: TextParser[u, r, t], sep: TextParser[u, sep, t]): TextParser[u, List[r], t] = 
        use Chain.{empty, snoc, toList};
        map(toList, sepBy1With((x, xs) -> snoc(xs, x), empty(), p, sep))

    pub def endByWith(append: (a, ac) -> ac, acc: ac, p: TextParser[u, a, t], sep: TextParser[u, sep,t]): TextParser[u, ac, t] = 
        manyWith(append, acc, seqLeft(p, sep))

    pub def endBy(p: TextParser[u, r, t], sep: TextParser[u, sep, t]): TextParser[u, List[r], t] = 
        use Chain.{empty, snoc, toList};
        map(toList, endByWith((x, xs) -> snoc(xs, x), empty(), p, sep))

    pub def endBy1With(append: (a, ac) -> ac, acc: ac, p: TextParser[u, a, t], sep: TextParser[u, sep, t]): TextParser[u, ac, t] = 
        bind(seqLeft(p, sep), x -> endByWith(append, append(x, acc), p, sep))

    pub def endBy1(p: TextParser[u, r, t], sep: TextParser[u, sep, t]): TextParser[u, List[r], t] = 
        use Chain.{empty, snoc, toList};
        map(toList, endBy1With((x, xs) -> snoc(xs, x), empty(), p, sep))

    // seperated and optionally terminated with sep.
    pub def sepEndByWith(append: (a, ac) -> ac, acc: ac, p: TextParser[u, a, t], sep: TextParser[u, sep,t]): TextParser[u, ac, t] = 
        use FlixParsec/Prim.{flatMap};
        let* xs = sepByWith(append, acc, p, sep);
        ifThenElse(sep, _ -> return(xs), return(xs))

    // seperated and optionally terminated with sep.
    pub def sepEndBy(p: TextParser[u, r, t], sep: TextParser[u, sep, t]): TextParser[u, List[r], t] = 
        use Chain.{empty, snoc, toList};
        map(toList, sepEndByWith((x, xs) -> snoc(xs, x), empty(), p, sep))

    // seperated and optionally terminated with sep.
    pub def sepEndBy1With(append: (a, ac) -> ac, acc: ac, p: TextParser[u, a, t], sep: TextParser[u, sep,t]): TextParser[u, ac, t] = 
        use FlixParsec/Prim.{flatMap};
        let* xs = sepBy1With(append, acc, p, sep);
        ifThenElse(sep, _ -> return(xs), return(xs))

    // seperated and optionally terminated with sep.
    pub def sepEndBy1(p: TextParser[u, r, t], sep: TextParser[u, sep, t]): TextParser[u, List[r], t] = 
        use Chain.{empty, snoc, toList};
        map(toList, sepEndBy1With((x, xs) -> snoc(xs, x), empty(), p, sep))

    /// Count how many times parser `p` succeeds
    pub def ntimesSepBy(p: TextParser[u, a, t], sep: TextParser[u, sep, t]): TextParser[u, Int32, t] = 
        sepByWith((_, ac) -> ac + 1, 0, p, sep)

    /// Count how many times parser `p` succeeds
    pub def ntimesEndBy(p: TextParser[u, a, t], sep: TextParser[u, sep, t]): TextParser[u, Int32, t] = 
        endByWith((_, ac) -> ac + 1, 0, p, sep)

    /// Count how many times parser `p` succeeds
    pub def ntimesSepEndBy(p: TextParser[u, a, t], sep: TextParser[u, sep, t]): TextParser[u, Int32, t] = 
        sepEndByWith((_, ac) -> ac + 1, 0, p, sep)

    /// p1 must succeed, p2 parsers zero-or-more
    pub def manyTill2With(append: (a, ac) -> ac, acc: ac, p1: TextParser[u, a, t], p2: TextParser[u, a, t], endp: TextParser[u, end, t]): TextParser[u, ac, t] = 
        bind(p1, x -> manyTillWith(append, append(x,acc), p2, endp))
        
    /// p1 must succeed, p2 parsers zero-or-more
    pub def manyTill2(p1: TextParser[u, a, t], p2: TextParser[u, a, t], endp: TextParser[u, end, t]): TextParser[u, List[a], t] = 
        use Chain.{empty, snoc, toList};
        map(toList, manyTill2With((x, xs) -> snoc(xs, x), empty(), p1, p2, endp))

    /// p1 must succeed, p2 parsers one-or-more
    pub def many1Till2With(append: (a, ac) -> ac, acc: ac, 
                            p1: TextParser[u, a, t], 
                            p2: TextParser[u, a, t], 
                            endp: TextParser[u, end, t]): TextParser[u, ac, t] = 
        bind(p1, x -> many1TillWith(append, append(x,acc), p2, endp))
        
    /// p1 must succeed, p2 parsers one-or-more
    pub def many1Till2(p1: TextParser[u, a, t], p2: TextParser[u, a, t], endp: TextParser[u, end, t]): TextParser[u, List[a], t] = 
        use Chain.{empty, snoc, toList};
        map(toList, many1Till2With((x, xs) -> snoc(xs, x), empty(), p1, p2, endp))



    pub def skipMany(p: TextParser[u, r,t]): TextParser[u, Unit, t] = 
        ifThenElse(p, _ -> skipMany(p), return())

    pub def skipMany1(p: TextParser[u, r,t]): TextParser[u, Unit, t] = 
        bind(p, _ -> skipMany(p))


    pub def skipManyTill(p: TextParser[u, r,t], endp: TextParser[u, end,t]): TextParser[u, Unit, t] = 
        ifThenElse(endp, _ -> return(), bind(p, _ -> skipManyTill(p, endp)))

    pub def skipMany1Till(p: TextParser[u, r,t], endp: TextParser[u, end,t]): TextParser[u, Unit, t] = 
        bind(p, _ -> skipManyTill(p, endp))


    pub def chainl1(p: TextParser[u, r, t], op: TextParser[u, (r, r) -> r, t]): TextParser[u, r, t] = 
        bind(p, v1 -> chainl1Helper(p, op, v1))
        
    def chainl1Helper(p: TextParser[u, r, t], op: TextParser[u, (r, r) -> r, t], x: r): TextParser[u, r, t] = 
        alt(bind(op, f -> bind(p, v1 -> chainl1Helper(p, op, f(x, v1)))), 
            return(x))

    pub def chainl(p: TextParser[u, r, t], op: TextParser[u, (r, r) -> r, t], x: r): TextParser[u, r, t] = 
        alt(chainl1(p,op), return(x))


    def chainr1Helper(p: TextParser[u, r, t], op: TextParser[u, (r, r) -> r, t], x: r): TextParser[u, r, t] = 
        alt(chainr1Step(p, op, x), return(x))

    def chainr1Step(p: TextParser[u, r, t], op: TextParser[u, (r, r) -> r, t], x: r): TextParser[u, r, t] = 
        bind(op, f -> 
            bind(bind(p, v1 -> chainr1Helper(p, op, v1)), y -> 
                bind(chainr1Helper(p,op, f(x,y)), ans -> return(ans))))

    pub def chainr1(p: TextParser[u, r, t], op: TextParser[u, (r, r) -> r, t]): TextParser[u, r, t] = 
        bind(p, v1 -> chainr1Helper(p, op, v1))

    pub def chainr(p: TextParser[u, r, t], op: TextParser[u, (r, r) -> r, t], x: r): TextParser[u, r, t] = 
        alt(chainr1(p, op), return(x))

    pub def tuple2(p1: TextParser[u, a, t], p2: TextParser[u, b, t]): TextParser[u, (a, b), t] =
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        return((a, b))

    pub def tuple3(p1: TextParser[u, a, t], 
                    p2: TextParser[u, b, t], 
                    p3: TextParser[u, c, t]): TextParser[u, (a, b, c), t] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        return((a, b, c))

    pub def tuple4(p1: TextParser[u, a, t], 
                    p2: TextParser[u, b, t], 
                    p3: TextParser[u, c, t], 
                    p4: TextParser[u, d, t]): TextParser[u, (a, b, c, d), t] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        return((a, b, c, d))


    pub def tuple5(p1: TextParser[u, a, t], 
                    p2: TextParser[u, b, t], 
                    p3: TextParser[u, c, t], 
                    p4: TextParser[u, d, t], 
                    p5: TextParser[u, e, t]): TextParser[u, (a, b, c, d, e), t] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        return((a, b, c, d, e))

    pub def tuple6(p1: TextParser[u, a, t], 
                    p2: TextParser[u, b, t], 
                    p3: TextParser[u, c, t], 
                    p4: TextParser[u, d, t], 
                    p5: TextParser[u, e, t], 
                    p6: TextParser[u, f, t]): 
                    TextParser[u, (a, b, c, d, e, f), t] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        let* f = p6;
        return((a, b, c, d, e, f))

    pub def tuple7(p1: TextParser[u, a, t], 
                    p2: TextParser[u, b, t], 
                    p3: TextParser[u, c, t], 
                    p4: TextParser[u, d, t], 
                    p5: TextParser[u, e, t], 
                    p6: TextParser[u, f, t], 
                    p7: TextParser[u, g, t]): TextParser[u, (a, b, c, d, e, f, g), t] = 
        use FlixParsec/Prim.{flatMap};
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        let* f = p6;
        let* g = p7;
        return((a, b, c, d, e, f, g))

    pub def tuple8(p1: TextParser[u, a, t], 
                    p2: TextParser[u, b, t], 
                    p3: TextParser[u, c, t], 
                    p4: TextParser[u, d, t], 
                    p5: TextParser[u, e, t], 
                    p6: TextParser[u, f, t], 
                    p7: TextParser[u, g, t], 
                    p8: TextParser[u, h, t]): TextParser[u, (a, b, c, d, e, f, g, h), t] = 
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

    pub def tuple9(p1: TextParser[u, a, t], 
                    p2: TextParser[u, b, t], 
                    p3: TextParser[u, c, t], 
                    p4: TextParser[u, d, t], 
                    p5: TextParser[u, e, t], 
                    p6: TextParser[u, f, t], 
                    p7: TextParser[u, g, t], 
                    p8: TextParser[u, h, t], 
                    p9: TextParser[u, i, t]): TextParser[u, (a, b, c, d, e, f, g, h, i), t] = 
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

    pub def tuple10(p1: TextParser[u, a, t], 
                    p2: TextParser[u, b, t], 
                    p3: TextParser[u, c, t], 
                    p4: TextParser[u, d, t], 
                    p5: TextParser[u, e, t], 
                    p6: TextParser[u, f, t], 
                    p7: TextParser[u, g, t], 
                    p8: TextParser[u, h, t], 
                    p9: TextParser[u, i, t], 
                    p10: TextParser[u, j, t]): TextParser[u, (a, b, c, d, e, f, g, h, i, j), t] = 
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
