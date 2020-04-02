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

use TextParser/Internal.DList;
use TextParser/Internal.{dcons, dsnoc, dlistToList, dempty};
use TextParser.Parser;
use TextParser.Index;
use TextParser.ParseError;
use TextParser.{bind, bindFS, return, alt, fail, failure, nestError, swapError, map};

///
/// Combinators to build parsers.
///

namespace TextParser/Combinators {

    def cons(x: a, xs: List[a]): List[a] = x :: xs
    
    pub def seqLeft(pa: Parser[a], pb: Parser[b]): Parser[a] =
        bind(pa, a -> bind(pb, _ -> return(a)))

    pub def seqRight(pa: Parser[a], pb: Parser[b]): Parser[b] =
        bind(pa, _ -> bind(pb, b -> return(b)))


    pub def satisfies(p: Parser[a], test: ans -> Bool): Parser[a] = 
        bind(p, ans -> if (test(ans)) return(ans) else fail("satisfies"))


    pub def countWith(f: (a, ac) -> ac, zero: ac, n: Int32, p: Parser[a]): Parser[ac] = 
        countWithHelper(f, zero, n, p, 0, return) |> nestError("countWith")


    def countWithHelper(f: (a, ac) -> ac, zero: ac, ntimes: Int32, p: Parser[a], i: Int32, cont: ac -> Parser[ac]): Parser[ac] = 
        use TextParser.flatMap;
        if (i == ntimes)
            cont(zero)
        else 
            let* a = bindFS(p, failure, return);
            countWithHelper(
                f, 
                zero, 
                ntimes, 
                p, 
                i+1,
                ac -> cont(f(a,ac))
            )

    pub def count(n: Int32, p: Parser[a]): Parser[List[a]] = 
        countWith(cons, Nil, n, p) |> swapError("count")

    pub def choice(ps: List[Parser[a]]): Parser[a] = 
        match ps { 
            case Nil => fail("choice")
            case p :: rs => bindFS(p, _ -> choice(rs), return)

        }


    pub def notFollowedBy(p: Parser[a]): Parser[Unit] = 
        bindFS(p, _ -> fail("notFollowedBy"), _ -> return(()))
        

    pub def option(one: ans, p: Parser[a]): Parser[a] = 
        bindFS(p, _ -> return(one), return)
        
    pub def optional(p: Parser[a]): Parser[Option[a]] = 
        bindFS(p, _ -> return(None), x -> return(Some(x)))
    

    pub def ignore(p: Parser[a]): Parser[Unit] = 
        map(_ -> (), p)



    pub def between(open: Parser[z1], close: Parser[z2], p: Parser[a]): Parser[a] = 
        seqRight(open, seqLeft(p, close))


    pub def manyWith(f: (a,ac) -> ac, zero: ac, p: Parser[a]): Parser[ac] = 
        manyWithHelper(f, zero, p, return) 


    def manyWithHelper(f: (a, ac) -> ac, zero: ac, p: Parser[a], cont: ac -> Parser[ac]): Parser[ac] = 
        use TextParser.flatMap;
        let* ans = optional(p);
        match ans { 
            case None => cont(zero)
            case Some(a) => 
                manyWithHelper(
                    f, 
                    zero, 
                    p, 
                    ac -> cont(f(a,ac))
                )
        }

    pub def many(p: Parser[a]): Parser[List[a]] = 
        manyWith(cons, Nil, p)

    pub def many1With(f: (a,ac) -> ac, zero: ac, p: Parser[a]): Parser[ac] = 
        use TextParser.flatMap; 
        let* a = p;
        let* ac = manyWith(f, zero, p);
        return(f(a, ac)) |> nestError("many1With")

    pub def many1(p: Parser[a]): Parser[List[a]] = 
        many1With(cons, Nil, p) |> swapError("many1")

    
    pub def ntimes(p: Parser[a]): Parser[Int32] = 
        manyWith((_,ac) -> ac+1, 0, p)

    pub def manyTillWith(f: (a, ac) -> ac, zero: ac, p: Parser[a], endp: Parser[z]): Parser[ac] = 
        manyTillWithHelper(f, zero, p, endp, return) |> nestError("manyTillWith")


    def manyTillWithHelper(f: (a, ac) -> ac, zero: ac, p: Parser[a], endp: Parser[z], cont: ac -> Parser[ac]): Parser[ac] = 
        use TextParser.flatMap;
        let* done = optional(endp);
        match done { 
            case Some(_) => cont(zero)
            case None => {
                let* a = bindFS(p, e -> failure(e), return);
                manyTillWithHelper(
                    f, 
                    zero, 
                    p, 
                    endp,
                    ac -> cont(f(a,ac))
                )
            }
        }
        
    pub def manyTill(p: Parser[a], endp: Parser[z]): Parser[List[a]] = 
        manyTillWithHelper(cons, Nil, p, endp, return) |> swapError("manyTill")

    pub def many1TillWith(f: (a, ac) -> ac, zero: ac, p: Parser[a], endp: Parser[z]): Parser[ac] = 
        use TextParser.flatMap;
        let* a = p;
        let* ac = manyTillWith(f, zero, p, endp);
        return(f(a, ac)) |> swapError("many1TillWith")

    pub def many1Till(p: Parser[a], endp: Parser[z]): Parser[List[a]] = 
        many1TillWith(cons, Nil, p, endp) |> swapError("many1Till")

    pub def skipMany(p: Parser[a]): Parser[Unit] = 
        manyWith((_,ac) -> ac, (), p)

    pub def skipMany1(p: Parser[a]): Parser[Unit] = 
        many1With((_,ac) -> ac, (), p) |> swapError("skipMany1")

    pub def skipManyTill(p: Parser[a], endp: Parser[z]): Parser[Unit] = 
        manyTillWith((_,ac) -> ac, (), p, endp)

    pub def skipMany1Till(p: Parser[a], endp: Parser[z]): Parser[Unit] = 
        many1TillWith((_,ac) -> ac, (), p, endp)

    
    pub def sepByWith(f: (a,ac) -> ac, zero: ac, p: Parser[a], sep: Parser[sep]): Parser[ac] = 
        sepByWithHelper(f, zero, p, sep, return) |> nestError("sepByWith")

        
    def sepByWithHelper(f: (a, ac) -> ac, zero: ac, p: Parser[a], sep: Parser[sep], cont: ac -> Parser[ac]): Parser[ac] = 
        use TextParser.flatMap;
        let* ans = optional(p);
        match ans { 
            case None => cont(zero)
            case Some(a) => 
                let* ac = manyWith(f, zero, seqRight(sep, p));
                return(f(a,ac))
        }

    pub def sepBy(p: Parser[a], sep: Parser[sep]): Parser[List[a]] = 
        sepByWith(cons, Nil, p, sep) |> swapError("sepBy")


    pub def sepBy1With(f: (a,ac) -> ac, zero: ac, p: Parser[a], sep: Parser[sep]): Parser[ac] = 
        use TextParser.flatMap;
        let* a = p;
        let* ac = manyWith(f, zero, seqRight(sep, p));
        return(f(a, ac)) |> swapError("sepBy1With")
        
    pub def sepBy1(p: Parser[a], sep: Parser[sep]): Parser[List[a]] = 
        sepBy1With(cons, Nil, p, sep) |> swapError("sepBy1")

    pub def endByWith(f: (a,ac) -> ac, zero: ac, p: Parser[a], sep: Parser[sep]): Parser[ac] = 
        manyWith(f, zero, seqLeft(p, sep)) |> nestError("endByWith")

    pub def endBy(p: Parser[a], sep: Parser[sep]): Parser[List[a]] = 
        endByWith(cons, Nil, p, sep) |> swapError("endBy")

    pub def endBy1With(f: (a,ac) -> ac, zero: ac, p: Parser[a], sep: Parser[sep]): Parser[ac] = 
        many1With(f, zero, seqLeft(p, sep)) |> swapError("endBy1With")

    pub def endBy1(f: (a,ac) -> ac, zero: ac, p: Parser[a], sep: Parser[sep]): Parser[ac] = 
        endBy1With(f, zero, p, sep) |> swapError("endBy1With")


    /// Optionally terminated with sep.
    pub def sepEndByWith(f: (a,ac) -> ac, zero: ac, p: Parser[a], sep: Parser[sep]): Parser[ac] = 
        seqLeft(sepByWith(f, zero, p, sep), optional(sep)) |> swapError("sepEndByWith")

    /// Optionally terminated with sep.
    pub def sepEndBy(p: Parser[a], sep: Parser[sep]): Parser[List[a]] = 
        sepEndByWith(cons, Nil, p, sep) |> swapError("sepEndBy")


    pub def sepEndBy1With(f: (a,ac) -> ac, zero: ac, p: Parser[a], sep: Parser[sep]): Parser[ac] = 
        seqLeft(sepBy1With(f, zero, p, sep), optional(sep)) |> swapError("sepEndBy1With")

    pub def sepEndBy1(p: Parser[a], sep: Parser[sep]): Parser[List[a]] = 
        sepEndBy1With(cons, Nil, p, sep) |> swapError("sepEndBy1")


    /// p1 must succeed, p2 parsers zero-or-more
    pub def manyTill2With(f: (a,ac) -> ac, zero: ac, p1: Parser[a], p2: Parser[a], endp: Parser[sep]): Parser[ac] = 
        use TextParser.flatMap;
        let* a = p1;
        let* ac = manyTillWith(f, zero, p2, endp);
        return(f(a,ac)) |> swapError("manyTill2With")

    /// p1 must succeed, p2 parsers zero-or-more
    pub def manyTill2(p1: Parser[a], p2: Parser[a], endp: Parser[sep]): Parser[List[a]] = 
        manyTill2With(cons, Nil, p1, p2, endp) |> swapError("manyTill2")

    /// p1 must succeed, p2 parsers one-or-more
    pub def many1Till2With(f: (a,ac) -> ac, zero: ac, p1: Parser[a], p2: Parser[a], endp: Parser[sep]): Parser[ac] = 
        use TextParser.flatMap;
        let* a = p1;
        let* ac = many1TillWith(f, zero, p2, endp);
        return(f(a,ac)) |> swapError("many1Till2With")

    /// p1 must succeed, p2 parsers one-or-more
    pub def many1Till2(p1: Parser[a], p2: Parser[a], endp: Parser[sep]): Parser[List[a]] = 
        many1Till2With(cons, Nil, p1, p2, endp) |> swapError("many1Till2")

    


    pub def chainl1(p: Parser[a], op: Parser[(a, a) -> a]): Parser[a] = 
        bind(p, v1 -> chainl1Helper(p, op, v1))

    def chainl1Helper(p: Parser[a], op: Parser[(a, a) -> a], x: a): Parser[a] = 
        alt(bind(op, f -> bind(p, v1 -> chainl1Helper(p, op, f(x, v1)))), 
            return(x))


    pub def chainl(p: Parser[a], op: Parser[a -> a -> a], x: a): Parser[a] = 
        alt(chainl1(p,op), return(x))


    def chainr1Helper(p: Parser[a], op: Parser[(a, a) -> a], x: a): Parser[a] = 
        alt(chainr1Step(p, op, x), return(x))

    def chainr1Step(p: Parser[a], op: Parser[(a, a) -> a], x: a): Parser[a] = 
        bind(op, f -> 
            bind(bind(p, v1 -> chainr1Helper(p, op, v1)), y -> 
                bind(chainr1Helper(p,op, f(x,y)), ans -> return(ans))))

    pub def chainr1(p: Parser[a], op: Parser[(a, a) -> a]): Parser[a] = 
        bind(p, v1 -> chainr1Helper(p, op, v1))

    pub def chainr(p: Parser[a], op: Parser[(a, a) -> a], x: a): Parser[a] = 
        alt(chainr1(p, op), return(x))

    
    pub def liftM2(f: (a, b) -> ans, p1: Parser[a], p2: Parser[b]): Parser[ans] = 
        bind(p1, v1 -> 
            bind(p2, v2 -> return(f(v1,v2))))

    pub def liftM3(f: (a, b, c) -> ans, p1: Parser[a], p2: Parser[b], p3: Parser[c]): Parser[ans] = 
        bind(p1, v1 -> 
            liftM2((v2,v3) -> f(v1,v2,v3), p2, p3))

    pub def liftM4(f: (a, b, c, d) -> ans, p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d]): Parser[ans] = 
        bind(p1, v1 -> 
            liftM3((v2,v3,v4) -> f(v1,v2,v3,v4), p2, p3, p4))
                    
    pub def liftM5(f: (a, b, c, d, e) -> ans, p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e]): Parser[ans] = 
        bind(p1, v1 -> 
            liftM4((v2,v3,v4,v5) -> f(v1,v2,v3,v4,v5), p2, p3, p4, p5))

    pub def liftM6(f: (a, b, c, d, e, f) -> ans, p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f]): Parser[ans] = 
        bind(p1, v1 -> 
            liftM5((v2,v3,v4,v5,v6) -> f(v1,v2,v3,v4,v5,v6), p2, p3, p4, p5, p6))

    pub def liftM7(f: (a, b, c, d, e, f, g) -> ans, p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f], p7: Parser[g]): Parser[ans] = 
        bind(p1, v1 -> 
            liftM6((v2,v3,v4,v5,v6,v7) -> f(v1,v2,v3,v4,v5,v6,v7), p2, p3, p4, p5, p6,p7))

    pub def liftM8(f: (a, b, c, d, e, f, g, h) -> ans, p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f], p7: Parser[g], p8: Parser[h]): Parser[ans] = 
        bind(p1, v1 -> 
            liftM7((v2,v3,v4,v5,v6,v7,v8) -> f(v1,v2,v3,v4,v5,v6,v7,v8), p2, p3, p4, p5, p6,p7,p8))

    pub def liftM9(f: (a, b, c, d, e, f, g, h, i) -> ans, p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f], p7: Parser[g], p8: Parser[h], p9: Parser[i]): Parser[ans] = 
        bind(p1, v1 -> 
            liftM8((v2,v3,v4,v5,v6,v7,v8,v9) -> f(v1,v2,v3,v4,v5,v6,v7,v8,v9), p2, p3, p4, p5, p6,p7,p8,p9))


    ///
    /// `pipe1` is `map` with the arguments flipped.
    ///
    pub def pipe1(p1: Parser[a], f: a -> ans): Parser[ans] = 
        map(f, p1)

    pub def pipe2(p1: Parser[a], p2: Parser[b], f: (a,b) -> ans): Parser[ans] = 
        liftM2(f, p1, p2)

    pub def pipe3(p1: Parser[a], p2: Parser[b], p3: Parser[c], f: (a,b,c) -> ans): Parser[ans] = 
        liftM3(f, p1, p2, p3)

    pub def pipe4(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], f: (a,b,c,d) -> ans): Parser[ans] = 
        liftM4(f, p1, p2, p3, p4)

    pub def pipe5(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], f: (a,b,c,d,e) -> ans): Parser[ans] = 
        liftM5(f, p1, p2, p3, p4, p5)

    pub def pipe6(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f], f: (a,b,c,d,e,f) -> ans): Parser[ans] = 
        liftM6(f, p1, p2, p3, p4, p5, p6)

    pub def pipe7(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f], p7: Parser[g], f: (a,b,c,d,e,f,g) -> ans): Parser[ans] = 
        liftM7(f, p1, p2, p3, p4, p5, p6, p7)

    pub def pipe8(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f], p7: Parser[g], p8: Parser[h], f: (a,b,c,d,e,f,g,h) -> ans): Parser[ans] = 
        liftM8(f, p1, p2, p3, p4, p5, p6, p7, p8)

    pub def pipe9(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f], p7: Parser[g], p8: Parser[h], p9: Parser[i], f: (a,b,c,d,e,f,g,h, i) -> ans): Parser[ans] = 
        liftM9(f, p1, p2, p3, p4, p5, p6, p7, p8, p9)


    pub def tuple2(p1: Parser[a], p2: Parser[b]): Parser[(a,b)] = 
        pipe2(p1, p2, (a,b) -> (a,b))

    pub def tuple3(p1: Parser[a], p2: Parser[b], p3: Parser[c]): Parser[(a,b,c)] = 
        pipe3(p1, p2, p3, (a,b,c) -> (a,b,c))

    pub def tuple4(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d]): Parser[(a,b,c,d)] = 
        pipe4(p1, p2, p3, p4, (a,b,c,d) -> (a,b,c,d))

    pub def tuple5(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e]): Parser[(a,b,c,d,e)] = 
        pipe5(p1, p2, p3, p4, p5, (a,b,c,d,e) -> (a,b,c,d,e))

    pub def tuple6(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f]): Parser[(a,b,c,d,e,f)] = 
        pipe6(p1, p2, p3, p4, p5, p6, (a,b,c,d,e,f) -> (a,b,c,d,e,f))

    pub def tuple7(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f], p7: Parser[g]): Parser[(a,b,c,d,e,f,g)] = 
        pipe7(p1, p2, p3, p4, p5, p6, p7, (a,b,c,d,e,f,g) -> (a,b,c,d,e,f,g))

    pub def tuple8(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f], p7: Parser[g], p8: Parser[h]): Parser[(a,b,c,d,e,f,g,h)] = 
        pipe8(p1, p2, p3, p4, p5, p6, p7, p8, (a,b,c,d,e,f,g,h) -> (a,b,c,d,e,f,g,h))

    pub def tuple9(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f], p7: Parser[g], p8: Parser[h], p9: Parser[i]): Parser[(a,b,c,d,e,f,g,h,i)] = 
        pipe9(p1, p2, p3, p4, p5, p6, p7, p8, p9, (a,b,c,d,e,f,g,h,i) -> (a,b,c,d,e,f,g,h,i))

}