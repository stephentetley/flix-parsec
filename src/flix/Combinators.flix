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

use TextParser.{Parser};
use TextParser.{return, bind, fail, failure, alt, bindEither, nestError, swapError, map, optional};

namespace TextParser/Combinators {

    pub def liftResult(x: Result[t,e]): Parser[t] = 
        match x {
            case Ok(a)  => return(a)
            case Err(_) => fail("liftResult")
        }

    pub def liftOption(x: Option[t]): Parser[t] = 
        match x {
            case Some(a) => return(a)
            case None    => fail("liftOption")
        }

    pub def seqLeft(p: Parser[a], q: Parser[b]): Parser[a] =
        use TextParser.flatMap;
        let* a = p;
        let* _ = q;
        return(a)

    pub def seqRight(p: Parser[a], q: Parser[b]): Parser[b] =
        use TextParser.flatMap;
        let* _ = p;
        let* b = q;
        return(b)

    pub def satisfies(p: Parser[a], test: ans -> Bool): Parser[a] = 
        use TextParser.flatMap;
        let* a = p;
        if (test(a))
            return(a)
        else
            fail("satifies")

    pub def countWith(append: (a, ac) -> ac, zero: ac, n: Int32, p: Parser[a]): Parser[ac] = 
        countWithHelper(append, zero, n, p, return) |> nestError("countWith")


    def countWithHelper(append: (a, ac) -> ac, zero: ac, ntimes: Int32, p: Parser[a], k: ac -> Parser[ac]): Parser[ac] = 
        use TextParser.flatMap;
        if (ntimes <= 0)
            k(zero)
        else 
            let* a = bindEither(p, failure, return);
            countWithHelper(
                append, 
                zero, 
                ntimes +1, 
                p, 
                ac -> { let ac1 = append(a,ac); k(ac1) }
            )

    pub def count(n: Int32, p: Parser[a]): Parser[List[a]] = 
        countWith((x,xs) -> x :: xs, Nil, n, p) |> swapError("count")

    pub def choice(ps: List[Parser[a]]): Parser[a] = 
        use TextParser.flatMap;
        match ps { 
            case Nil => fail("choice")
            case p :: rs => { 
                let* ao = optional(p);
                match ao { 
                    case Some(a) => return(a)
                    case None => choice(rs)
                }
            }
        }

    pub def ignore(p: Parser[a]): Parser[Unit] = 
        use TextParser.flatMap;
        let* _ = p;
        return()
        
    pub def between(open: Parser[z1], close: Parser[z2], p: Parser[a]): Parser[a] = 
        use TextParser.flatMap;
        let* _ = open;
        let* a = p;
        let* _ = close;
        return(a)


    pub def manyWith(append: (a,ac) -> ac, zero: ac, p: Parser[a]): Parser[ac] = 
        manyWithHelper(append, zero, p, return) 


    def manyWithHelper(append: (a, ac) -> ac, zero: ac, p: Parser[a], k: ac -> Parser[ac]): Parser[ac] = 
        use TextParser.flatMap;
        let* ans = optional(p);
        match ans { 
            case None => k(zero)
            case Some(a) => 
                manyWithHelper(
                    append, 
                    zero, 
                    p, 
                    ac -> { let ac1 = append(a, ac); k(ac1) }
                )
        }

    pub def many(p: Parser[a]): Parser[List[a]] = 
        manyWith((x,xs) -> x :: xs, Nil, p)

    pub def many1With(append: (a,ac) -> ac, zero: ac, p: Parser[a]): Parser[ac] = 
        use TextParser.flatMap; 
        let* a = p;
        let* ac = manyWith(append, zero, p);
        return(append(a, ac)) |> nestError("many1With")

    pub def many1(p: Parser[a]): Parser[List[a]] = 
        many1With((x,xs) -> x :: xs, Nil, p) |> swapError("many1")

    pub def manyTillWith(append: (a, ac) -> ac, zero: ac, p: Parser[a], endp: Parser[e]): Parser[ac] = 
        manyTillWithHelper(append, zero, p, endp, return) |> nestError("manyTillWith")


    def manyTillWithHelper(append: (a, ac) -> ac, zero: ac, p: Parser[a], endp: Parser[e], sk: ac -> Parser[ac]): Parser[ac] = 
        use TextParser.flatMap;
        let* done = optional(endp);
        match done { 
            case Some(_) => sk(zero)
            case None => 
                bindEither(p
                    , failure
                    , x -> manyTillWithHelper(append, zero, p, endp, ac -> { let ac1 = append(x, ac); sk(ac1)})
                )                
        }
        
    pub def manyTill(p: Parser[a], endp: Parser[z]): Parser[List[a]] = 
        manyTillWithHelper((x,xs) -> x :: xs, Nil, p, endp, return) |> swapError("manyTill")

    pub def many1TillWith(f: (a, ac) -> ac, zero: ac, p: Parser[a], endp: Parser[e]): Parser[ac] = 
        use TextParser.flatMap;
        let* a = p;
        let* ac = manyTillWith(f, zero, p, endp);
        return(f(a, ac)) |> swapError("many1TillWith")

    pub def many1Till(p: Parser[a], endp: Parser[e]): Parser[List[a]] = 
        many1TillWith((x,xs) -> x :: xs, Nil, p, endp) |> swapError("many1Till")

    pub def skipMany(p: Parser[a]): Parser[Unit] = 
        manyWith((_,_) -> (), (), p)

    pub def skipMany1(p: Parser[a]): Parser[Unit] = 
        many1With((_,ac) -> ac, (), p) |> swapError("skipMany1")

    pub def skipManyTill(p: Parser[a], endp: Parser[e]): Parser[Unit] = 
        manyTillWith((_,ac) -> ac, (), p, endp)

    pub def skipMany1Till(p: Parser[a], endp: Parser[e]): Parser[Unit] = 
        many1TillWith((_,ac) -> ac, (), p, endp)

    
    pub def sepByWith(append: (a,ac) -> ac, zero: ac, p: Parser[a], sep: Parser[sep]): Parser[ac] = 
        sepByWithHelper(append, zero, p, sep, return) |> nestError("sepByWith")

    def sepByWithHelper(append: (a, ac) -> ac, zero: ac, p: Parser[a], sep: Parser[sep], k: ac -> Parser[ac]): Parser[ac] = 
        use TextParser.flatMap;
        let* ans = optional(p);
        match ans { 
            case None => k(zero)
            case Some(a) => {
                let* ac = manyWith(append, zero, seqRight(sep, p));
                let ac1 = append(a,ac);
                return(ac1)
            }
        }

    pub def sepBy(p: Parser[a], sep: Parser[sep]): Parser[List[a]] = 
        sepByWith((x,xs) -> x :: xs, Nil, p, sep) |> swapError("sepBy")


    pub def sepBy1With(append: (a,ac) -> ac, zero: ac, p: Parser[a], sep: Parser[sep]): Parser[ac] = 
        use TextParser.flatMap;
        let* a = p;
        let* ac = manyWith(append, zero, seqRight(sep, p));
        let ac1 = append(a, ac);
        return(ac1) |> swapError("sepBy1With")
        
    pub def sepBy1(p: Parser[a], sep: Parser[sep]): Parser[List[a]] = 
        sepBy1With((x,xs) -> x :: xs, Nil, p, sep) |> swapError("sepBy1")

    pub def endByWith(append: (a,ac) -> ac, zero: ac, p: Parser[a], sep: Parser[sep]): Parser[ac] = 
        manyWith(append, zero, seqLeft(p, sep)) |> nestError("endByWith")

    pub def endBy(p: Parser[a], sep: Parser[sep]): Parser[List[a]] = 
        endByWith((x,xs) -> x :: xs, Nil, p, sep) |> swapError("endBy")

    pub def endBy1With(append: (a,ac) -> ac, zero: ac, p: Parser[a], sep: Parser[sep]): Parser[ac] = 
        many1With(append, zero, seqLeft(p, sep)) |> swapError("endBy1With")

    pub def endBy1(p: Parser[a], sep: Parser[sep]): Parser[List[a]] = 
        endBy1With((x,xs) -> x :: xs, Nil, p, sep) |> swapError("endBy1")


    /// Optionally terminated with sep.
    pub def sepEndByWith(append: (a,ac) -> ac, zero: ac, p: Parser[a], sep: Parser[sep]): Parser[ac] = 
        seqLeft(sepByWith(append, zero, p, sep), optional(sep)) |> swapError("sepEndByWith")

    /// Optionally terminated with sep.
    pub def sepEndBy(p: Parser[a], sep: Parser[sep]): Parser[List[a]] = 
        sepEndByWith((x,xs) -> x :: xs, Nil, p, sep) |> swapError("sepEndBy")


    pub def sepEndBy1With(append: (a,ac) -> ac, zero: ac, p: Parser[a], sep: Parser[sep]): Parser[ac] = 
        seqLeft(sepBy1With(append, zero, p, sep), optional(sep)) |> swapError("sepEndBy1With")

    pub def sepEndBy1(p: Parser[a], sep: Parser[sep]): Parser[List[a]] = 
        sepEndBy1With((x,xs) -> x :: xs, Nil, p, sep) |> swapError("sepEndBy1")


    /// p1 must succeed, p2 parsers zero-or-more
    pub def manyTill2With(f: (a,ac) -> ac, zero: ac, p1: Parser[a], p2: Parser[a], endp: Parser[sep]): Parser[ac] = 
        use TextParser.flatMap;
        let* a = p1;
        let* ac = manyTillWith(f, zero, p2, endp);
        return(f(a,ac)) |> swapError("manyTill2With")

    /// p1 must succeed, p2 parsers zero-or-more
    pub def manyTill2(p1: Parser[a], p2: Parser[a], endp: Parser[sep]): Parser[List[a]] = 
        manyTill2With((x,xs) -> x :: xs, Nil, p1, p2, endp) |> swapError("manyTill2")

    /// p1 must succeed, p2 parsers one-or-more
    pub def many1Till2With(append: (a,ac) -> ac, zero: ac, p1: Parser[a], p2: Parser[a], endp: Parser[sep]): Parser[ac] = 
        use TextParser.flatMap;
        let* a = p1;
        let* ac = many1TillWith(append, zero, p2, endp);
        let ac1 = append(a, ac);
        return(ac1) |> swapError("many1Till2With")

    /// p1 must succeed, p2 parsers one-or-more
    pub def many1Till2(p1: Parser[a], p2: Parser[a], endp: Parser[sep]): Parser[List[a]] = 
        many1Till2With((x,xs) -> x :: xs, Nil, p1, p2, endp) |> swapError("many1Till2")

    /// Count how many times parser `p` succeeds
    pub def ntimes(p: Parser[a]): Parser[Int32] = 
        manyWith((_,ac) -> ac + 1, 0, p)

    /// Count how many times parser `p` succeeds
    pub def ntimesSepBy(p: Parser[a], sep: Parser[sep]): Parser[Int32] = 
        sepByWith((_,ac) -> ac + 1, 0, p, sep)

    /// Count how many times parser `p` succeeds
    pub def ntimesSepEndBy(p: Parser[a], sep: Parser[sep]): Parser[Int32] = 
        sepEndByWith((_,ac) -> ac + 1, 0, p, sep)


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

    pub def tuple2(p1: Parser[a], p2: Parser[b]): Parser[(a,b)] =
        use TextParser.flatMap;
        let* a = p1; 
        let* b = p2;
        return((a,b))

    pub def tuple3(p1: Parser[a], p2: Parser[b], p3: Parser[c]): Parser[(a,b,c)] = 
        use TextParser.flatMap;
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        return((a,b,c))

    pub def tuple4(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d]): Parser[(a,b,c,d)] = 
        use TextParser.flatMap;
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        return((a,b,c,d))


    pub def tuple5(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e]): Parser[(a,b,c,d,e)] = 
        use TextParser.flatMap;
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        return((a,b,c,d,e))

    pub def tuple6(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f]): Parser[(a,b,c,d,e,f)] = 
        use TextParser.flatMap;
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        let* f = p6;
        return((a,b,c,d,e,f))

    pub def tuple7(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f], p7: Parser[g]): Parser[(a,b,c,d,e,f,g)] = 
        use TextParser.flatMap;
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        let* f = p6;
        let* g = p7;
        return((a,b,c,d,e,f,g))

    pub def tuple8(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f], p7: Parser[g], p8: Parser[h]): Parser[(a,b,c,d,e,f,g,h)] = 
        use TextParser.flatMap;
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        let* f = p6;
        let* g = p7;
        let* h = p8;
        return((a,b,c,d,e,f,g,h))

    pub def tuple9(p1: Parser[a], p2: Parser[b], p3: Parser[c], p4: Parser[d], p5: Parser[e], p6: Parser[f], p7: Parser[g], p8: Parser[h], p9: Parser[i]): Parser[(a,b,c,d,e,f,g,h,i)] = 
        use TextParser.flatMap;
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        let* f = p6;
        let* g = p7;
        let* h = p8;
        let* i = p9;
        return((a,b,c,d,e,f,g,h,i))

}
