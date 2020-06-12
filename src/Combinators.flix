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

use TextParser.{TextParser};
use TextParser.{return, fail, failure, map, alt, bind, ifThenElse};

namespace TextParser/Combinators {

    // Move into "Text" module when available.
    pub def newline(): TextParser[Unit, t] = 
        use TextParser.flatMap;
        use Text/Regex/Pattern.{compile};
        let* patt = liftResult(compile("\R"));
        let* _ = TextParser.lookingAt(patt);
        return()


    pub def liftOption(x: Option[r]): TextParser[r, t] =  match x {
        case Some(a) => return(a)
        case None    => fail()
    }

    pub def liftResult(x: Result[r, e]): TextParser[r, t] =  match x {
        case Ok(a) => return(a)
        case Err(_) => fail()
    }

    pub def optional(p: TextParser[a, t]): TextParser[Option[a], t] = 
        alt(map(Some,p), return(None))

    pub def optionalWithDefault(p: TextParser[a, t], d: a): TextParser[a, t] = 
        alt(p, return(d))

    pub def seqLeft(p: TextParser[r, t], q: TextParser[s, t]): TextParser[r, t] =
        use TextParser.flatMap;
        let* a = p;
        let* _ = q;
        return(a)

    pub def seqRight(p: TextParser[r, t], q: TextParser[s, t]): TextParser[s, t] =
        use TextParser.flatMap;
        let* _ = p;
        let* b = q;
        return(b)

    pub def ignore(p: TextParser[r, t]): TextParser[Unit, t] =
        map(constant(), p)

    pub def between(openp: TextParser[u, t], closep: TextParser[v, t], p: TextParser[r, t]): TextParser[r, t] = 
        use TextParser.flatMap;
        let* _ = openp;
        let* a = p;
        let* _ = closep;
        return(a)

    pub def satisfies(p: TextParser[r, t], test: r -> Bool): TextParser[r, t] = 
        use TextParser.flatMap;
        let* a = p;
        if (test(a))
            return(a)
        else
            failure("satisfies")

    pub def choice(ps: List[TextParser[a, t]]): TextParser[a, t] = 
        match ps { 
            case Nil => failure("choice")
            case p :: rs => ifThenElse(p, return, choice(rs))
        }

    pub def countWith(append: (a, ac) -> ac, acc: ac, n: Int32, p: TextParser[a, t]): TextParser[ac, t] = 
        use TextParser.flatMap;
        if (n <= 0)
            return(acc)
        else {
            let* a = p;
            countWith(append, append(a, acc), n-1, p)
        }

    pub def count(n: Int32, p: TextParser[r, t]): TextParser[List[r], t] = 
        use Data/Transient/DList.{empty, snoc, toList};
        map(toList, countWith((x,xs) -> snoc(xs,x), empty(), n, p))
    


    pub def manyWith(append: (a,ac) -> ac, acc: ac, p: TextParser[a, t]): TextParser[ac, t] = 
        ifThenElse(p, a -> {let acc1 = append(a, acc); manyWith(append, acc1, p)}, return(acc))


    pub def many(p: TextParser[r, t]): TextParser[List[r], t] = 
        use Data/Transient/DList.{empty, snoc, toList};
        map(toList, manyWith((x,xs) -> snoc(xs,x), empty(), p))
        
    pub def many1With(append: (a,ac) -> ac, acc: ac, p: TextParser[a, t]): TextParser[ac, t] = 
        bind(p, x -> manyWith(append, append(x,acc), p))
        
    pub def many1(p: TextParser[r, t]): TextParser[List[r], t] = 
        use Data/Transient/DList.{empty, snoc, toList};
        map(toList, many1With((x,xs) -> snoc(xs,x), empty(), p))


    /// Count how many times parser `p` succeeds
    pub def ntimes(p: TextParser[r, t]): TextParser[Int32, t] = 
        manyWith((_,ac) -> ac + 1, 0, p)


    pub def manyTillWith(append: (a,ac) -> ac, acc: ac, p: TextParser[a,t], endp: TextParser[end,t]): TextParser[ac, t] = 
        ifThenElse(endp, _ -> return(acc), bind(p, a -> manyTillWith(append, append(a, acc), p, endp)))

    pub def manyTill(p: TextParser[r,t], endp: TextParser[end,t]): TextParser[List[r], t] = 
        use Data/Transient/DList.{empty, snoc, toList};
        map(toList, manyTillWith((x,xs) -> snoc(xs,x), empty(), p, endp))
        



    // /// Count how many times parser `p` succeeds
    // pub def ntimesSepBy(p: TextParser[a], sep: TextParser[sep]): TextParser[Int32] = 
    //     sepByWith((_,ac) -> ac + 1, 0, p, sep)

    // /// Count how many times parser `p` succeeds
    // pub def ntimesSepEndBy(p: TextParser[a], sep: TextParser[sep]): TextParser[Int32] = 
    //     sepEndByWith((_,ac) -> ac + 1, 0, p, sep)



    pub def skipMany(p: TextParser[r,t]): TextParser[Unit, t] = 
        ifThenElse(p, _ -> skipMany(p), return())

    pub def skipMany1(p: TextParser[r,t]): TextParser[Unit, t] = 
        bind(p, _ -> skipMany(p))


    pub def skipManyTill(p: TextParser[r,t], endp: TextParser[end,t]): TextParser[Unit, t] = 
        ifThenElse(endp, _ -> return(), bind(p, _ -> skipManyTill(p, endp)))

    pub def skipMany1Till(p: TextParser[r,t], endp: TextParser[end,t]): TextParser[Unit, t] = 
        bind(p, _ -> skipManyTill(p, endp))




    // pub def manyTillWith(append: (a, ac) -> ac, zero: ac, p: TextParser[a], endp: TextParser[e]): TextParser[ac] = 
    //     manyTillWithHelper(append, zero, p, endp, return) |> nestError("manyTillWith")


    // def manyTillWithHelper(append: (a, ac) -> ac, zero: ac, p: TextParser[a], endp: TextParser[e], sk: ac -> Parser[ac]): TextParser[ac] = 
    //     use TextParser.flatMap;
    //     let* done = optional(endp);
    //     match done { 
    //         case Some(_) => sk(zero)
    //         case None => 
    //             bindEither(p
    //                 , failure
    //                 , x -> manyTillWithHelper(append, zero, p, endp, ac -> { let ac1 = append(x, ac); sk(ac1)})
    //             )                
    //     }
        
    // pub def manyTill(p: TextParser[a], endp: TextParser[z]): TextParser[List[a]] = 
    //     manyTillWithHelper((x,xs) -> x :: xs, Nil, p, endp, return) |> swapError("manyTill")

    // pub def many1TillWith(f: (a, ac) -> ac, zero: ac, p: TextParser[a], endp: TextParser[e]): TextParser[ac] = 
    //     use TextParser.flatMap;
    //     let* a = p;
    //     let* ac = manyTillWith(f, zero, p, endp);
    //     return(f(a, ac)) |> swapError("many1TillWith")

    // pub def many1Till(p: TextParser[a], endp: TextParser[e]): TextParser[List[a]] = 
    //     many1TillWith((x,xs) -> x :: xs, Nil, p, endp) |> swapError("many1Till")

    // pub def skipMany(p: TextParser[a]): TextParser[Unit] = 
    //     manyWith((_,_) -> (), (), p)

    // pub def skipMany1(p: TextParser[a]): TextParser[Unit] = 
    //     many1With((_,ac) -> ac, (), p) |> swapError("skipMany1")

    // pub def skipManyTill(p: TextParser[a], endp: TextParser[e]): TextParser[Unit] = 
    //     manyTillWith((_,ac) -> ac, (), p, endp)

    // pub def skipMany1Till(p: TextParser[a], endp: TextParser[e]): TextParser[Unit] = 
    //     many1TillWith((_,ac) -> ac, (), p, endp)

    
    // pub def sepByWith(append: (a,ac) -> ac, zero: ac, p: TextParser[a], sep: TextParser[sep]): TextParser[ac] = 
    //     sepByWithHelper(append, zero, p, sep, return) |> nestError("sepByWith")

    // def sepByWithHelper(append: (a, ac) -> ac, zero: ac, p: TextParser[a], sep: TextParser[sep], k: ac -> Parser[ac]): TextParser[ac] = 
    //     use TextParser.flatMap;
    //     let* ans = optional(p);
    //     match ans { 
    //         case None => k(zero)
    //         case Some(a) => {
    //             let* ac = manyWith(append, zero, seqRight(sep, p));
    //             let ac1 = append(a,ac);
    //             return(ac1)
    //         }
    //     }

    // pub def sepBy(p: TextParser[a], sep: TextParser[sep]): TextParser[List[a]] = 
    //     sepByWith((x,xs) -> x :: xs, Nil, p, sep) |> swapError("sepBy")


    // pub def sepBy1With(append: (a,ac) -> ac, zero: ac, p: TextParser[a], sep: TextParser[sep]): TextParser[ac] = 
    //     use TextParser.flatMap;
    //     let* a = p;
    //     let* ac = manyWith(append, zero, seqRight(sep, p));
    //     let ac1 = append(a, ac);
    //     return(ac1) |> swapError("sepBy1With")
        
    // pub def sepBy1(p: TextParser[a], sep: TextParser[sep]): TextParser[List[a]] = 
    //     sepBy1With((x,xs) -> x :: xs, Nil, p, sep) |> swapError("sepBy1")

    // pub def endByWith(append: (a,ac) -> ac, zero: ac, p: TextParser[a], sep: TextParser[sep]): TextParser[ac] = 
    //     manyWith(append, zero, seqLeft(p, sep)) |> nestError("endByWith")

    // pub def endBy(p: TextParser[a], sep: TextParser[sep]): TextParser[List[a]] = 
    //     endByWith((x,xs) -> x :: xs, Nil, p, sep) |> swapError("endBy")

    // pub def endBy1With(append: (a,ac) -> ac, zero: ac, p: TextParser[a], sep: TextParser[sep]): TextParser[ac] = 
    //     many1With(append, zero, seqLeft(p, sep)) |> swapError("endBy1With")

    // pub def endBy1(p: TextParser[a], sep: TextParser[sep]): TextParser[List[a]] = 
    //     endBy1With((x,xs) -> x :: xs, Nil, p, sep) |> swapError("endBy1")


    // /// Optionally terminated with sep.
    // pub def sepEndByWith(append: (a,ac) -> ac, zero: ac, p: TextParser[a], sep: TextParser[sep]): TextParser[ac] = 
    //     seqLeft(sepByWith(append, zero, p, sep), optional(sep)) |> swapError("sepEndByWith")

    // /// Optionally terminated with sep.
    // pub def sepEndBy(p: TextParser[a], sep: TextParser[sep]): TextParser[List[a]] = 
    //     sepEndByWith((x,xs) -> x :: xs, Nil, p, sep) |> swapError("sepEndBy")


    // pub def sepEndBy1With(append: (a,ac) -> ac, zero: ac, p: TextParser[a], sep: TextParser[sep]): TextParser[ac] = 
    //     seqLeft(sepBy1With(append, zero, p, sep), optional(sep)) |> swapError("sepEndBy1With")

    // pub def sepEndBy1(p: TextParser[a], sep: TextParser[sep]): TextParser[List[a]] = 
    //     sepEndBy1With((x,xs) -> x :: xs, Nil, p, sep) |> swapError("sepEndBy1")


    // /// p1 must succeed, p2 parsers zero-or-more
    // pub def manyTill2With(f: (a,ac) -> ac, zero: ac, p1: TextParser[a], p2: TextParser[a], endp: TextParser[sep]): TextParser[ac] = 
    //     use TextParser.flatMap;
    //     let* a = p1;
    //     let* ac = manyTillWith(f, zero, p2, endp);
    //     return(f(a,ac)) |> swapError("manyTill2With")

    // /// p1 must succeed, p2 parsers zero-or-more
    // pub def manyTill2(p1: TextParser[a], p2: TextParser[a], endp: TextParser[sep]): TextParser[List[a]] = 
    //     manyTill2With((x,xs) -> x :: xs, Nil, p1, p2, endp) |> swapError("manyTill2")

    // /// p1 must succeed, p2 parsers one-or-more
    // pub def many1Till2With(append: (a,ac) -> ac, zero: ac, p1: TextParser[a], p2: TextParser[a], endp: TextParser[sep]): TextParser[ac] = 
    //     use TextParser.flatMap;
    //     let* a = p1;
    //     let* ac = many1TillWith(append, zero, p2, endp);
    //     let ac1 = append(a, ac);
    //     return(ac1) |> swapError("many1Till2With")

    // /// p1 must succeed, p2 parsers one-or-more
    // pub def many1Till2(p1: TextParser[a], p2: TextParser[a], endp: TextParser[sep]): TextParser[List[a]] = 
    //     many1Till2With((x,xs) -> x :: xs, Nil, p1, p2, endp) |> swapError("many1Till2")





    pub def chainl1(p: TextParser[r, t], op: TextParser[(r, r) -> r, t]): TextParser[r, t] = 
        bind(p, v1 -> chainl1Helper(p, op, v1))
        
    def chainl1Helper(p: TextParser[r, t], op: TextParser[(r, r) -> r, t], x: r): TextParser[r, t] = 
        alt(bind(op, f -> bind(p, v1 -> chainl1Helper(p, op, f(x, v1)))), 
            return(x))

    pub def chainl(p: TextParser[r, t], op: TextParser[(r, r) -> r, t], x: r): TextParser[r, t] = 
        alt(chainl1(p,op), return(x))


    def chainr1Helper(p: TextParser[r, t], op: TextParser[(r, r) -> r, t], x: r): TextParser[r, t] = 
        alt(chainr1Step(p, op, x), return(x))

    def chainr1Step(p: TextParser[r, t], op: TextParser[(r, r) -> r, t], x: r): TextParser[r, t] = 
        bind(op, f -> 
            bind(bind(p, v1 -> chainr1Helper(p, op, v1)), y -> 
                bind(chainr1Helper(p,op, f(x,y)), ans -> return(ans))))

    pub def chainr1(p: TextParser[r, t], op: TextParser[(r, r) -> r, t]): TextParser[r, t] = 
        bind(p, v1 -> chainr1Helper(p, op, v1))

    pub def chainr(p: TextParser[r, t], op: TextParser[(r, r) -> r, t], x: r): TextParser[r, t] = 
        alt(chainr1(p, op), return(x))

    pub def tuple2(p1: TextParser[a, t], p2: TextParser[b, t]): TextParser[(a,b), t] =
        use TextParser.flatMap;
        let* a = p1; 
        let* b = p2;
        return((a,b))

    pub def tuple3(p1: TextParser[a, t], p2: TextParser[b, t], p3: TextParser[c, t]): TextParser[(a,b,c), t] = 
        use TextParser.flatMap;
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        return((a,b,c))

    pub def tuple4(p1: TextParser[a, t], p2: TextParser[b, t], p3: TextParser[c, t], p4: TextParser[d, t]): TextParser[(a,b,c,d), t] = 
        use TextParser.flatMap;
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        return((a,b,c,d))


    pub def tuple5(p1: TextParser[a, t], p2: TextParser[b, t], p3: TextParser[c, t], p4: TextParser[d, t], p5: TextParser[e, t]): TextParser[(a,b,c,d,e), t] = 
        use TextParser.flatMap;
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        return((a,b,c,d,e))

    pub def tuple6(p1: TextParser[a, t], p2: TextParser[b, t], p3: TextParser[c, t], p4: TextParser[d, t], p5: TextParser[e, t], p6: TextParser[f, t]): TextParser[(a,b,c,d,e,f), t] = 
        use TextParser.flatMap;
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        let* f = p6;
        return((a,b,c,d,e,f))

    pub def tuple7(p1: TextParser[a, t], p2: TextParser[b, t], p3: TextParser[c, t], p4: TextParser[d, t], p5: TextParser[e, t], p6: TextParser[f, t], p7: TextParser[g, t]): TextParser[(a,b,c,d,e,f,g), t] = 
        use TextParser.flatMap;
        let* a = p1; 
        let* b = p2;
        let* c = p3;
        let* d = p4;
        let* e = p5;
        let* f = p6;
        let* g = p7;
        return((a,b,c,d,e,f,g))

    pub def tuple8(p1: TextParser[a, t], p2: TextParser[b, t], p3: TextParser[c, t], p4: TextParser[d, t], p5: TextParser[e, t], p6: TextParser[f, t], p7: TextParser[g, t], p8: TextParser[h, t]): TextParser[(a,b,c,d,e,f,g,h), t] = 
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

    pub def tuple9(p1: TextParser[a, t], p2: TextParser[b, t], p3: TextParser[c, t], p4: TextParser[d, t], p5: TextParser[e, t], p6: TextParser[f, t], p7: TextParser[g, t], p8: TextParser[h, t], p9: TextParser[i, t]): TextParser[(a,b,c,d,e,f,g,h,i), t] = 
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
