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

use TextParser.Parser;
use TextParser.Parser.Parser;
use TextParser.Index;
use TextParser.ParseError;
use TextParser.{bind, return, alt, fail, nestError, apply1, makeParseError, nestErrorMessage, map};

///
/// Character level parsers.
///

namespace TextParser/Combinators {


    
    pub def seqLeft(pa: Parser[a], pb: Parser[b]): Parser[a] =
        bind(pa, a -> bind(pb, _ -> return(a)))

    pub def seqRight(pa: Parser[a], pb: Parser[b]): Parser[b] =
        bind(pa, _ -> bind(pb, b -> return(b)))


    pub def satisfies(p: Parser[ans], test: ans -> Bool): Parser[ans] = 
        bind(p, ans -> if (test(ans)) return(ans) else fail())


    // pub def countWith(f: (a, ac) -> ac, i: Int32, p: Parser[a]): Parser[ac] = 


    // def countWithHelper(f: (a, ac) -> ac, i: Int32, p: Parser[ans], inp: String, pos: Index, fk: ParseError -> Result[ans], sk: (List[ans], Index) -> ListAnswer[ans]): ListAnswer[ans] = 
    //     if (i <= 0)
    //         sk(Nil, pos)
    //     else 
    //         match apply1(p, inp, pos) {
    //             case Err(e) => fk(e)
    //             case Ok(a, pos1) => countHelper(i-1, p, inp, pos1, fk, (xs,posk) -> sk(a :: xs, posk))
    //         }


    pub def count(i: Int32, p: Parser[ans]): Parser[List[ans]] = 
        Parser((inp, pos) -> countHelper(i, p, inp, pos, e -> Err(nestErrorMessage("count", e)), (xs, pos1) -> Ok(xs, pos1)))

    type alias ListAnswer[ans] = Result[(List[ans], Index), ParseError]

    def countHelper(i: Int32, p: Parser[ans], inp: String, pos: Index, fk: ParseError -> ListAnswer[ans], sk: (List[ans], Index) -> ListAnswer[ans]): ListAnswer[ans] = 
        if (i <= 0)
            sk(Nil, pos)
        else 
            match apply1(p, inp, pos) {
                case Err(e) => fk(e)
                case Ok(a, pos1) => countHelper(i-1, p, inp, pos1, fk, (xs,posk) -> sk(a :: xs, posk))
            }
    

    pub def notFollowedBy(p: Parser[ans]): Parser[Unit] = 
        Parser((inp, pos) -> match apply1(p, inp, pos) {
            case Err(_) => Ok((), pos)
            case Ok(_,_) => Err(makeParseError(inp, pos, "notFollowedBy"))
        })

    pub def option(one: ans, p: Parser[ans]): Parser[ans] = 
        Parser((inp, pos) -> match apply1(p, inp, pos) { 
                case Err(_) => Ok(one, pos)
                case Ok(ans, pos1) => Ok(ans, pos1)
            })

    pub def optional(p: Parser[ans]): Parser[Option[ans]] = 
        alt(map(x -> Some(x), p), return(None))


    pub def ignore(p: Parser[ans]): Parser[Unit] = 
        map(_ -> (), p)

    ///
    /// Parser the input with `p`, the if the parser succeed return None. 
    ///
    pub def assertNone(p: Parser[a]): Parser[Option[ans]] = 
        map(_ -> None, p)
    



    pub def between(open: Parser[z1], close: Parser[z2], p: Parser[ans]): Parser[ans] = 
        seqRight(open, seqLeft(p, close))


    pub def many(p: Parser[ans]): Parser[List[ans]] = 
        Parser((inp, pos) -> manyHelper(p, inp, pos, (xs, pos1)-> Ok(xs, pos1)))


    pub def many1(p: Parser[ans]): Parser[List[ans]] = 
        bind(p, v1 -> 
            bind(many(p), vs -> return(v1 :: vs))) |> nestError("many1")


    def manyHelper(p: Parser[ans], inp: String, pos: Index, sk: (List[ans], Index) -> ListAnswer[ans]): ListAnswer[ans] = 
        match apply1(p, inp, pos) {
            case Err(_) => sk(Nil, pos)
            case Ok(a, pos1) => manyHelper(p, inp, pos1, (xs,posk) -> sk(a :: xs, posk))
        }

    type alias SkipAnswer = Result[(Unit, Index), ParseError]

    pub def skipMany(p: Parser[ans]): Parser[Unit] = 
        Parser((inp, pos) -> skipManyHelper(p, inp, pos, pos1 -> Ok((), pos1)))

    pub def skipMany1(p: Parser[ans]): Parser[Unit] = 
        seqRight(p, skipMany(p))


    def skipManyHelper(p: Parser[ans], inp: String, pos: Index, sk: Index -> SkipAnswer): SkipAnswer = 
        match apply1(p, inp, pos) {
            case Err(_) => sk(pos)
            case Ok(_, pos1) => skipManyHelper(p, inp, pos1, posk -> sk(posk))
        }

    pub def sepBy(p: Parser[ans], sep: Parser[sep]): Parser[List[ans]] = 
        Parser((inp, pos) -> sepByHelper(p, sep, inp, pos, (xs,pos1)-> Ok(xs, pos1)))


    def sepByHelper(p: Parser[ans], sep: Parser[sep], inp: String, pos: Index, sk: (List[ans], Index) -> ListAnswer[ans]): ListAnswer[ans] = 
        match apply1(p, inp, pos) {
            case Err(_) => sk(Nil, pos)
            case Ok(a, pos1) => manyHelper(seqRight(sep, p), inp, pos1, (xs,posk) -> sk(a :: xs, posk))
        }

    pub def sepBy1(p: Parser[ans], sep: Parser[sep]): Parser[List[ans]] = 
        bind(p, v1 -> 
            bind(many(seqRight(sep,p)), vs -> return(v1 :: vs))) |> nestError("sepBy1")

    pub def endBy(p: Parser[ans], sep: Parser[sep]): Parser[List[ans]] = 
        many(seqLeft(p, sep))

    pub def endBy1(p: Parser[ans], sep: Parser[sep]): Parser[List[ans]] = 
        many1(seqLeft(p, sep)) |> nestError("endBy1")

    pub def sepEndBy(p: Parser[ans], sep: Parser[sep]): Parser[List[ans]] = 
        Parser((inp, pos) -> sepEndByHelper(p, sep, inp, pos, (xs,pos1)-> Ok(xs, pos1)))

    def sepEndByHelper(p: Parser[ans], sep: Parser[sep], inp: String, pos: Index, sk: (List[ans], Index) -> ListAnswer[ans]): ListAnswer[ans] = 
        match apply1(p, inp, pos) {
            case Err(_) => sk(Nil, pos)
            case Ok(a, pos1) => match apply1(sep, inp, pos1) {
                case Err(_) => sk(a :: Nil, pos)
                case Ok(_, pos2) => sepEndByHelper(p, sep, inp, pos2, (xs,posk) -> sk(a :: xs, posk))
            }
        }

    pub def sepEndBy1(p: Parser[ans], sep: Parser[sep]): Parser[List[ans]] = 
        Parser((inp, pos) -> match apply1(p, inp, pos) {
            case Err(e) => Err(nestErrorMessage("sepEndBy1", e))
            case Ok(a, pos1) => match apply1(sep, inp, pos1) {
                case Err(_) => Ok(a :: Nil, pos1)
                case Ok(_, pos2) => sepEndByHelper(p, sep, inp, pos2, (xs,posk) -> Ok(a :: xs, posk))
            }
        })

    pub def choose(p: Parser[Option[a]]): Parser[List[a]] = 
        Parser((inp, pos) -> chooseHelper(p, inp, pos, (xs, posk) -> Ok(xs, posk)))
    
    def chooseHelper(p: Parser[Option[a]], inp: String, pos: Index, sk: (List[a], Index) ->  ListAnswer[a]): ListAnswer[a] = 
        match apply1(p, inp, pos) {
            case Err(_) => sk(Nil, pos)
            case Ok(Some(a), pos1) => chooseHelper(p, inp, pos1, (xs,posk) -> sk(a :: xs, posk))
            case Ok(None, pos1) => chooseHelper(p, inp, pos1, (xs,posk) -> sk(xs, posk))
        }

    pub def choice(ps: List[Parser[ans]]): Parser[ans] = 
        Parser((inp, pos) -> match ps {
                case Nil => Err(makeParseError(inp, pos, "choice"))
                case p :: rs =>  match apply1(p, inp, pos) {
                    case Ok(a, pos1) => Ok(a, pos1)
                    case Err(_) => apply1(choice(rs), inp, pos)
                }
            })

    
    pub def manyTillWith(f: (List[a], e) -> ans, p: Parser[a], endp: Parser[e]): Parser[ans] =
        let fk = e -> Err(e);
        let sk = (xs, end1, pos) -> Ok(f(xs, end1), pos);
        Parser((inp, pos) -> manyTillWithHelper(p, endp, inp, pos, fk, sk))

    
    def manyTillWithHelper(p: Parser[a], endp: Parser[e], inp: String, pos: Index, fk: ParseError -> Result[ans, ParseError], sk: (List[a], e, Index) -> Result[ans, ParseError]): Result[ans, ParseError] = 
        match apply1(endp, inp, pos) {
            case Ok(end1, pos1) => sk(Nil, end1, pos1)
            case Err(_) => match apply1(p, inp, pos) {
                case Err(e) => fk(e)
                case Ok(x, pos1) => manyTillWithHelper(p, endp, inp, pos1, fk, (xs, end2, pos2) -> sk(x :: xs, end2, pos2))
            }
        }

    pub def manyTill(p: Parser[a], endp: Parser[e]): Parser[List[a]] =
        manyTillWith((xs,_) -> xs, p, endp) 


    pub def manyTillApply(p: Parser[a], endp: Parser[e], f: (List[a], e) -> ans): Parser[ans] =
        manyTillWith(f, p, endp)

    pub def many1Till(p: Parser[a], end: Parser[sep]): Parser[List[a]] = 
        pipe2(p, manyTill(p,end), (v, vs) -> v :: vs)

    /// p1 must succeed
    pub def manyTill2(p1: Parser[a], p2: Parser[a], end: Parser[sep]): Parser[List[a]] = 
        pipe2(p1, manyTill(p2,end), (v, vs) -> v :: vs)

    /// p1 must succeed, p2 must succeed at least once.
    pub def many1Till2(p1: Parser[a], p2: Parser[a], end: Parser[sep]): Parser[List[a]] = 
        pipe2(p1, many1Till(p2,end), (v, vs) -> v :: vs)


    pub def skipManyTill(p: Parser[a], end: Parser[e]): Parser[Unit] = 
        let fk = e -> Err(nestErrorMessage("skipManyTill", e));
        let sk = pos -> Ok((), pos);
        Parser((inp, pos) -> skipManyTillHelper(p, end, inp, pos, fk, sk))

    def skipManyTillHelper(p: Parser[a], end: Parser[e], inp: String, pos: Index, fk: ParseError -> Result[(Unit, Index), ParseError], sk: Index -> Result[(Unit, Index), ParseError]): Result[(Unit, Index), ParseError] = 
        match apply1(end, inp, pos) {
            case Ok(_, pos1) => sk(pos1)
            case Err(_) => match apply1(p, inp, pos) {
                case Err(e) => fk(e)
                case Ok(_, pos1) => skipManyTillHelper(p, end, inp, pos1, fk, sk)
            }
        }

    pub def skipMany1Till(p: Parser[a], end: Parser[sep]): Parser[Unit] = 
        seqRight(p, skipManyTill(p, end))

    /// p1 must succeed
    pub def skipManyTill2(p1: Parser[a], p2:Parser[a], end: Parser[sep]): Parser[Unit] = 
        seqRight(p1, skipManyTill(p2, end))

    /// p1 must succeed, p2 must succeed at least once.
    pub def skipMany1Till2(p1: Parser[a], p2:Parser[a], end: Parser[sep]): Parser[Unit] = 
        seqRight(p1, skipMany1Till(p2, end))
    

    
    pub def skipped(p: Parser[Unit]): Parser[String] = 
        Parser((inp,pos) -> match apply1(p, inp, pos) {
            case Err(e) => Err(nestErrorMessage("skipped", e))
            case Ok(_, pos1) => match TextParser/Internal.trySlice(pos, pos1, inp) {
                case None => Err(makeParseError(inp, pos, "skipped"))
                case Some(s) => Ok(s, pos1)
            }
        })

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

    pub def string2(p1: Parser[String], p2: Parser[String]): Parser[String] = 
        pipe2(p1, p2, (a,b) -> a + b)

    pub def string3(p1: Parser[String], p2: Parser[String], p3: Parser[String]): Parser[String] = 
        pipe3(p1, p2, p3, (a,b,c) -> a + b + c)

    pub def string4(p1: Parser[String], p2: Parser[String], p3: Parser[String], p4: Parser[String]): Parser[String] = 
        pipe4(p1, p2, p3, p4, (a,b,c,d) -> a + b + c + d)

    pub def string5(p1: Parser[String], p2: Parser[String], p3: Parser[String], p4: Parser[String], p5: Parser[String]): Parser[String] = 
        pipe5(p1, p2, p3, p4, p5, (a,b,c,d, e) -> a + b + c + d + e)


}