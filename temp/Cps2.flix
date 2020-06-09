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

use Cps2.ErrMsg;

/// Adding position to Cps1's error.

namespace Cps2 {

    type alias ErrMsg = { msg: String, pos: Int32 }

    type alias NextRes[t] = Int32 -> Result[t, ErrMsg]

    type alias Parser1[t] = String -> Int32 -> Result[t, ErrMsg] 

    type alias Success[r, t] = r -> NextRes[t] -> Parser1[t]

    pub opaque type Parser[r, t] = Success[r, t] -> NextRes[t] -> Parser1[t]

    pub def return(x: a): Parser[a, t] = 
        Parser((succ, next, inp, pos) -> succ(x, next, inp, pos))

    pub def fail(): Parser[r, t] = 
        Parser((_, next, _, pos) -> next(pos))

    def apply1 (p: Parser[r, t], sk: Success[r, t], fk: NextRes[t], inp: String, pos: Int32): Result[t, ErrMsg] = 
        let Parser(pf) = p;
        pf(sk, fk, inp, pos)

    pub def map(f: a -> b, p: Parser[a, t]): Parser[b, t] =
        bind(p, x -> return(f(x)))

    pub def ap(mf: Parser[a -> b, t], ma: Parser[a, t]): Parser[b, t] = 
        let* f = mf;
        let* a = ma;
        return(f(a))

    pub def bind(ma: Parser[q, t], k: q -> Parser[r, t]):  Parser[r, t] =
        Parser(succ -> apply1(ma, x -> apply1(k(x), succ)))

    pub def flatMap(k: q -> Parser[r, t], ma: Parser[q, t]):  Parser[r, t] = bind(ma, k)

    pub def alt(p: Parser[a, t], q: Parser[a, t]): Parser[a, t] = 
        Parser((sc, nc, inp, pos) -> apply1(p, sc, apply1(q, sc, nc, inp), inp, pos))

    pub def seqLeft(p: Parser[a, t], q: Parser[b, t]): Parser[a, t] = 
        let* a = p;
        let* _ = q;
        return(a)
    
    pub def seqRight(p: Parser[a, t], q: Parser[b, t]): Parser[b, t] = 
        let* _ = p;
        let* b = q;
        return(b)
        
    
    pub def symbol(sym: String): Parser[String, t] = 
        Parser((sc, nc, inp, pos) ->
            let symlen = String.length(sym);
            let s1 = String.slice(inp, pos, pos + symlen);
            if (s1 == sym)
                sc(sym, nc, inp, pos + symlen)
            else
                nc(pos)
        )

    pub def optional(p: Parser[a, t]): Parser[Option[a], t] = 
        alt(map(Some,p), return(None))

    pub def optionWithDefault(p: Parser[a, t], d: a): Parser[a, t] = 
        alt(p, return(d))

    pub def endOfInput() : Parser[Unit, t] =
        Parser((sc, nc, inp, pos) ->
            let len = String.length(inp);
            if (pos >= len)
                sc((), nc, inp, pos)
            else
                nc(pos)
        )

    pub def ignore(p: Parser[r, t]) : Parser[Unit, t] =
        map(constant(), p)

    pub def swapError(p: Parser[r,t], msg: String) : Parser[r, t] =
        Parser((sc, _, inp, pos) -> apply1(p, sc, _ -> Err({ msg = msg, pos = pos}), inp, pos))

    pub def run(p: Parser[t, t], input: String) : Result[t, ErrMsg] = 
        let fk = pos -> Err({ msg = "Parse error!", pos = pos });
        let sk = (a, _, _, _) -> Ok(a);
        let Parser(p1) = p;
        p1(sk, fk, input, 0)

    pub def runs(p: Parser[t, t], input: String) : Result[t, String] = 
        match run(p, input) { 
            case Ok(a) => Ok(a)
            case Err(e) => Err(Int32.toString(e.pos) + ": " + e.msg)
        }

    pub def one(): Parser[Int32, t] = bind(symbol("one"), _ -> return(1))
    pub def two(): Parser[Int32, t] = bind(symbol("two"), _ -> return(2))

}

// def main(): Result[Int32, String] = 
//     use Cps2.{runs, one, two, seqRight, swapError};
//     runs(seqRight(one(), two() `swapError` "two-failed"), "one two")

def main(): Result[Int32, String] = 
    use Cps2.{runs, one, two, seqRight};
    runs(seqRight(one(), two()), "onetwo")

// def main(): Result[Unit, String] = 
//     use Cps2.{runs, alt, one, endOfInput, ignore};
//     runs(alt(endOfInput(), ignore(one())), "one")


// def main(): Result[Unit, String] = 
//     use Cps2.{runs, endOfInput, swapError};
//     runs(endOfInput() `swapError` "Not end of input", "This is some text")

