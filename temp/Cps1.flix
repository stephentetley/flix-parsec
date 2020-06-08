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

/// A baseline count lines that doesn't use the parser combinators.

namespace Cps1 {

    type alias ErrMsg = String

    type alias NextRes[t] = Result[t, ErrMsg]

    type alias Parser1[t] = (String, Int32) -> Result[t, ErrMsg] 

    type alias Success[r, t] = r -> NextRes[t] -> Parser1[t]

    pub opaque type Parser[r, t] = Success[r, t] -> NextRes[t] -> Parser1[t]

    pub def return(x: a): Parser[a, t] = 
        Parser((succ, next, inp, pos) -> succ(x, next, inp, pos))

    pub def fail(): Parser[r, t] = 
        Parser((_, next, _, _) -> next)

    def apply1 (p: Parser[r, t], sk: Success[r, t], fk: NextRes[t], inp: String, pos: Int32): Result[t, ErrMsg] = 
        let Parser(pf) = p;
        pf(sk, fk, inp, pos)


    pub def bind(ma: Parser[q, t], k: q -> Parser[r, t]):  Parser[r, t] =
        Parser(succ -> apply1(ma, x -> apply1(k(x), succ)))

    pub def flatMap(k: q -> Parser[r, t], ma: Parser[q, t]):  Parser[r, t] = bind(ma, k)

    pub def symbol(sym: String): Parser[String, t] = 
        Parser((sc, nc, inp, pos) ->
            let symlen = String.length(sym);
            let s1 = String.slice(inp, pos, pos + symlen);
            if (s1 == sym)
                sc(sym, nc, inp, pos + symlen)
            else
                nc
        )

    pub def run(p: Parser[t, t], input: String) : Result[t, ErrMsg] = 
        let fk = Err("Parse error!");
        let sk = (a, _, _, _) -> Ok(a);
        let Parser(p1) = p;
        p1(sk, fk, input, 0)

    pub def one(): Parser[Int32, t] = bind(symbol("one"), _ -> return(1))


}

def main(): Result[Int32, String] = 
    use Cps1.{run, one};
    run(one(), "one two")