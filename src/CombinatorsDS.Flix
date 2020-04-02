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
use TextParser.{bindFS, return};

///
/// Combinators to build parsers.
///

namespace TextParser/CombinatorsDS {

    pub def ntimes(p: Parser[a]): Parser[Int32] = 
        manyWith((_,ac) -> ac + 1, 0, p)

    pub def manyWith(f: (a,ac) -> ac, zero: ac, p: Parser[a]): Parser[ac] = 
        manyWithHelper(f, p, zero)  

    pub def optional(p: Parser[a]): Parser[Option[a]] = 
        bindFS(p, _ -> return(None), x -> return(Some(x)))
    

    def manyWithHelper(f: (a, ac) -> ac, p: Parser[a], ac: ac): Parser[ac] = 
        use TextParser.flatMap;
        let* ans = optional(p);
        match ans { 
            case None => return(ac)
            case Some(a) => manyWithHelper(f, p, f(a,ac))
        }

    pub def skipManyTill(p: Parser[a], endp: Parser[z]): Parser[Unit] = 
        manyTillWithHelper((_,ac) -> ac, p, endp, ())

    def manyTillWithHelper(f: (a, ac) -> ac, p: Parser[a], endp: Parser[e], ac: ac): Parser[ac] = 
        use TextParser.flatMap;
        let* done = optional(endp);
        match done { 
            case Some(_) => return(ac)
            case None => {
                let* a = p;
                manyTillWithHelper(f, p, endp, f(a, ac))
            }
        }
    
}