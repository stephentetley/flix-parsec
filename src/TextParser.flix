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



namespace TextParser {

    type alias ErrMsg = { msg: String, pos: Int32 }

    type alias NextRes[t] = Int32 -> Result[t, ErrMsg]

    type alias Parser1[t] = String -> Int32 -> Result[t, ErrMsg] 

    type alias Success[r, t] = r -> NextRes[t] -> Parser1[t]

    pub opaque type TextParser[r, t] = Success[r, t] -> NextRes[t] -> Parser1[t]

    ///
    /// Alias for `parseString`.
    ///
    pub def run(p: TextParser[t, t], input: String) : Result[t, ErrMsg] = 
        let fk = pos -> Err({ msg = "Parse error!", pos = pos });
        let sk = (a, _, _, _) -> Ok(a);
        let TextParser(p1) = p;
        p1(sk, fk, input, 0)

    /// TODO - convert pos into line and column...
    /// 
    pub def runs(p: TextParser[t, t], input: String) : Result[t, String] = 
        match run(p, input) { 
            case Ok(a) => Ok(a)
            case Err(e) => Err(Int32.toString(e.pos) + ": " + e.msg)
        }

    pub def parseFile(parser: TextParser[t, t], path: System.FilePath, cs: Text.Charset): Result[t, ErrMsg] & Impure =
        match System/File.readFile(path, cs) {
            case Err(_) => Err({ msg = "IO Error - could not read file", pos = 0 })
            case Ok(text) => run(parser, text)
        }


    pub def return(x: a): TextParser[a, t] = 
        TextParser((succ, next, inp, pos) -> succ(x, next, inp, pos))

    pub def fail(): TextParser[r, t] = 
        TextParser((_, next, _, pos) -> next(pos))

    pub def failure(msg: String) : TextParser[r, t] =
        TextParser((_, _, _, pos) -> Err({ msg = msg, pos = pos}))

    def apply1 (p: TextParser[r, t], sk: Success[r, t], fk: NextRes[t], inp: String, pos: Int32): Result[t, ErrMsg] = 
        let TextParser(pf) = p;
        pf(sk, fk, inp, pos)

    pub def map(f: a -> b, p: TextParser[a, t]): TextParser[b, t] =
        bind(p, x -> return(f(x)))

    pub def ap(mf: TextParser[a -> b, t], ma: TextParser[a, t]): TextParser[b, t] = 
        let* f = mf;
        let* a = ma;
        return(f(a))

    pub def bind(ma: TextParser[q, t], k: q -> TextParser[r, t]):  TextParser[r, t] =
        TextParser(succ -> apply1(ma, x -> apply1(k(x), succ)))

    pub def flatMap(k: q -> TextParser[r, t], ma: TextParser[q, t]):  TextParser[r, t] = bind(ma, k)

    // Use Clean optimization or not?
    pub def alt(p: TextParser[a, t], q: TextParser[a, t]): TextParser[a, t] = 
        TextParser((sc, nc, inp, pos) -> 
            apply1(p, (r,_) -> sc(r, nc), apply1(q, sc, nc, inp), inp, pos))


    pub def ifThenElse(cond: TextParser[r, t], thenp: r -> TextParser[s,t], elsep: TextParser[s, t]): TextParser[s, t] = 
        TextParser((sc, nc, inp, pos) -> 
            apply1(cond, x -> apply1(thenp(x),sc), apply1(elsep, sc, nc, inp), inp, pos))


    pub def endOfInput() : TextParser[Unit, t] =
        TextParser((sc, nc, inp, pos) ->
            let len = String.length(inp);
            if (pos >= len)
                sc((), nc, inp, pos)
            else
                nc(pos)
        )

    pub def string(s: String): TextParser[String, t] = 
        TextParser((sc, nc, inp, pos) ->
            let strlen = String.length(s);
            let s1 = String.slice(inp, pos, pos + strlen);
            if (s1 == s)
                sc(s, nc, inp, pos + strlen)
            else
                nc(pos)
        )

    pub def char(ch: Char): TextParser[Char, t] = 
        TextParser((sc, nc, inp, pos) ->
            let len = String.length(inp);            
            if (pos < len) {
                let c = String.charAt(pos, inp);
                if (c == ch)
                    sc(c, nc, inp, pos + 1)
                else 
                    nc(pos)
            }
            else
                nc(pos)
        )

    pub def anyChar(): TextParser[Char, t] = 
        TextParser((sc, nc, inp, pos) ->
            let len = String.length(inp);            
            if (pos < len) {
                let c = String.charAt(pos, inp);
                sc(c, nc, inp, pos + 1)
            }
            else
                nc(pos)
        )

    pub def lookingAt(patt: Text/Regex.Pattern): TextParser[String, t] = 
        use Text/Regex.{startsWithWithin}; // Name will change...
        TextParser((sc, nc, inp, pos) ->
            let ans = startsWithWithin(patt, inp, pos, String.length(inp)) as & Pure;
            match ans { 
                case Ok(s) => sc(s, nc, inp, pos + String.length(s))
                case Err(_) => nc(pos)
            }
        )


    pub def swapError(msg: String, p: TextParser[r,t]) : TextParser[r, t] =
        TextParser((sc, _, inp, pos) -> apply1(p, sc, _ -> Err({ msg = msg, pos = pos}), inp, pos))




//     /// Internal helper to make ParseErrors
//     def applyParseError(msg: String, r: Config, s: TextCursor): ParseError & Impure =
//         use TextParser/Internal/TextCursor.{getPos, getInput};
//         use TextParser/Error.{makeParseError};
//         let tabsize = r.tabsize;
//         let pos = s.getPos();
//         let inp = s.getInput();
//         makeParseError(inp, tabsize, pos, msg)


//     pub def nestError(msg: String, p: Parser[ans]): Parser[ans] =
//         use TextParser/Error.{nestErrorMessage};
//         Parser((r,s) ->
//             let Parser(p1) = p;
//             match p1(r,s) {
//                 case Ok(a) => Ok(a)
//                 case Err(e) => Err(nestErrorMessage(msg, e))
//             }
//         )



//     ///
//     /// lookahead - run the parser but don't consume input on success
//     /// On fail, fail.
//     ///
//     pub def lookahead(p: Parser[a]): Parser[a] =
//         use TextParser/Internal/TextCursor.{setPos, getPos};
//         use TextParser/Error.{nestErrorMessage};
//         Parser((r,s) -> {
//             let Parser(p1) = p;
//             let start = s.getPos();
//             match p1(r,s) {
//                 case Err(e) => Err(nestErrorMessage("lookahead", e))
//                 case Ok(a) => {
//                     s.setPos(start);
//                     Ok(a)
//                 }
//             }
//         })





//     ///
//     /// Run parser `p` within a bounded length
//     ///
//     pub def bounded(n: Int32, p: Parser[a]): Parser[a] =
//         use TextParser/Internal/TextCursor.{horizon};
//         Parser((r,s) ->
//             match horizon(n, s) {
//                 case Ok(str) => match parseString(p, str) {
//                     case Ok(a) => Ok(a)
//                     case Err(_) => Err(applyParseError("bounded", r, s))
//                 }
//                 case Err(_)  => Err(applyParseError("bounded", r, s))
//             }
//         )



//     ///
//     /// Run parser `p` if it fails reset input and raise the error
//     ///
//     pub def tryparse(p: Parser[a]): Parser[a] =
//         use TextParser/Internal/TextCursor.{getPos, setPos};
//         Parser((r,s) ->
//             let Parser(p1) = p;
//             let start = s.getPos();
//             match p1(r,s) {
//                 case Err(e) => {
//                     s.setPos(start);
//                     Err(e)
//                 }
//                 case Ok(a) => Ok(a)
//             }
//         )


}
