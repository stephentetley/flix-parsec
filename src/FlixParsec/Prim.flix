/*
 * Copyright 2021 Stephen Tetley
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


namespace FlixParsec {

    type alias ParseContext = {input: String, tabSize: Int32}

    type alias Failures = Chain[String]

    type alias FailK[t] = Int32 -> Failures ~> t

    type alias SuccessK[r, t] = r -> FailK[t] -> ParseContext -> Int32 -> Failures ~> t

    pub opaque type TextParser[r, t] = SuccessK[r, t] -> FailK[t] -> ParseContext -> Int32 -> Failures ~> t

    /// instance Functor[TextParser[r]] {
    ///     def map(f: a -> b & ef, x: TextParser[a, t]): TextParser[b, t] & ef = map(f, x)
    /// }

}

namespace FlixParsec/Prim {

    use FlixParsec.{TextParser, ParseContext, Failures, SuccessK, FailK};
    use FlixParsec.TextParser.{TextParser};

    type alias Position = { lineNumber: Int32, column: Int32 }

    type alias Error1 = { msgs: List[String], pos: Int32 }

    type alias ParseError = 
        { message: String
        , position: Position
        , textBefore: String
        , textAfter: String
        }

    pub def showPosition(pos: Position): String = 
        "Line=" + Int32.toString(pos.lineNumber) + ", Col=" + Int32.toString(pos.column)

    pub def samePosition(pos1: Position, pos2: Position): Bool =
        pos1.lineNumber == pos2.lineNumber and pos1.column == pos2.column

    pub def showParseError(err: ParseError): String = 
        String.unlines(err.message :: showPosition(err.position) 
                            :: "Seen:" :: "-----" :: err.textBefore 
                            :: "Pending:" :: "--------" :: err.textAfter :: Nil)

    def showErrorsList(stk: List[String]): String = match stk {
        case Nil => "*error*"
        case x :: rs => x + errorMessageHelper(rs)
    }

    def errorMessageHelper(rest: List[String]): String = match rest {
        case Nil => ""
        case x :: Nil => "[" + x + "]"
        case x :: rs => "[" + x + errorMessageHelper(rs) + "]"
    }

    def fileNotFoundError(path: System.FilePath): ParseError = 
        { message = "File not found: ${path}"
        , position = {lineNumber = 0, column = 0}
        , textBefore = ""
        , textAfter = ""
        }

    def generalError[a: ToString](err: a): ParseError = 
        { message = "${err}"
        , position = {lineNumber = 0, column = 0}
        , textBefore = ""
        , textAfter = ""
        }

    ///
    /// This is exposed so client code can implement wrappers over `run` etc. without
    /// knowing about the internal error type `Error1`.
    ///
    type alias TextParser1[t] = TextParser[t, Result[t, Error1]]

    ///
    /// Run parser `p` on the string `input`.
    ///
    pub def run(p: TextParser1[t], input: String) : Result[t, ParseError] & Impure =
        use Chain.{toList, empty};
        use FlixParsec/Internal/ErrorUtils.{textPos, getTextBeforeAndAfter};
        let context = {input = input, tabSize = 4};
        let fk = (pos, errs) -> Err({ msgs = toList(errs), pos = pos }) as & Impure;
        let sk = (a, _, _, _, _) -> Ok(a) as & Impure;
        let TextParser(p1) = p;
        match p1(sk, fk, context, 0, empty()) {
            case Ok(a) => Ok(a)
            case Err(e1) => {
                let (l,c) = textPos(context.tabSize, e1.pos, input);
                let posn = { lineNumber = l, column = c };
                let (before, after) = getTextBeforeAndAfter(e1.pos, input);
                Err({ message = showErrorsList(e1.msgs)
                    , position = posn
                    , textBefore = before
                    , textAfter = after })
            }
        }

    ///
    pub def runs(p: TextParser1[t], input: String) : Result[t, String] & Impure =
        match run(p, input) {
            case Ok(a) => Ok(a)
            case Err(e) => Err(showPosition(e.position) + ": " + e.message)
        }

    pub def parseFile(parser: TextParser1[t], path: System.FilePath, cs: Text.Charset): Result[t, ParseError] & Impure =
        use Result.flatMap;
        let* exists = System/File.exists(path) |> Result.mapErr(generalError);
        if (exists) {
            let* src = System/File.readFile(path, cs) |> Result.mapErr(generalError);
            run(parser, src)
        } else
            Err(fileNotFoundError(path))


    pub def parsesFile(parser: TextParser1[t], path: System.FilePath, cs: Text.Charset): Result[t, String] & Impure =
        match parseFile(parser, path, cs) {
            case Ok(a) => Ok(a)
            case Err(e) => Err(showPosition(e.position) + ": " + e.message)
        }

    pub def parseFileWithBOM(parser: TextParser1[t], path: System.FilePath, cs: Text.Charset): Result[t, ParseError] & Impure =
        use Result.flatMap;
        let* exists = System/File.exists(path) |> Result.mapErr(generalError);
        if (exists) {
            let* src = System/File.readFileWithBOM(path, cs) |> Result.mapErr(generalError);
            run(parser, src)
        } else
            Err(fileNotFoundError(path))

    pub def parsesFileWithBOM(parser: TextParser1[t], path: System.FilePath, cs: Text.Charset): Result[t, String] & Impure =
        match parseFileWithBOM(parser, path, cs) {
            case Ok(a) => Ok(a)
            case Err(e) => Err(showPosition(e.position) + ": " + e.message)
        }

    pub def return(x: a): TextParser[a, t] = TextParser(succ -> succ(x))

    pub def failure(message: String) : TextParser[r, t] =
        use Chain.{singleton};
        TextParser((_, fk, _, pos, _) -> fk(pos, singleton(message)))

    ///
    /// `swapError` is quite subtle. Swapping the error continuate needs to be
    /// a "local" operation which is undone (restored) afterwards (c.f. local in the Reader
    /// monad). It must not be a "stateful" operation with unlimited extent.
    ///
    pub def swapError(p: TextParser[r,t], message: String) : TextParser[r, t] =
        use Chain.{singleton};
        TextParser((sk, fk, ctx, pos, errs) ->
            let fk1 = (pos1, _) -> fk(pos1, singleton(message));
            let sk1 = (x, _, _, pos1, _) -> sk(x, fk, ctx, pos1, errs);
            apply1(p, sk1, fk1, ctx, pos, errs)
        )

    def apply1 (p: TextParser[r, t], sk: SuccessK[r, t], fk: FailK[t], ctx: ParseContext, pos: Int32, errs: Failures): t & Impure =
        let TextParser(pf) = p;
        pf(sk, fk, ctx, pos, errs)

    pub def map(f: a -> b & ef, p: TextParser[a, t]): TextParser[b, t] =
        TextParser((sk, fk, ctx, pos, errs) ->
            let sk1 = (x, fk1, _, pos1, errs1) -> sk(f(x) as & Impure, fk1, ctx, pos1, errs1);
            apply1(p, sk1, fk, ctx, pos, errs)
        )

    pub def ap(mf: TextParser[a -> b & ef, t], ma: TextParser[a, t]): TextParser[b, t] =
        let* f = mf;
        let* a = ma;
        liftAction(_ -> f(a) as & Impure)

    pub def bind(ma: TextParser[q, t], k: q -> TextParser[r, t]):  TextParser[r, t] =
        TextParser(sk ->
            apply1(ma, x -> apply1(k(x), sk))
        )

    pub def flatMap(k: q -> TextParser[r, t], ma: TextParser[q, t]):  TextParser[r, t] = bind(ma, k)

    pub def alt(p: TextParser[a, t], q: TextParser[a, t]): TextParser[a, t] =
        use Chain.{append};
        TextParser((sk, fk, ctx, pos, errs) ->
            let pfk = (_, perrs) -> {
                let qfk = (_, qerrs) -> fk(pos, append(perrs, qerrs));
                apply1(q, sk, qfk, ctx, pos, perrs)
            };
            apply1(p, sk, pfk, ctx, pos, errs)
        )


    pub def seqLeft(p: TextParser[r, t], q: TextParser[s, t]): TextParser[r, t] =
        TextParser(sk ->
            apply1(p, x -> apply1(q, _ -> sk(x)))
        )

    pub def seqRight(p: TextParser[r, t], q: TextParser[s, t]): TextParser[s, t] =
        TextParser(sk ->
            apply1(p, _ -> apply1(q, y -> sk(y)))
        )

    pub def ifThenElse(cond: TextParser[r, t], thenp: r -> TextParser[s,t], elsep: TextParser[s, t]): TextParser[s, t] = 
        TextParser((sk, fk, inp, pos) ->
            apply1(cond, x -> apply1(thenp(x), sk), apply1(elsep, sk, fk, inp), inp, pos))

    pub def choice(ps: List[TextParser[a, t]]): TextParser[a, t] =
        use Chain.{singleton};
        TextParser((sk, fk, ctx, pos, errs) ->
            match ps {
                case Nil => fk(pos, singleton("choice"))
                case p :: rs => {
                    let fk1 = (_, _) -> apply1(choice(rs), sk, fk, ctx, pos, errs);
                    apply1(p, sk, fk1, ctx, pos, errs)
                }
            }
        )


    pub def liftOption(x: Option[r], errMsg: String): TextParser[r, t] =  match x {
        case Some(a) => return(a)
        case None    => failure(errMsg)
    }

    pub def liftResult(x: Result[r, e]): TextParser[r, t] =  match x {
        case Ok(a) => return(a)
        case Err(_) => failure("liftResult")
    }

    pub def liftAction(f: Unit -> r & ef): TextParser[r, t] =
        TextParser((sk, fk, ctx, pos, errs) ->
            let a = f() as & Impure;
            sk(a, fk, ctx, pos, errs)
        )

    pub def liftActionResult(f: Unit -> Result[r, e] & ef): TextParser[r, t] =
        use Chain.{singleton};
        TextParser((sk, fk, ctx, pos, errs) ->
            let ans = f() as & Impure;
            match ans {
                case Ok(a) => sk(a, fk, ctx, pos, errs)
                case Err(_) => fk(pos, singleton("listActionResult"))
            }
        )

    pub def getPosition(): TextParser[Position, t] =  
        TextParser((sk, fk, ctx, pos) ->
            let (x,y) = FlixParsec/Internal/ErrorUtils.textPos(ctx.tabSize, pos, ctx.input);
            sk({ lineNumber = x, column = y }, fk, ctx, pos)
        )


    pub def endOfInput() : TextParser[Unit, t] =
        use Chain.{singleton};
        TextParser((sk, fk, ctx, pos, errs) ->
            let len = String.length(ctx.input);
            if (pos >= len)
                sk((), fk, ctx, pos, errs)
            else
                fk(pos, singleton("eof - not at end"))
        ) 

    pub def string(s: String): TextParser[String, t] =
        use Chain.{singleton};
        TextParser((sk, fk, ctx, pos, errs) ->
            let strlen = String.length(s);
            let s1 = String.slice(ctx.input, pos, pos + strlen);
            if (s1 == s)
                sk(s, fk, ctx, pos + strlen, errs)
            else {
                let msg = "string - expecting '" + s + "', encountered '" + s1 + "'";
                fk(pos, singleton(msg))
            }
        )

    pub def char(ch: Char): TextParser[Char, t] = 
        use Chain.{singleton};
        TextParser((sk, fk, ctx, pos, errs) ->
            let len = String.length(ctx.input);
            if (pos < len) {
                let c = String.charAt(pos, ctx.input);
                if (c == ch)
                    sk(c, fk, ctx, pos + 1, errs)
                else  {
                    let msg = "char - expecting '" + Char.toString(ch) + "', encountered '" + Char.toString(c) + "'";
                    fk(pos, singleton(msg))
                }
            } else {
                let msg = "char - unexpected eof";
                fk(pos, singleton(msg))
            }
        )

    pub def anyChar(): TextParser[Char, t] =
        use Chain.{singleton};
        TextParser((sk, fk, ctx, pos, errs) ->
            let len = String.length(ctx.input);
            if (pos < len) {
                let c = String.charAt(pos, ctx.input);
                sk(c, fk, ctx, pos + 1, errs)
            } else {
                let msg = "anyChar - eof";
                fk(pos, singleton(msg))
            }
        )


    // Truncates if eof...
    pub def horizon(n: Int32): TextParser[String, t] = 
        use Chain.{singleton};
        TextParser((sk, fk, ctx, pos, errs) ->
            if (n < 0) {
                let msg = "horizon - invalid size";
                fk(pos, singleton(msg))
            } else {
                let str = String.slice(ctx.input, pos, pos + n);
                sk(str, fk, ctx, pos + String.length(str), errs)
            } 
        )


    ///
    /// lookahead - run the parser but don't consume input on success
    /// On fail, fail.
    ///
    pub def lookahead(p: TextParser[r, t]): TextParser[r, t] =
        TextParser((sk, fk, ctx, pos) ->
            let sk1 = (ans, fk1, _, _) -> sk(ans, fk1, ctx, pos);
            apply1(p, sk1, fk, ctx, pos)
        )

    ///
    /// Run parser `p` within a bounded length
    ///
    pub def bounded(n: Int32, p: TextParser[r, t]): TextParser[r, t] =
        use Chain.{singleton};
        TextParser((sk, fk, ctx, pos, errs) ->
            /// Applies parser `p` on a slice of the input, then "restores" the full input 
            /// for the success continuation.
            let str = String.slice(ctx.input, pos, pos + n);
            let fk1 = (_, _) -> fk(pos, singleton("bounded"));
            let sk1 = (ans, _, _, _) -> sk(ans, fk, ctx, pos + String.length(str));
            apply1(p, sk1, fk1, {input = str, tabSize = ctx.tabSize}, 0, errs)
        )


}
