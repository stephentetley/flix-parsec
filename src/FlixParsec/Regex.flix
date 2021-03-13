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



namespace FlixParsec/Regex {

    use FlixParsec.TextParser;
    use FlixParsec.TextParser.{TextParser};

    pub def lookingAt(patt: Text/Regex.Pattern): TextParser[u, t1, String] =
        use Chain.{singleton};
        use Text/Regex.{boundedStartsWith};
        TextParser((sk, fk, ctx, st, errs) ->
            let ans = boundedStartsWith(ctx.input, patt, st.pos, String.length(ctx.input)) as & Pure;
            match ans { 
                case Some(s) => sk(s, fk, ctx, {pos = st.pos + String.length(s) | st}, errs)
                case None(_) => fk(st.pos, singleton("lookingAt failed"))
            }
        )


    /// Version of `lookingAt` with a MatchEvaluator
    pub def evalLookingAt(patt: Text/Regex.Pattern, eval: Text/Regex.MatchEvaluator[a]): TextParser[u, t1, a] =
        use Chain.{singleton};
        use Text/Regex.{evalBoundedStartsWith};
        use Text/Regex/MatchEvaluator.{tuple2, getLength};
        let eval2 = tuple2(getLength(), eval);
        TextParser((sk, fk, ctx, st, errs) ->
            let ans = evalBoundedStartsWith(ctx.input, patt, st.pos, String.length(ctx.input), eval2) as & Pure;
            match ans {
                case Ok(len, x) => sk(x, fk, ctx, {pos = st.pos + len | st}, errs)
                case Err(_) => fk(st.pos, singleton("evalLookingAt failed"))
            }
        )

    pub def skippingAt(patt: Text/Regex.Pattern): TextParser[u, t1, Unit] = 
        use Chain.{singleton};
        use Text/Regex.{boundedStartsWith};
        TextParser((sk, fk, ctx, st, errs) ->
            let ans = boundedStartsWith(ctx.input, patt, st.pos, String.length(ctx.input)) as & Pure;
            match ans { 
                case Some(s) => sk((), fk, ctx, {pos = st.pos + String.length(s) | st}, errs)
                case None => fk(st.pos, singleton("skippingAt failed"))
            }
        )

    /// Parsing fails if regex does not compile.
    pub def lookingAts(regex: String): TextParser[u, t1, String] = 
        use Text/Regex/Pattern.{compile};
        use FlixParsec/Prim.{bind, liftOption};
        bind(liftOption(compile(regex), "lookingAts"), lookingAt)

    /// Parsing fails if regex does not compile.
    pub def evalLookingAts(regex: String, eval: Text/Regex.MatchEvaluator[a]): TextParser[u, t1, a] =
        use Text/Regex/Pattern.{compile};
        use FlixParsec/Prim.{bind, liftOption};
        bind(liftOption(compile(regex), "evalLookingAts"), patt -> evalLookingAt(patt, eval))


    /// Parsing fails if regex does not compile.
    pub def skippingAts(regex: String): TextParser[u, t1, Unit] = 
        use Text/Regex/Pattern.{compile};
        use FlixParsec/Prim.{bind, liftOption};
        bind(liftOption(compile(regex), "skippingAts"), skippingAt)


}
