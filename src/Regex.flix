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

    use FlixParsec.{GenParser};
    use FlixParsec.GenParser.{GenParser};

    pub def lookingAt(patt: Text/Regex.Pattern): GenParser[ka1, st, String] =
        use Chain.{singleton};
        use Text/Regex.{boundedStartsWith};
        GenParser((sk, fk, ctx, st, errs) ->
            let ans = boundedStartsWith(st.pos, String.length(ctx.input), patt, ctx.input) as & Pure;
            match ans { 
                case Some(s) => sk(s, fk, ctx, {pos = st.pos + String.length(s) | st}, errs)
                case None(_) => fk(st.pos, singleton("lookingAt failed"))
            }
        )


    /// Version of `lookingAt` with a MatchEvaluator
    pub def evalLookingAt(patt: Text/Regex.Pattern, eval: Text/Regex.MatchEvaluator[a]): GenParser[ka1, st, a] & Impure =
        use Chain.{singleton};
        use Text/Regex.{evalBoundedStartsWith};
        use Text/Regex/MatchEvaluator.{tuple2, getLength};
        let eval2 = tuple2(getLength(), eval);
        GenParser((sk, fk, ctx, st, errs) ->
            let ans = evalBoundedStartsWith(st.pos, String.length(ctx.input), patt, eval2, ctx.input) as & Pure;
            match ans {
                case Ok(len, x) => sk(x, fk, ctx, {pos = st.pos + len | st}, errs)
                case Err(_) => fk(st.pos, singleton("evalLookingAt failed"))
            }
        )

    pub def skippingAt(patt: Text/Regex.Pattern): GenParser[ka1, st, Unit] = 
        use Chain.{singleton};
        use Text/Regex.{boundedStartsWith};
        GenParser((sk, fk, ctx, st, errs) ->
            let ans = boundedStartsWith(st.pos, String.length(ctx.input), patt, ctx.input) as & Pure;
            match ans { 
                case Some(s) => sk((), fk, ctx, {pos = st.pos + String.length(s) | st}, errs)
                case None => fk(st.pos, singleton("skippingAt failed"))
            }
        )

    /// Parsing fails if regex does not compile.
    pub def lookingAts(regex: String): GenParser[ka1, st, String] = 
        use Text/Regex/Pattern.{compile};
        use FlixParsec/Prim.{bind, liftResult};
        bind(liftResult(compile(regex), _ -> "lookingAts"), lookingAt)

    /// Parsing fails if regex does not compile.
    pub def evalLookingAts(regex: String, eval: Text/Regex.MatchEvaluator[a]): GenParser[ka1, st, a] =
        use Text/Regex/Pattern.{compile};
        use FlixParsec/Prim.{bind, liftResult};
        bind(liftResult(compile(regex), _ -> "evalLookingAts"), patt -> evalLookingAt(patt, eval) as & Pure) /// TODO remove cast


    /// Parsing fails if regex does not compile.
    pub def skippingAts(regex: String): GenParser[ka1, st, Unit] = 
        use Text/Regex/Pattern.{compile};
        use FlixParsec/Prim.{bind, liftResult};
        bind(liftResult(compile(regex), _ -> "skippingAts"), skippingAt)

}
