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



mod FlixParsec.Regex {

    use FlixParsec.{GenParser};
    use FlixParsec.GenParser.{GenParser};
/*
    pub def lookingAt(rgx: Regex): GenParser[ka1, ust, ef, String] =
        GenParser((sk, fk, ctx, st, errs) ->
            let ans = Text.Regex.boundedMatchPrefixContent({start = st.pos}, {end = String.length(ctx.input)}, patt, ctx.input);
            match ans { 
                case Some(s) => sk(s, fk, ctx, {pos = st.pos + String.length(s) | st}, errs)
                case None(_) => fk(st.pos, Chain.singleton("lookingAt failed"))
            }
        )


    /// Version of `lookingAt` with a MatchEvaluator
    // pub def evalLookingAt(patt: Text.Regex.Pattern, eval: MatchEvaluator[a]): GenParser[ka1, st, a] =
    //     use Text.RegexEval.Regex.{boundedPrefix};
    //     use Text.RegexEval.MatchEvaluator.{getLength};
    //     let eval2 = Applicative.product(getLength(), eval);
    //     GenParser((sk, fk, ctx, st, errs) ->
    //         let ans = boundedPrefix({start = st.pos}, {end = String.length(ctx.input)}, patt, eval2, ctx.input);
    //         match ans {
    //             case Ok(len, x) => sk(x, fk, ctx, {pos = st.pos + len | st}, errs)
    //             case Err(_) => fk(st.pos, Chain.singleton("evalLookingAt failed"))
    //         }
    //     )

    pub def skippingAt(patt: Text.Regex.Pattern): GenParser[ka1, st, Unit] = 
        GenParser((sk, fk, ctx, st, errs) ->
            let ans = Text.Regex.boundedMatchPrefixContent({start = st.pos}, {end = String.length(ctx.input)}, patt, ctx.input);
            match ans { 
                case Some(s) => sk((), fk, ctx, {pos = st.pos + String.length(s) | st}, errs)
                case None => fk(st.pos, Chain.singleton("skippingAt failed"))
            }
        )

    /// Parsing fails if regex does not compile.
    pub def lookingAts(regex: String): GenParser[ka1, st, String] = 
        use Text.Regex.Pattern.{compile};
        use FlixParsec.Prim.{bind, liftResult1};
        bind(liftResult1(compile(regex), _ -> "lookingAts"), lookingAt)


    /// Parsing fails if regex does not compile.
    // pub def evalLookingAts(regex: String, eval: MatchEvaluator[a]): GenParser[ka1, st, a] =
    //     use Text.Regex.Pattern.{compile};
    //     use FlixParsec.Prim.{bind, liftResult1};
    //     bind(liftResult1(compile(regex), _ -> "evalLookingAts"), patt -> evalLookingAt(patt, eval))


    /// Parsing fails if regex does not compile.
    pub def skippingAts(regex: String): GenParser[ka1, st, Unit] = 
        use Text.Regex.Pattern.{compile};
        use FlixParsec.Prim.{bind, liftResult1};
        bind(liftResult1(compile(regex), _ -> "skippingAts"), skippingAt)
*/
}
