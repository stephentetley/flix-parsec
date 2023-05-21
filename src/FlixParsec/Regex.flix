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

    use GradedApplicative.{*>, point}
    use GradedMonad.{>>=}

    use FlixParsec.{GenParser};
    use FlixParsec.Prim.{restOfInput, throwError, incrCursorPosition}

    pub def lookingAt(rgx: Regex): GenParser[ka, ust, ef, String] =
        restOfInput()       >>= rest    -> 
        match Regex.getPrefix(substr = rgx, rest) { 
            case Some(s) => {incrCursorPosition(String.length(s)) *> point(s)}
            case None    => throwError("lookingAt")
        }

    pub def skippingAt(rgx: Regex): GenParser[ka, st, ef, Unit] = 
        lookingAt(rgx) |> GradedFunctor.ignore

}
