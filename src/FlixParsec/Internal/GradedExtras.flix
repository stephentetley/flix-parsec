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



mod FlixParsec.Internal.GradedExtras {

    
    use Graded.EnvStateError
    use Graded.EnvStateError.{EnvStateError}

    /// Missing in EnvStateError
    pub def swapError(p: EnvStateError[ka, env, st, ef, a], message: String) : EnvStateError[ka, env, st, ef, a] =
        Graded.EnvStateError.catchError(p, _ -> Graded.EnvStateError.throwError(message))

    /// Wrong name in EnvStateError...
    pub def runEnvStateError(ma: EnvStateError[(s, a), env, s, ef, a], e: env, st: s): Result[String, (s, a)] \ ef =
        Graded.EnvStateError.runStateError(ma, e, st)

}
