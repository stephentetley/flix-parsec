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



pub mod FlixParsec.Trace {

    use Monad.{>>=}
    use Applicative.{point}

    use FlixParsec.GenParser.{GenParser}
    use FlixParsec.GenParser.{getSourcePosition}
    use FlixParsec.Internal.ParseError.{showSourcePosition}

    pub def trace(message: String, p: GenParser[ka, ust, a]): GenParser[ka, ust, a] \ IO = 
        let mkMessage = (spos, epos) -> "// ${message}. Start <<${showSourcePosition(spos)}>>, End <<${showSourcePosition(epos)}>>";
        getSourcePosition()                             >>= pos1 -> 
        p                                               >>= ans  ->
        getSourcePosition()                             >>= pos2 ->
        {println(mkMessage(pos1, pos2)); point(())}     >>= _ -> 
        point(ans)

    // pub def tell(message: String): GenParser[ka, ust, Unit] = 
    //     let mkMessage = pos -> "// ${message}. Position <<${showSourcePosition(pos)}>>";
    //     getSourcePosition()                                 >>= spos -> 
    //     {let s = mkMessage(spos); println(s); point(())}    >>= _    ->
    //     point()

}
