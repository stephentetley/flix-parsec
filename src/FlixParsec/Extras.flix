/*
 * Copyright 2022 Stephen Tetley
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

/// This module contains Extra functions that have dependencies on `basis-base`...

namespace FlixParsec/Extras {

    use Basis/System.FilePath;
    use Basis/Text.Charset;

    use FlixParsec/Prim.{GenParser1, Parser1, ParseError};

    pub def parseFile(parser: GenParser1[st, ans], 
                        initState: st, 
                        tabSize: Int32, 
                        path: FilePath, 
                        cs: Charset): Result[ans, ParseError] & Impure =
        use Result.flatMap;
        let* exists = Basis/System/Files.exists(path, Nil) |> Result.mapErr(FlixParsec/Prim.generalError);
        if (exists) {
            let* src = Basis/System/Files.readStringWithCharset(path, cs) |> Result.mapErr(FlixParsec/Prim.generalError);
            FlixParsec/Prim.runGenParser(parser, initState, tabSize, src)
        } else
            Err(FlixParsec/Prim.fileNotFoundError(ToString.toString(path)))


    pub def parsesFile(parser: Parser1[ans],
                        tabSize: Int32, 
                        path: FilePath, 
                        cs: Charset): Result[ans, String] & Impure =
        match parseFile(parser, (), tabSize, path, cs) {
            case Ok(a) => Ok(a)
            case Err(e) => Err(FlixParsec/Prim.showSourcePosition(e.position) + ": " + e.message)
        }

    // pub def parseFileWithBOM(parser: GenParser1[st, ans], 
    //                             initState: st, 
    //                             tabSize: Int32, 
    //                             path: FilePath): Result[ans, ParseError] & Impure =
    //     use Result.flatMap;
    //     let* exists = FlixSandbox/System/IO.fileExists(path) |> Result.mapErr(FlixParsec/Prim.generalError);
    //     if (exists) {
    //         let* src = FlixSandbox/System/File/BOM.readFileWithBOM(path) |> Result.mapErr(FlixParsec/Prim.generalError);
    //         FlixParsec/Prim.runGenParser(parser, initState, tabSize, src)
    //     } else
    //         Err(FlixParsec/Prim.fileNotFoundError(ToString.toString(path)))

    // pub def parsesFileWithBOM(parser: Parser1[ans], 
    //                             tabSize: Int32, 
    //                             path: FilePath): Result[ans, String] & Impure =
    //     match parseFileWithBOM(parser, (), tabSize, path) {
    //         case Ok(a) => Ok(a)
    //         case Err(e) => Err(FlixParsec/Prim.showSourcePosition(e.position) + ": " + e.message)
    //     }

}
