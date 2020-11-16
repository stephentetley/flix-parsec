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



namespace TextParser/Trace {

    use TextParser.TextParser;

    pub def trace(message: String, p: TextParser[a, t]): TextParser[a, t] = 
        use TextParser.{flatMap, return, getPosition, showPosition, liftAction};
        let* pos1 = getPosition();
        let* ans = p;
        let* pos2 = getPosition();
        let msg = "// " + message + ". Start {" + showPosition(pos1) + "}, End {" + showPosition(pos2) + "}";
        let* _ =  liftAction(_ -> Console.printLine(msg));
        return(ans)

    pub def tell(message: String): TextParser[Unit, t] = 
        use TextParser.{flatMap, return, getPosition, showPosition, liftAction};
        let* pos1 = getPosition();
        let msg = "// " + message + ". Position {" + showPosition(pos1) +  "}";
        let* _ =  liftAction(_ -> Console.printLine(msg));
        return()

}