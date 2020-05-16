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

use TextParser.Parser;
use TextParser.{liftPrim, liftPrimResult, fail, return};


namespace TextParser/Text {

    pub def string(s: String): Parser[String] = 
        liftPrimResult(TextParser/Internal/TextCursor.string(s))

    pub def whiteSpace(): Parser[Unit] =  
        liftPrim(TextParser/Internal/TextCursor.skipWhiteSpace)

    pub def whiteSpace1(): Parser[Unit] =  
        liftPrimResult(TextParser/Internal/TextCursor.whiteSpace1)
        

}
