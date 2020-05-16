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
use TextParser/Internal.{TextCursor};
use TextParser/Internal.TextCursor.{TextCursor};
use TextParser/Error.{PrimError};
use TextParser/Error.PrimError.{IllegalArgumentException, IllegalStateException, InputMismatchException, 
                                NoSuchElementException, PatternSyntaxException};

namespace TextParser/Internal {

    pub opaque type TextCursor = ##flix.runtime.spt.textparser.TextCursor

}

namespace TextParser/Internal/TextCursor {

    pub def new(input: String): TextCursor & Impure =
        import new flix.runtime.spt.textparser.TextCursor(String) as newTextCursor;
        TextCursor(newTextCursor(input))
        

    pub def getPos(x: TextCursor): Int32 & Impure = 
        import flix.runtime.spt.textparser.TextCursor.getPos();
        let TextCursor(x1) = x;
        x1.getPos()

    pub def setPos(x: TextCursor, n: Int32): Unit & Impure = 
        import flix.runtime.spt.textparser.TextCursor.setPos(Int32);
        let TextCursor(x1) = x;
        x1.setPos(n)
    
    pub def literal(x: TextCursor, s: String): Result[String, PrimError] & Impure = 
        import flix.runtime.spt.textparser.TextCursor.literalX(String);
        let TextCursor(x1) = x;
        let ans = x1.literalX(s);
        if (isNull(ans)) 
            Err(InputMismatchException)
        else 
            Ok(ans)


}