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

    pub def getInput(x: TextCursor): String & Impure = 
        import flix.runtime.spt.textparser.TextCursor.getInput();
        let TextCursor(x1) = x;
        x1.getInput()


    pub def string(s: String, x: TextCursor): Result[String, PrimError] & Impure = 
        import flix.runtime.spt.textparser.TextCursor.stringX(String);
        let TextCursor(x1) = x;
        let ans = x1.stringX(s);
        if (isNull(ans)) 
            Err(InputMismatchException)
        else 
            Ok(ans)

    pub def skipWhiteSpace(x: TextCursor): Unit & Impure = 
        import flix.runtime.spt.textparser.TextCursor.skipWhiteSpace();
        let TextCursor(x1) = x;
        x1.skipWhiteSpace()

    pub def whiteSpace1(x: TextCursor): Result[Unit, PrimError] & Impure = 
        import flix.runtime.spt.textparser.TextCursor.whiteSpace1X();
        let TextCursor(x1) = x;
        let ok = x1.whiteSpace1X();
        if (!ok)
            Err(InputMismatchException)
        else 
            Ok()

    pub def manyChar(c: Char, x: TextCursor): String & Impure = 
        import flix.runtime.spt.textparser.TextCursor.manyChar(Char);
        let TextCursor(x1) = x;
        x1.manyChar(c)

    pub def skipChar(c: Char, x: TextCursor): Int32 & Impure = 
        import flix.runtime.spt.textparser.TextCursor.skipChar(Char);
        let TextCursor(x1) = x;
        x1.skipChar(c)
    
    pub def manyAlphabetic(x: TextCursor): String & Impure = 
        import flix.runtime.spt.textparser.TextCursor.manyAlphabetic();
        let TextCursor(x1) = x;
        x1.manyAlphabetic()
        
    pub def manyLetter(x: TextCursor): String & Impure = 
        import flix.runtime.spt.textparser.TextCursor.manyLetter();
        let TextCursor(x1) = x;
        x1.manyLetter()

    pub def manyDigit(x: TextCursor): String & Impure = 
        import flix.runtime.spt.textparser.TextCursor.manyDigit();
        let TextCursor(x1) = x;
        x1.manyDigit()

    pub def manyLowerCase(x: TextCursor): String & Impure = 
        import flix.runtime.spt.textparser.TextCursor.manyLowerCase();
        let TextCursor(x1) = x;
        x1.manyLowerCase()
    
    pub def manyUpperCase(x: TextCursor): String & Impure = 
        import flix.runtime.spt.textparser.TextCursor.manyUpperCase();
        let TextCursor(x1) = x;
        x1.manyUpperCase()

    pub def manyLetterOrDigit(x: TextCursor): String & Impure = 
        import flix.runtime.spt.textparser.TextCursor.manyLetterOrDigit();
        let TextCursor(x1) = x;
        x1.manyLetterOrDigit()

}