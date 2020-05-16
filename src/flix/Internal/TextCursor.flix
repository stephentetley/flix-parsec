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

    pub def char(c: Char, x: TextCursor): Result[Char, PrimError] & Impure = 
        import flix.runtime.spt.textparser.TextCursor.charX(Char);
        let TextCursor(x1) = x;
        let ans = x1.charX(c);
        if (!ans) 
            Err(InputMismatchException)
        else 
            Ok(c)

    pub def anyChar(x: TextCursor): Result[Char, PrimError] & Impure = 
        import flix.runtime.spt.textparser.TextCursor.anyCharX();
        let TextCursor(x1) = x;
        let str = x1.anyCharX();
        if (isNull(str)) 
            Err(InputMismatchException)
        else {
            let c = String.charAt(0, str);
            Ok(c)
        }

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
    
    pub def countChar(n: Int32, c: Char, x: TextCursor): Result[String, PrimError] & Impure = 
        import flix.runtime.spt.textparser.TextCursor.countCharX(Int32, Char);
        let TextCursor(x1) = x;
        let ans = x1.countCharX(n, c);
        if (isNull(ans))
            Err(InputMismatchException)
        else 
            Ok(ans)


    pub def manyNotChar(c: Char, x: TextCursor): String & Impure = 
        import flix.runtime.spt.textparser.TextCursor.manyNotChar(Char);
        let TextCursor(x1) = x;
        x1.manyNotChar(c)

    pub def skipNotChar(c: Char, x: TextCursor): Int32 & Impure = 
        import flix.runtime.spt.textparser.TextCursor.skipNotChar(Char);
        let TextCursor(x1) = x;
        x1.skipNotChar(c)

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


    pub def manyOneOf(cs: List[Char], x: TextCursor): String & Impure = 
        import flix.runtime.spt.textparser.TextCursor.manyOneOf(String);        
        let TextCursor(x1) = x;
        let s = String.flatten(List.map(Char.toString, cs));
        x1.manyOneOf(s)

    pub def skipOneOf(cs: List[Char], x: TextCursor): Int & Impure = 
        import flix.runtime.spt.textparser.TextCursor.skipOneOf(String);        
        let TextCursor(x1) = x;
        let s = String.flatten(List.map(Char.toString, cs));
        x1.skipOneOf(s)

    pub def manyNoneOf(cs: List[Char], x: TextCursor): String & Impure = 
        import flix.runtime.spt.textparser.TextCursor.manyNoneOf(String);        
        let TextCursor(x1) = x;
        let s = String.flatten(List.map(Char.toString, cs));
        x1.manyNoneOf(s)

    pub def skipNoneOf(cs: List[Char], x: TextCursor): Int & Impure = 
        import flix.runtime.spt.textparser.TextCursor.skipNoneOf(String);        
        let TextCursor(x1) = x;
        let s = String.flatten(List.map(Char.toString, cs));
        x1.skipNoneOf(s)

    pub def lookingAt(patt: Regex.Pattern, x: TextCursor): Result[String, PrimError] & Impure = 
        import flix.runtime.spt.textparser.TextCursor.lookingAtX(##java.util.regex.Pattern);
        use Regex.Pattern.{Pattern};
        let TextCursor(x1) = x;
        let Pattern(patt1) = patt;
        let ans = x1.lookingAtX(patt1);
        if (isNull(ans))
            Err(InputMismatchException)
        else 
            Ok(ans)

    pub def charsTillString(needle: String, x: TextCursor): String & Impure = 
        import flix.runtime.spt.textparser.TextCursor.charsTillString(String);        
        let TextCursor(x1) = x;
        x1.charsTillString(needle)

    pub def skipCharsTillString(needle: String, x: TextCursor): Int & Impure = 
        import flix.runtime.spt.textparser.TextCursor.skipCharsTillString(String);        
        let TextCursor(x1) = x;
        x1.skipCharsTillString(needle)

    pub def restOfLine(consumeEol: Bool, x: TextCursor): String & Impure = 
        import flix.runtime.spt.textparser.TextCursor.restOfLine(Bool);        
        let TextCursor(x1) = x;
        x1.restOfLine(consumeEol)
    
    pub def skipRestOfLine(consumeEol: Bool, x: TextCursor): Int & Impure = 
        import flix.runtime.spt.textparser.TextCursor.skipRestOfLine(Bool);        
        let TextCursor(x1) = x;
        x1.skipRestOfLine(consumeEol)

    pub def isEof(x: TextCursor): Bool & Impure = 
        import flix.runtime.spt.textparser.TextCursor.isEof();        
        let TextCursor(x1) = x;
        x1.isEof()

}