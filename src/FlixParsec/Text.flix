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


mod FlixParsec.Text {

    use Functor.{<$>};
    use Applicative.{point};

    use FlixParsec.{GenParser};
    use FlixParsec.Prim.{flatMap, fail, lookahead, swapError, liftResult1};    
    use FlixParsec.Regex.{lookingAts, skippingAts};
    use FlixParsec.Char.{char};
    

    pub def makePattern(regex: String): GenParser[ka1, st, Text.Regex.Pattern] = 
        liftResult1(Text.Regex.Pattern.compile(regex), _ -> "makePattern")

    pub def makePatternWithFlags(regex: String, flags: List[Text.Regex.Pattern.Flag]): GenParser[ka1, st, Text.Regex.Pattern] = 
        liftResult1(Text.Regex.Pattern.compileWithFlags(regex, flags), _ -> "makePatternWithFlags")

    // Parse a newline recognized by the regular expression "\R".
    pub def newline(): GenParser[ka1, st, Unit] = skippingAts("\\R")
        

    /// Run parser `p` bounding the input to only the rest of the current line.
    /// If `p` succeeds continue parsing the whole input.
    pub def parseLine(p: GenParser[ka1, st, a], consumeNewline: Bool): GenParser[ka1, st, a] =
        use FlixParsec.Prim.{bounded};
        let regex = if (consumeNewline) ".*\\R?" else ".*";
        let* restofline = lookahead(lookingAts(regex));
        let* ans = bounded(String.length(restofline), p);
        point(ans)

    /// Parse the rest of the line.
    pub def restOfLine(consumeNewline: Bool): GenParser[ka1, st, String] = 
        let regex = if (consumeNewline) ".*\\R?" else ".*";
        lookingAts(regex)
        
    /// Peek rest of line.
    pub def peekRestOfLine(): GenParser[ka1, st, String] = 
        lookahead(restOfLine(false))

    /// Does not consume input.
    pub def restOfLineLength(): GenParser[ka1, st, Int32] = 
        String.length <$> peekRestOfLine()

    /// Skip the rest of the line including the terminating newline if present.
    pub def skipLine(): GenParser[ka1, st, Unit] =
        skippingAts(".*\\R?")


    
    pub def anchorLineMatches(regex: String): GenParser[ka1, st, Unit] =    
        let* rest1 = restOfLine(true);
        let* patt = makePattern(regex);
        if (Text.Regex.isMatch(patt, rest1)) 
            point()
        else
            fail("anchorLineMatches not matched")

    pub def anchorLine(regex: String): GenParser[ka1, st, Unit] =        
        let* rest1 = restOfLine(true);
        let* patt = makePattern(regex);
        if (Text.Regex.isSubmatch(patt, rest1)) 
            point()
        else
            fail("anchorLine not matched")




    /// zero or more
    pub def whiteSpace(): GenParser[ka1, st, Unit] = skippingAts("\\s*")

    
    /// one or more
    pub def spaces(): GenParser[ka1, st, Unit] = skippingAts("\\s*")

    /// one or more
    pub def whiteSpace1(): GenParser[ka1, st, Unit] = skippingAts("\\s+")

    /// space or tab, zero or more
    pub def blank(): GenParser[ka1, st, Unit] = skippingAts("\\p{Blank}*")

    
    /// space or tab,  one or more
    pub def blank1(): GenParser[ka1, st, Unit] = skippingAts("\\p{Blank}+")

    /// zero or more
    pub def horizontalWhiteSpace(): GenParser[ka1, st, Unit] = skippingAts("\\h*")

    
    /// one or more
    pub def horizontalWhiteSpace1(): GenParser[ka1, st, Unit] = skippingAts("\\h+")

    pub def manyChar(c: Char): GenParser[ka1, st, String] =
        let regex = Text.Regex.Pattern.quote(Char.toString(c)) + "*";
        lookingAts(regex)

    pub def many1Char(c: Char): GenParser[ka1, st, String] =
        let regex = Text.Regex.Pattern.quote(Char.toString(c)) + "+";
        lookingAts(regex)

        
    pub def content(n: Int32): GenParser[ka1, st, String] =
        String.trim <$> FlixParsec.Prim.horizon(n)

    /// One or more ASCII digits [0-9].
    pub def digits(): GenParser[ka1, st, String] = lookingAts("\\d+")

    /// One or more ASCII hex digits.
    pub def hexDigits(): GenParser[ka1, st, String] = lookingAts("\\p{XDigit}+")

    /// One or more.
    pub def lowerLetters(): GenParser[ka1, st, String] = lookingAts("\\p{Lower}+")

    /// One or more.
    pub def upperLetters(): GenParser[ka1, st, String] = lookingAts("\\p{Upper}+")

    /// One or more.
    pub def alphas(): GenParser[ka1, st, String] = lookingAts("\\p{Alpha}+")

    /// One or more.
    pub def alphanums(): GenParser[ka1, st, String] = lookingAts("\\p{Alnum}+")


    pub def isEof(): GenParser[ka1, st, Bool] = 
        FlixParsec.Combinators.succeeds(FlixParsec.Prim.endOfInput())
        
}
