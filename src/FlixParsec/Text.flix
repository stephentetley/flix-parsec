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


namespace FlixParsec/Text {

    use FlixParsec.TextParser;
    use FlixParsec/Prim.{return, failure, map, lookahead, swapError, liftOption, char};
    use FlixParsec/Combinators.{ignore};
    use FlixParsec/Regex.{lookingAts, skippingAts};

    pub def makePattern(regex: String): TextParser[u, t1, Text/Regex.Pattern] = 
        liftOption(Text/Regex/Pattern.compile(regex),  "makePattern")

    pub def makePatternWithFlags(regex: String, flags: List[Text/Regex/Pattern.Flag]): TextParser[u, t1, Text/Regex.Pattern] = 
        liftOption(Text/Regex/Pattern.compileWithFlags(regex, flags),  "makePatternWithFlags")

    // Parse a newline recognized by the regular expression "\R".
    pub def newline(): TextParser[u, t1, Unit] = skippingAts("\R")
        

    /// Run parser `p` bounding the input to only the rest of the current line.
    /// If `p` succeeds continue parsing the whole input.
    pub def parseLine(p: TextParser[u, t1, a], consumeNewline: Bool): TextParser[u, t1, a] =
        use FlixParsec/Prim.{flatMap, bounded};
        let regex = if (consumeNewline) ".*\R?" else ".*";
        let* restofline = lookahead(lookingAts(regex));
        let* ans = bounded(String.length(restofline), p);
        return(ans)

    /// Parse the rest of the line.
    pub def restOfLine(consumeNewline: Bool): TextParser[u, t1, String] = 
        let regex = if (consumeNewline) ".*\R?" else ".*";
        lookingAts(regex)
        
    /// Peek rest of line.
    pub def peekRestOfLine(): TextParser[u, t1, String] = 
        lookahead(restOfLine(false))

    /// Does not consume input.
    pub def restOfLineLength(): TextParser[u, t1, Int32] = 
        map(String.length, peekRestOfLine())

    /// Skip the rest of the line including the terminating newline if present.
    pub def skipLine(): TextParser[u, t1, Unit] =
        skippingAts(".*\R?")


    
    pub def anchorLineMatches(regex: String): TextParser[u, t1, Unit] = 
        use FlixParsec/Prim.flatMap;        
        let* rest1 = restOfLine(true);
        let* patt = makePattern(regex);
        if (Text/Regex.matches(rest1, patt)) 
            return()
        else
            failure("anchorLineMatches not matched")

    pub def anchorLine(regex: String): TextParser[u, t1, Unit] = 
        use FlixParsec/Prim.flatMap;        
        let* rest1 = restOfLine(true);
        let* patt = makePattern(regex);
        if (Text/Regex.contains(rest1, patt)) 
            return()
        else
            failure("anchorLine not matched")


    pub def tabulation(): TextParser[u, t1, Unit] = ignore(char('\u0009'))

    pub def space(): TextParser[u, t1, Unit] = ignore(char('\u0020'))
    
    pub def carriageReturn(): TextParser[u, t1, Unit] = ignore(char('\u000D'))

    pub def lineFeed(): TextParser[u, t1, Unit] = ignore(char('\u000A'))


    /// zero or more
    pub def whiteSpace(): TextParser[u, t1, Unit] = skippingAts("\s*")

    
    /// one or more
    pub def whiteSpace1(): TextParser[u, t1, Unit] = skippingAts("\s+")

    /// space or tab, zero or more
    pub def blank(): TextParser[u, t1, Unit] = skippingAts("\p{Blank}*")

    
    /// space or tab,  one or more
    pub def blank1(): TextParser[u, t1, Unit] = skippingAts("\p{Blank}+")

    /// zero or more
    pub def horizontalWhiteSpace(): TextParser[u, t1, Unit] = skippingAts("\h*")

    
    /// one or more
    pub def horizontalWhiteSpace1(): TextParser[u, t1, Unit] = skippingAts("\h+")

    pub def manyChar(c: Char): TextParser[u, t1, String] =
        let regex = Text/Regex/Pattern.quote(Char.toString(c)) + "*";
        lookingAts(regex)

    pub def many1Char(c: Char): TextParser[u, t1, String] =
        let regex = Text/Regex/Pattern.quote(Char.toString(c)) + "+";
        lookingAts(regex)

        
    pub def content(n: Int32): TextParser[u, t1, String] =
        map(String.trim, FlixParsec/Prim.horizon(n))

    /// One or more ASCII digits [0-9].
    pub def digits(): TextParser[u, t1, String] = lookingAts("\d+")

    /// One or more ASCII hex digits.
    pub def hexDigits(): TextParser[u, t1, String] = lookingAts("\p{XDigit}+")

    /// One or more.
    pub def lowerLetters(): TextParser[u, t1, String] = lookingAts("\p{Lower}+")

    /// One or more.
    pub def upperLetters(): TextParser[u, t1, String] = lookingAts("\p{Upper}+")

    /// One or more.
    pub def alphas(): TextParser[u, t1, String] = lookingAts("\p{Alpha}+")

    /// One or more.
    pub def alphanums(): TextParser[u, t1, String] = lookingAts("\p{Alnum}+")


    pub def isEof(): TextParser[u, t1, Bool] = 
        FlixParsec/Combinators.succeeds(FlixParsec/Prim.endOfInput())
        
}
