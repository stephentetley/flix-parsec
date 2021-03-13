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

    use FlixParsec.{GenParser};
    use FlixParsec/Prim.{return, failure, map, lookahead, swapError, liftOption, char};
    use FlixParsec/Combinators.{ignore};
    use FlixParsec/Regex.{lookingAts, skippingAts};

    pub def makePattern(regex: String): GenParser[t1, u, Text/Regex.Pattern] = 
        liftOption(Text/Regex/Pattern.compile(regex),  "makePattern")

    pub def makePatternWithFlags(regex: String, flags: List[Text/Regex/Pattern.Flag]): GenParser[t1, u, Text/Regex.Pattern] = 
        liftOption(Text/Regex/Pattern.compileWithFlags(regex, flags),  "makePatternWithFlags")

    // Parse a newline recognized by the regular expression "\R".
    pub def newline(): GenParser[t1, u, Unit] = skippingAts("\R")
        

    /// Run parser `p` bounding the input to only the rest of the current line.
    /// If `p` succeeds continue parsing the whole input.
    pub def parseLine(p: GenParser[t1, u, a], consumeNewline: Bool): GenParser[t1, u, a] =
        use FlixParsec/Prim.{flatMap, bounded};
        let regex = if (consumeNewline) ".*\R?" else ".*";
        let* restofline = lookahead(lookingAts(regex));
        let* ans = bounded(String.length(restofline), p);
        return(ans)

    /// Parse the rest of the line.
    pub def restOfLine(consumeNewline: Bool): GenParser[t1, u, String] = 
        let regex = if (consumeNewline) ".*\R?" else ".*";
        lookingAts(regex)
        
    /// Peek rest of line.
    pub def peekRestOfLine(): GenParser[t1, u, String] = 
        lookahead(restOfLine(false))

    /// Does not consume input.
    pub def restOfLineLength(): GenParser[t1, u, Int32] = 
        map(String.length, peekRestOfLine())

    /// Skip the rest of the line including the terminating newline if present.
    pub def skipLine(): GenParser[t1, u, Unit] =
        skippingAts(".*\R?")


    
    pub def anchorLineMatches(regex: String): GenParser[t1, u, Unit] = 
        use FlixParsec/Prim.flatMap;        
        let* rest1 = restOfLine(true);
        let* patt = makePattern(regex);
        if (Text/Regex.matches(rest1, patt)) 
            return()
        else
            failure("anchorLineMatches not matched")

    pub def anchorLine(regex: String): GenParser[t1, u, Unit] = 
        use FlixParsec/Prim.flatMap;        
        let* rest1 = restOfLine(true);
        let* patt = makePattern(regex);
        if (Text/Regex.contains(rest1, patt)) 
            return()
        else
            failure("anchorLine not matched")


    pub def tabulation(): GenParser[t1, u, Unit] = ignore(char('\u0009'))

    pub def space(): GenParser[t1, u, Unit] = ignore(char('\u0020'))
    
    pub def carriageReturn(): GenParser[t1, u, Unit] = ignore(char('\u000D'))

    pub def lineFeed(): GenParser[t1, u, Unit] = ignore(char('\u000A'))


    /// zero or more
    pub def whiteSpace(): GenParser[t1, u, Unit] = skippingAts("\s*")

    
    /// one or more
    pub def whiteSpace1(): GenParser[t1, u, Unit] = skippingAts("\s+")

    /// space or tab, zero or more
    pub def blank(): GenParser[t1, u, Unit] = skippingAts("\p{Blank}*")

    
    /// space or tab,  one or more
    pub def blank1(): GenParser[t1, u, Unit] = skippingAts("\p{Blank}+")

    /// zero or more
    pub def horizontalWhiteSpace(): GenParser[t1, u, Unit] = skippingAts("\h*")

    
    /// one or more
    pub def horizontalWhiteSpace1(): GenParser[t1, u, Unit] = skippingAts("\h+")

    pub def manyChar(c: Char): GenParser[t1, u, String] =
        let regex = Text/Regex/Pattern.quote(Char.toString(c)) + "*";
        lookingAts(regex)

    pub def many1Char(c: Char): GenParser[t1, u, String] =
        let regex = Text/Regex/Pattern.quote(Char.toString(c)) + "+";
        lookingAts(regex)

        
    pub def content(n: Int32): GenParser[t1, u, String] =
        map(String.trim, FlixParsec/Prim.horizon(n))

    /// One or more ASCII digits [0-9].
    pub def digits(): GenParser[t1, u, String] = lookingAts("\d+")

    /// One or more ASCII hex digits.
    pub def hexDigits(): GenParser[t1, u, String] = lookingAts("\p{XDigit}+")

    /// One or more.
    pub def lowerLetters(): GenParser[t1, u, String] = lookingAts("\p{Lower}+")

    /// One or more.
    pub def upperLetters(): GenParser[t1, u, String] = lookingAts("\p{Upper}+")

    /// One or more.
    pub def alphas(): GenParser[t1, u, String] = lookingAts("\p{Alpha}+")

    /// One or more.
    pub def alphanums(): GenParser[t1, u, String] = lookingAts("\p{Alnum}+")


    pub def isEof(): GenParser[t1, u, Bool] = 
        FlixParsec/Combinators.succeeds(FlixParsec/Prim.endOfInput())
        
}
