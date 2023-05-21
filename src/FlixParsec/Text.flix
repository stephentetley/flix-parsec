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


    use GradedFunctor.{<$$>}
    use GradedApplicative.{point}
    use GradedMonad.{>>=}

    use FlixParsec.{GenParser}
    use FlixParsec.Prim.{GenParser1, bounded, lookahead, throwError}
    use FlixParsec.Regex.{lookingAt, skippingAt}
    


    // Parse a newline recognized by the regular expression "\R".
    pub def newline(): GenParser[ka, ust, ef, Unit] = skippingAt(regex"\\R")
        

    /// Run parser `p` bounding the input to only the rest of the current line.
    /// If `p` succeeds continue parsing the whole input.
    pub def parseLine(p: GenParser1[ust, ef, a], consumeNewline: Bool): GenParser[ka, ust, ef, a] =
        let regex = if (consumeNewline) regex".*\\R?" else regex".*";
        lookahead(lookingAt(regex))                >>= restofline ->
        bounded(String.length(restofline), p)

    /// Parse the rest of the line.
    pub def restOfLine(consumeNewline: Bool): GenParser[ka, ust, ef, String] = 
        let regex = if (consumeNewline) regex".*\\R?" else regex".*";
        lookingAt(regex)


    /// Peek rest of line.
    pub def peekRestOfLine(): GenParser[ka, ust, ef, String] = 
        lookahead(restOfLine(false))

    /// Does not consume input.
    pub def restOfLineLength(): GenParser[ka, ust, ef, Int32] = 
        peekRestOfLine() <$$> String.length

    /// Skip the rest of the line including the terminating newline if present.
    pub def skipLine(): GenParser[ka, ust, ef, Unit] =
        skippingAt(regex".*\\R?")

    pub def anchorLineMatches(rgx: Regex): GenParser[ka, ust, ef, Unit] =    
        restOfLine(true)                >>= rest1 ->
        if (Regex.isMatch(rgx, rest1)) 
            point()
        else
            throwError("anchorLineMatches not matched")

    pub def anchorLine(rgx: Regex): GenParser[ka, ust, ef, Unit] =        
        restOfLine(true)                        >>= rest1 ->
        if (Regex.isSubmatch(rgx, rest1)) 
            point()
        else
            throwError("anchorLine not matched")

    /// zero or more
    pub def whiteSpace(): GenParser[ka, ust, ef, Unit] = skippingAt(regex"\\s*")

    
    /// one or more
    pub def spaces(): GenParser[ka, ust, ef, Unit] = skippingAt(regex"\\s*")

    /// one or more
    pub def whiteSpace1(): GenParser[ka, ust, ef, Unit] = skippingAt(regex"\\s+")

    /// space or tab, zero or more
    pub def blank(): GenParser[ka, ust, ef, Unit] = skippingAt(regex"\\p{Blank}*")

    
    /// space or tab,  one or more
    pub def blank1(): GenParser[ka, ust, ef, Unit] = skippingAt(regex"\\p{Blank}+")

    /// zero or more
    pub def horizontalWhiteSpace(): GenParser[ka, ust, ef, Unit] = skippingAt(regex"\\h*")

    
    /// one or more
    pub def horizontalWhiteSpace1(): GenParser[ka, ust, ef, Unit] = skippingAt(regex"\\h+")

    pub def manyChar(c: Char): GenParser[ka, ust, ef, String] =
        match String.toRegex(Regex.quote(Char.toString(c)) + "*") {
            case Ok(rgx) => lookingAt(rgx)
            case Err(_)  => throwError("manyChar")
        }
        

    pub def many1Char(c: Char): GenParser[ka, ust, ef, String] =
        match String.toRegex(Regex.quote(Char.toString(c)) + "+") {
            case Ok(rgx) => lookingAt(rgx)
            case Err(_)  => throwError("many1Char")
        }

        
    pub def content(n: Int32): GenParser[ka, ust, ef, String] =
        FlixParsec.Prim.take(n) <$$> String.trim

    /// One or more ASCII digits [0-9].
    pub def digits(): GenParser[ka, ust, ef, String] = lookingAt(regex"\\d+")

    /// One or more ASCII hex digits.
    pub def hexDigits(): GenParser[ka, ust, ef, String] = lookingAt(regex"\\p{XDigit}+")

    /// One or more.
    pub def lowerLetters(): GenParser[ka, ust, ef, String] = lookingAt(regex"\\p{Lower}+")

    /// One or more.
    pub def upperLetters(): GenParser[ka, ust, ef, String] = lookingAt(regex"\\p{Upper}+")

    /// One or more.
    pub def alphas(): GenParser[ka, ust, ef, String] = lookingAt(regex"\\p{Alpha}+")

    /// One or more.
    pub def alphanums(): GenParser[ka, ust, ef,  String] = lookingAt(regex"\\p{Alnum}+")


    pub def isEof(): GenParser[ka, ust, ef,  Bool] = 
        FlixParsec.Combinators.succeeds(FlixParsec.Prim.endOfInput())


}
