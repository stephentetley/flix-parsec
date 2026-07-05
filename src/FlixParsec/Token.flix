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



pub mod FlixParsec.Token {

    use Applicative.{<*, *>}
    use Applicative.{point}
    use Monad.{>>=}

    use FlixParsec.GenParser.{GenParser}
    use FlixParsec.GenParser.{throwError}
    use FlixParsec.Combinators.{alt, between, manyWith, skipMany, skipMany1, 
        notFollowedBy, sepBy, sepBy1, pchoice}
    use FlixParsec.Char.{satisfy, char, string, oneOf, noneOf}
    
    use FlixParsec.Token.Status.{Done, More}



    pub type alias LanguageDef[ka: Type, ust: Type] = 
        { commentStart = String
        , commentEnd = String
        , commentLine = String
        , nestedComments = Bool  
        , identStart = GenParser[ka, ust, Char]
        , identLetter = GenParser[ka, ust, Char]
        , opStart = GenParser[ka, ust, Char]
        , opLetter = GenParser[ka, ust, Char]
        , reservedNames = List[String]
        , reservedOpNames = List[String]
        , caseSensitive = Bool
        }

    enum Status with Eq { 
        case More
        case Done
    }


    pub def parens(langDef: LanguageDef[ka, ust], p: GenParser[ka, ust, a]): GenParser[ka, ust, a] =  
        between(symbol(langDef, "("), symbol(langDef, ")"), p)

    pub def braces(langDef: LanguageDef[ka, ust], p: GenParser[ka, ust, a]): GenParser[ka, ust, a] =  
        between(symbol(langDef, "{"), symbol(langDef, "}"), p)

    pub def angles(langDef: LanguageDef[ka, ust], p: GenParser[ka, ust, a]): GenParser[ka, ust, a] =  
        between(symbol(langDef, "<"), symbol(langDef, ">"), p)

    pub def brackets(langDef: LanguageDef[ka, ust], p: GenParser[ka, ust, a]): GenParser[ka, ust, a] =  
        between(symbol(langDef, "["), symbol(langDef, "]"), p)

    pub def squotes(langDef: LanguageDef[ka, ust], p: GenParser[ka, ust, a]): GenParser[ka, ust, a] =  
        between(symbol(langDef, "'"), symbol(langDef, "'"), p)

    pub def dquotes(langDef: LanguageDef[ka, ust], p: GenParser[ka, ust, a]): GenParser[ka, ust, a] = 
        let dquote = Char.toString('\u0022');
        between(symbol(langDef, dquote), symbol(langDef, dquote), p)
        
    pub def semi(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, String] =  
        symbol(langDef, ";")

    pub def comma(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, String] =  
        symbol(langDef, ",")

    pub def colon(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, String] =  
        symbol(langDef, ":")

    pub def dot(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, String] =  
        symbol(langDef, ".")

    pub def semiSep(langDef: LanguageDef[ka, ust], p: GenParser[ka, ust, a]): GenParser[ka, ust, List[a]] =  
        sepBy(p, semi(langDef))
    
    pub def commaSep(langDef: LanguageDef[ka, ust], p: GenParser[ka, ust, a]): GenParser[ka, ust, List[a]] =  
        sepBy(p, comma(langDef))

    pub def semiSep1(langDef: LanguageDef[ka, ust], p: GenParser[ka, ust, a]): GenParser[ka, ust, List[a]] =  
        sepBy1(p, semi(langDef))
    
    pub def commaSep1(langDef: LanguageDef[ka, ust], p: GenParser[ka, ust, a]): GenParser[ka, ust, List[a]] =  
        sepBy1(p, comma(langDef))


    pub def charLiteral(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, Char] =  
        lexeme(langDef, FlixParsec.Literal.charLiteral())

    pub def stringLiteral(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, String] =  
        lexeme(langDef, FlixParsec.Literal.stringLiteral())

    pub def int8(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, Int8] = 
        lexeme(langDef, FlixParsec.Literal.int8())

    pub def int16(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, Int16] = 
        lexeme(langDef, FlixParsec.Literal.int16())
    
    pub def int32(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, Int32] = 
        lexeme(langDef, FlixParsec.Literal.int32())
    
    pub def int64(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, Int64] = 
        lexeme(langDef, FlixParsec.Literal.int64())

    pub def bigInt(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, BigInt] = 
        lexeme(langDef, FlixParsec.Literal.bigInt())

    pub def float32(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, Float32] = 
        lexeme(langDef, FlixParsec.Literal.float32())

    pub def float64(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, Float64] = 
        lexeme(langDef, FlixParsec.Literal.float64())

    pub def hexInt32(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, Int32] = 
        lexeme(langDef, FlixParsec.Literal.hexInt32())

    pub def octInt32(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, Int32] = 
        lexeme(langDef, FlixParsec.Literal.octInt32())

    pub def symbol(langDef: LanguageDef[ka, ust], name: String): GenParser[ka, ust, String] = 
        lexeme(langDef, string(name))

    pub def lexeme(langDef: LanguageDef[ka, ust], p: GenParser[ka, ust, a]): GenParser[ka, ust, a] = 
        p <* whiteSpace(langDef)

    /// White space

    pub def whiteSpace(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, Unit] = 
        let noLine = String.isEmpty(langDef#commentLine);
        let noMulti = String.isEmpty(langDef#commentStart);
        match (noLine, noMulti) {
            case (true, true)   => skipMany(simpleSpace())
            case (true, _)      => skipMany(alt(simpleSpace(), multilineComment(langDef)))
            case (_, true)      => skipMany(alt(simpleSpace(), onelineComment(langDef)))
            case (_, _)         => skipMany(pchoice(List#{simpleSpace(), onelineComment(langDef),multilineComment(langDef)}))
        }


    def simpleSpace(): GenParser[ka, ust, Unit] =  
        skipMany1(satisfy(Char.isWhitespace))

    def onelineComment(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, Unit] = 
        use FlixParsec.Text.{restOfLine};
        (string(langDef#commentLine) *> restOfLine(true)) 
            |> Functor.ignore

    def multilineComment(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, Unit] = 
        string(langDef#commentStart) *> inComment(langDef)

    def inComment(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, Unit] = 
        if (langDef#nestedComments) 
            inCommentMulti(langDef) 
        else 
            inCommentSingle(langDef)

    def inCommentMulti(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, Unit] = 
        let startEnd = List.distinct(String.toList(langDef#commentEnd) ::: String.toList(langDef#commentStart));
        let endP = string(langDef#commentEnd)   *> point(Done);
        let p1 = multilineComment(langDef)      *> point(More);
        let p2 = skipMany1(noneOf(startEnd))    *> point(More);
        let p3 = oneOf(startEnd)                *> point(More);
        pchoice(List#{endP, p1, p2, p3})  >>= ans ->
        match ans { 
            case Done => point()
            case More => inCommentSingle(langDef)
        }

    def inCommentSingle(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, Unit] = 
        let startEnd = List.distinct(String.toList(langDef#commentEnd) ::: String.toList(langDef#commentStart));
        let endP = string(langDef#commentEnd) *> point(Done);
        let p1 = skipMany1(noneOf(startEnd)) *> point(More);
        let p2 = oneOf(startEnd) *> point(More);
        pchoice(List#{endP, p1, p2}) >>= ans -> 
        match ans { 
            case Done => point()
            case More => inCommentSingle(langDef)
        }

    /// Reserved

    pub def reserved(langDef: LanguageDef[ka, ust], name: String): GenParser[ka, ust, Unit] = 
        lexeme(langDef, reservedHelper(langDef, name)) 

    def reservedHelper(langDef: LanguageDef[ka, ust], name: String): GenParser[ka, ust, Unit] =
        caseString(langDef, name) *> notFollowedBy(langDef#identLetter) *> point()

    def caseString(langDef: LanguageDef[ka, ust], name: String): GenParser[ka, ust, Unit] =
        if (langDef#caseSensitive) 
            Functor.ignore(string(name))
        else 
            walk(String.toList(name))

    def walk(cs: List[Char]): GenParser[ka, ust, Unit] =
        match cs { 
            case c :: rs => caseChar(c) *> walk(rs)
            case Nil     => point()
        }           
    
    def caseChar(c: Char): GenParser[ka, ust, Char] = 
        if (Char.isLetter(c)) 
            alt(char(Char.toUpperCase(c)), char(Char.toLowerCase(c)))
        else
            char(c)

    /// Identifier

    pub def identifier(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, String] = 
        lexeme(langDef, identifierHelper(langDef))

    def identifierHelper(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, String] = 
        ident(langDef) >>= name -> 
        if (isReservedName(langDef, name))
            throwError("reserved word: ${name}")
        else 
            point(name) 


    def ident(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, String] = 
        langDef#identStart >>= c1 ->
        manyWith((ac, x) -> "${ac}${x}", "${c1}", langDef#identLetter)


    def isReservedName(langDef: LanguageDef[ka, ust], name: String): Bool = 
        if (langDef#caseSensitive)
            List.exists(x -> x == name, langDef#reservedNames)
        else
            List.exists(x -> FlixParsec.Internal.SimpleCollator.caseInsenstiveEquals(x, name),
                         langDef#reservedNames)



    /// Reserved-op
    pub def reservedOp(langDef: LanguageDef[ka, ust], name: String): GenParser[ka, ust, Unit] = 
        lexeme(langDef, reservedOpHelper(langDef, name)) 

    def reservedOpHelper(langDef: LanguageDef[ka, ust], name: String): GenParser[ka, ust, Unit] =
        string(name) *> notFollowedBy(langDef#opLetter) *> point()

    /// Operator
    pub def operator(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, String] = 
        lexeme(langDef, operatorHelper(langDef))

    def operatorHelper(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, String] = 
        oper(langDef) >>= name -> 
        if (isReservedOp(langDef, name))
            throwError("reserved word: ${name}")
        else 
            point(name) 

    def oper(langDef: LanguageDef[ka, ust]): GenParser[ka, ust, String] = 
        langDef#opStart     >>= c1 ->
        manyWith((ac, x) -> "${ac}${x}", "${c1}", langDef#opLetter)

    def isReservedOp(langDef: LanguageDef[ka, ust], name: String): Bool = 
        List.exists(x -> x == name, langDef#reservedOpNames)

}
