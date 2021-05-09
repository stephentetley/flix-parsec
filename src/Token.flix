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



namespace FlixParsec/Token {

    use FlixParsec.{GenParser};
    use FlixParsec/Prim.{flatMap, return, fail};
    use FlixParsec/Combinators.{alt, ignore, seqRight, skipMany1, skipMany, between, 
        sepBy, sepBy1, notFollowedBy, manyWith, sepByWith};
    use FlixParsec/Combinators.{*>};
    use FlixParsec/Char.{string, char, oneOf, noneOf, satisfy};
    

    type alias LanguageDef[ka1, st] = 
        { commentStart: String
        , commentEnd: String
        , commentLine: String
        , nestedComments: Bool  
        , identStart: GenParser[ka1, st, Char]
        , identLetter: GenParser[ka1, st, Char]
        , opStart: GenParser[ka1, st, Char]
        , opLetter: GenParser[ka1, st, Char]
        , reservedNames: List[String]
        , reservedOpNames: List[String]
        , caseSensitive: Bool
        }

    enum Status { 
        case More
        case Done
    }


    pub def parens(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, a] =  
        between(symbol(langDef, "("), symbol(langDef, ")"), p)

    pub def braces(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, a] =  
        between(symbol(langDef, "{"), symbol(langDef, "}"), p)

    pub def angles(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, a] =  
        between(symbol(langDef, "<"), symbol(langDef, ">"), p)

    pub def brackets(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, a] =  
        between(symbol(langDef, "["), symbol(langDef, "]"), p)

    pub def squotes(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, a] =  
        between(symbol(langDef, "'"), symbol(langDef, "'"), p)

    pub def dquotes(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, a] = 
        let dquote = Char.toString('\u0022');
        between(symbol(langDef, dquote), symbol(langDef, dquote), p)
        
    pub def semi(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, String] =  
        symbol(langDef, ";")

    pub def comma(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, String] =  
        symbol(langDef, ",")

    pub def colon(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, String] =  
        symbol(langDef, ":")

    pub def dot(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, String] =  
        symbol(langDef, ".")

    pub def semiSep(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] =  
        sepBy(p, semi(langDef))
    
    pub def commaSep(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] =  
        sepBy(p, comma(langDef))

    pub def semiSep1(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] =  
        sepBy1(p, semi(langDef))
    
    pub def commaSep1(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] =  
        sepBy1(p, comma(langDef))


    pub def tupled(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = 
        parens(langDef, commaSep(langDef, p))

    pub def tupled1(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = 
        parens(langDef, commaSep1(langDef, p))

    pub def commaBraces(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = 
        braces(langDef, commaSep(langDef, p))
    
    pub def commaBraces1(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = 
        braces(langDef, commaSep1(langDef, p))

    pub def commaBrackets(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = 
        brackets(langDef, commaSep(langDef, p))
    
    pub def commaBrackets1(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = 
        brackets(langDef, commaSep1(langDef, p))

    pub def semiBraces(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = 
        braces(langDef, semiSep(langDef, p))
    
    pub def semiBraces1(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = 
        braces(langDef, semiSep1(langDef, p))

    pub def semiBrackets(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = 
        brackets(langDef, semiSep(langDef, p))
    
    pub def semiBrackets1(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] =
        brackets(langDef, semiSep1(langDef, p))

    /// TODO - need rest of family of functions implementing...
    pub def commaSepWith(langDef: LanguageDef[ka1, st], 
                        combine: (ac, a) -> ac,
                        acc: ac,
                        p: GenParser[ka1, st, a]): GenParser[ka1, st, ac] =  
        sepByWith(combine, acc, p, comma(langDef))

    pub def commaBracesWith(langDef: LanguageDef[ka1, st], 
                            combine: (ac, a) -> ac,
                            acc: ac,
                            p: GenParser[ka1, st, a]): GenParser[ka1, st, List[a]] = 
        braces(langDef, commaSepWith(langDef, combine, acc, p))


    pub def charLiteral(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, Char] =  
        lexeme(langDef, FlixParsec/Literal.charLiteral())

    pub def stringLiteral(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, String] =  
        lexeme(langDef, FlixParsec/Literal.stringLiteral())



    pub def int8(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, Int8] = 
        lexeme(langDef, FlixParsec/Literal.int8())

    pub def int16(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, Int16] = 
        lexeme(langDef, FlixParsec/Literal.int16())
    
    pub def int32(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, Int32] = 
        lexeme(langDef, FlixParsec/Literal.int32())
    
    pub def int64(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, Int64] = 
        lexeme(langDef, FlixParsec/Literal.int64())

    pub def bigInt(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, BigInt] = 
        lexeme(langDef, FlixParsec/Literal.bigInt())

    pub def float32(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, Float32] = 
        lexeme(langDef, FlixParsec/Literal.float32())

    pub def float64(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, Float64] = 
        lexeme(langDef, FlixParsec/Literal.float64())

    pub def hexInt32(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, Int32] = 
        lexeme(langDef, FlixParsec/Literal.hexInt32())

    pub def octInt32(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, Int32] = 
        lexeme(langDef, FlixParsec/Literal.octInt32())

    pub def symbol(langDef: LanguageDef[ka1, st], name: String): GenParser[ka1, st, String] = 
        lexeme(langDef, string(name))

    pub def lexeme(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, a] = 
        let* a  = p;
        let* _  = whiteSpace(langDef);
        return(a)

    /// White space

    pub def whiteSpace(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, Unit] = 
        let noLine = String.isEmpty(langDef.commentLine);
        let noMulti = String.isEmpty(langDef.commentStart);
        match (noLine, noMulti) {
            case (true, true) => skipMany(simpleSpace())
            case (true, _) => skipMany(simpleSpace() `alt` multilineComment(langDef))
            case (_, true) => skipMany(simpleSpace() `alt` onelineComment(langDef))
            case (_, _) => skipMany(simpleSpace() `alt` (onelineComment(langDef) `alt` multilineComment(langDef)))
        }


    def simpleSpace(): GenParser[ka1, st, Unit] =  
        skipMany1(satisfy(Char.isWhiteSpace))

    def onelineComment(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, Unit] = 
        use FlixParsec/Text.{restOfLine};
        (string(langDef.commentLine) *> restOfLine(true)) 
            |> ignore

    def multilineComment(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, Unit] = 
        string(langDef.commentStart) *> inComment(langDef)

    def inComment(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, Unit] = 
        if (langDef.nestedComments) 
            inCommentMulti(langDef) 
        else 
            inCommentSingle(langDef)

    def inCommentMulti(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, Unit] = 
        let startEnd = List.distinct(String.toList(langDef.commentEnd) ::: String.toList(langDef.commentStart));
        let endP = string(langDef.commentEnd)   *> return(Done);
        let p1 = multilineComment(langDef)      *> return(More);
        let p2 = skipMany1(noneOf(startEnd))    *> return(More);
        let p3 = oneOf(startEnd)                *> return(More);
        let* ans = endP `alt` (p1 `alt` (p2 `alt` p3));
        match ans { 
            case Done => return()
            case More => inCommentSingle(langDef)
        }

    def inCommentSingle(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, Unit] = 
        let startEnd = List.distinct(String.toList(langDef.commentEnd) ::: String.toList(langDef.commentStart));
        let endP = string(langDef.commentEnd) *> return(Done);
        let p1 = skipMany1(noneOf(startEnd)) *> return(More);
        let p2 = oneOf(startEnd) *> return(More);
        let* ans = endP `alt` p1 `alt` p2;
        match ans { 
            case Done => return()
            case More => inCommentSingle(langDef)
        }

    /// Reserved

    pub def reserved(langDef: LanguageDef[ka1, st], name: String): GenParser[ka1, st, Unit] = 
        lexeme(langDef, reservedHelper(langDef, name)) 

    def reservedHelper(langDef: LanguageDef[ka1, st], name: String): GenParser[ka1, st, Unit] =
        let* _  = caseString(langDef, name);
        let* _  = notFollowedBy(langDef.identLetter);
        return()

    def caseString(langDef: LanguageDef[ka1, st], name: String): GenParser[ka1, st, Unit] =
        if (langDef.caseSensitive) 
            ignore(string(name))
        else 
            walk(String.toList(name))

    def walk(cs: List[Char]): GenParser[ka1, st, Unit] =
        match cs { 
            case c :: rs => { 
                let* _ = caseChar(c);
                walk(rs)
            }
            case Nil => return()
        }           
    
    def caseChar(c: Char): GenParser[ka1, st, Char] = 
        if (Char.isLetter(c)) 
            alt(char(Char.toUpperCase(c)), char(Char.toLowerCase(c)))
        else
            char(c)

    /// Identifier

    pub def identifier(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, String] = 
        lexeme(langDef, identifierHelper(langDef))

    def identifierHelper(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, String] = 
        let* name = ident(langDef);
        if (isReservedName(langDef, name))
            fail("reserved word: ${name}")
        else 
            return(name) 


    def ident(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, String] = 
        let* c1 = langDef.identStart;
        let* s  = manyWith((ac, x) -> "${ac}${x}", "${c1}", langDef.identLetter);
        return(s)

    def isReservedName(langDef: LanguageDef[ka1, st], name: String): Bool = 
        if (langDef.caseSensitive)
            List.exists(x -> x == name, langDef.reservedNames)
        else
            List.exists(x -> equalsCI(x, name), langDef.reservedNames)

    def equalsCI(s1: String, s2: String): Bool = equalsCIHelper(s1, s2) as & Pure 

    def equalsCIHelper(s1: String, s2: String): Bool & Impure = 
        use Text/Collator.CollatorStrength.{Primary};
        let collator = Text/Collator.getInstance();
        Text/Collator.setStrength!(collator, Primary);
        Text/Collator.equals(collator, s1, s2)

    /// Reserved-op
    pub def reservedOp(langDef: LanguageDef[ka1, st], name: String): GenParser[ka1, st, Unit] = 
        lexeme(langDef, reservedOpHelper(langDef, name)) 

    def reservedOpHelper(langDef: LanguageDef[ka1, st], name: String): GenParser[ka1, st, Unit] =
        let* _  = string(name);
        let* _  = notFollowedBy(langDef.opLetter);
        return()

    /// Operator
    pub def operator(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, String] = 
        lexeme(langDef, operatorHelper(langDef))

    def operatorHelper(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, String] = 
        let* name = oper(langDef);
        if (isReservedOp(langDef, name))
            fail("reserved word: ${name}")
        else 
            return(name) 

    def oper(langDef: LanguageDef[ka1, st]): GenParser[ka1, st, String] = 
        let* c1 = langDef.opStart;
        let* s  = manyWith((ac, x) -> "${ac}${x}", "${c1}", langDef.opLetter);
        return(s)

    def isReservedOp(langDef: LanguageDef[ka1, st], name: String): Bool = 
        List.exists(x -> x == name, langDef.reservedOpNames)

}
