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
    use FlixParsec/Prim.{flatMap, return};
    use FlixParsec/Combinators.{alt, ignore, seqRight, skipMany1, skipMany, between};
    use FlixParsec/Combinators.{*>};
    use FlixParsec/Char.{string, oneOf, noneOf, satisfy};
    

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

    pub def symbol(langDef: LanguageDef[ka1, st], name: String): GenParser[ka1, st, String] = 
        lexeme(langDef, string(name))

    pub def lexeme(langDef: LanguageDef[ka1, st], p: GenParser[ka1, st, a]): GenParser[ka1, st, a] = 
        let* a  = p;
        let* _  = whiteSpace(langDef);
        return(a)


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

    

}
