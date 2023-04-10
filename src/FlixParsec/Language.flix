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



mod FlixParsec.Language {

    use FlixParsec.{GenParser};
    use FlixParsec.Combinators.{alt};
    use FlixParsec.Char.{letter, alphaNum, oneOf, char};
    use FlixParsec.Token.{LanguageDef};

    pub def emptyDef(): LanguageDef[ka1, st] = 
        let opLetterP = oneOf(
            ':' :: '!' :: '#' :: '$' :: '%' :: '&' :: '*' :: '+' 
            :: '.' :: '/' :: '<' :: '=' :: '>' :: '?' :: '@' :: '\\' 
            :: '^' :: '|' :: '-' :: '~' 
            :: Nil
        );
        { commentStart      = ""
        , commentEnd        = ""
        , commentLine       = ""
        , nestedComments    = true
        , identStart        = alt(letter(), char('_'))
        , identLetter       = alt(alphaNum(), oneOf('_' :: '!' :: Nil)) 
        , opStart           = opLetterP
        , opLetter          = opLetterP
        , reservedOpNames   = Nil
        , reservedNames     = Nil
        , caseSensitive     = true
        }

}
