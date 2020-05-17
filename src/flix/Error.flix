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

namespace TextParser/Error {


    pub enum PrimError { 
        case IOError,
        case IllegalArgumentException,
        case IllegalStateException,
        case InputMismatchException, 
        case NoSuchElementException,
        case PatternSyntaxException,
        case Generic(String)
    }

    pub def showPrimError (x: PrimError): String = match x {
        case IOError                    => "IOError"
        case IllegalArgumentException   => "IllegalArgumentException"
        case IllegalStateException      => "IllegalStateException"
        case InputMismatchException     => "InputMismatchException"
        case NoSuchElementException     => "NoSuchElementException"
        case PatternSyntaxException     => "PatternSyntaxException"
        case Generic(s)                 => s
    }


    pub opaque type ParseError = 
        { messages: List[String]
        , linenum: Int32
        , column: Int32 
        , justseen: String
        , pending: String 
        }

    pub def makeParseError(input: String, tabsize: Int32, pos: Int32, msg: String): ParseError = 
        let pair = TextParser/Internal/Common.textPos(pos, tabsize, input) |> x -> Option.getWithDefault(x, (1,1));
        let lin = fst(pair);
        let col = snd(pair);
        let (seen, todo) = TextParser/Internal/Common.getBeforeAndAfter(pos, input);
        let ans = { messages = msg :: Nil, linenum = lin, column = col, justseen = seen, pending = todo };
        ParseError(ans)

    pub def nestErrorMessage(outer: String, e: ParseError): ParseError = 
        let ParseError(err) = e;
        let msgs = outer :: err.messages;
        ParseError({ messages = msgs | err })

    pub def swapErrorMessage(msg: String, e: ParseError): ParseError = 
        let ParseError(err) = e;
        let stack = match err.messages {
            case _ :: rs => msg :: rs
            case Nil => msg :: Nil
        };
        ParseError({ messages = stack | err })

    pub def showParserError(e: ParseError): String = 
        let ParseError(err) = e;
        let h1 = "Line: " + Int32.toString(err.linenum) + ", column: " + Int32.toString(err.column);
        let seen = err.justseen + "***";
        let todo = "***" + err.pending;
        let msg = errorMessage(err.messages);
        String.unlines(h1 :: msg :: seen :: todo :: Nil)

    def errorMessage(stk: List[String]): String = match stk {
            case Nil => "*error*"
            case x :: rs => x + errorMessageHelper(rs)
        }

    def errorMessageHelper(rest: List[String]): String = 
        match rest { 
            case Nil => ""
            case x :: Nil => "[" + x + "]"
            case x :: rs => "[" + x + errorMessageHelper(rs) + "]"
        }

}