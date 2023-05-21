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



mod FlixParsec.Internal.ParseError {

    ///
    /// Note `SourcePosition` is derived when there is a parse error. 
    /// During parsing, the parse state tracks just an index (cursor) into the input string.
    ///
    pub type alias SourcePosition = 
        { lineNumber = Int32
        , column = Int32 
        }

    pub type alias InternalError = 
        { message = String
        , position = Int32 
        }

    pub type alias ParseError = 
        { message = String
        , position = SourcePosition
        , textBefore = String
        , textAfter = String
        }

    pub def makeParseError(tabSize: {tabSize = Int32}, err: InternalError, input: String): ParseError = 
        let (before, after) = getTextBeforeAndAfter(err.position, input);
        { message    = err.message
        , position   = sourcePosition(tabSize, position = err.position, input)
        , textBefore = before
        , textAfter  = after
        }


    pub def showSourcePosition(pos: SourcePosition): String = 
        "Line=${pos.lineNumber}, Col=${pos.column}"

    pub def samePosition(pos1: SourcePosition, pos2: SourcePosition): Bool =
        pos1.lineNumber == pos2.lineNumber and pos1.column == pos2.column

    pub def showParseError(err: ParseError): String = 
        String.unlines(err.message :: showSourcePosition(err.position) 
                            :: "Seen:" :: "-----" :: err.textBefore 
                            :: "Pending:" :: "--------" :: err.textAfter :: Nil)


    pub def fileNotFoundError(path: String): ParseError = 
        { message = "File not found: ${path}"
        , position = {lineNumber = 0, column = 0}
        , textBefore = ""
        , textAfter = ""
        }

    pub def generalError(msg: String): ParseError = 
        { message = msg
        , position = {lineNumber = 0, column = 0}
        , textBefore = ""
        , textAfter = ""
        }

    pub def sourcePosition(tabSize: {tabSize = Int32}, position: {position = Int32}, s: String): SourcePosition = 
        match textPosHelper(String.length(s), position.position, tabSize.tabSize, s, 1, 1, 0) {
            case Some(sp)   => sp
            case None       => {lineNumber = 0, column = 0}
        }


    /// TODO - we could do a simpler version of this with Regex ranges and `countMatch("\R")`
    def textPosHelper(len: Int32, 
                        pos: Int32, 
                        tabsize: Int32, 
                        src: String, 
                        linenum: Int32, 
                        col: Int32, 
                        ix: Int32): Option[SourcePosition] = 
        if (ix == pos)
            Some({lineNumber = linenum, column = col})
        else { 
            if (ix > len)
                None        
            else {
                match String.charAt(ix, src) {
                    case '\n' => textPosHelper(len, pos, tabsize, src, linenum+1, 1, ix+1)
                    case '\r' =>
                        if (ix+1 <= pos and ix + 1 <= len) {
                            if (String.charAt(ix+1, src) == '\n') { 
                                // carriage return & line feed
                                textPosHelper(len, pos, tabsize, src, linenum+1, 1, ix+2)
                            } else {
                                // jsut carriage return
                                textPosHelper(len, pos, tabsize, src, linenum+1, 1, ix+1)
                            }
                        } else {
                            // At end anyway, just seen newline so increment to next
                            Some({lineNumber = linenum+1, column = 1})
                        }
                    case '\t' => textPosHelper(len, pos, tabsize, src, linenum, col + tabsize, ix+1)
                    case _ => textPosHelper(len, pos, tabsize, src, linenum, col + 1, ix+1)
                }
            }
        }

    pub def getTextBeforeAndAfter(pos: Int32, s: String): (String, String) = 
        let (before, after) = String.splitAt(pos, s);
        let befores = String.split({regex = "\\R{1}"}, before);
        let afters = String.split({regex = "\\R{1}"}, after);
        let after1 = { 
            if (List.length(afters) > 3) 
                String.unlines(List.slice(start = 0, end = 2, afters))
            else 
                after
        };
        let before1 ={ 
            let len = List.length(befores);
            if (len > 3) 
                String.unlines(List.slice(start = len - 3, end = len, befores))
            else 
                before
        }; 
        (before1, after1)

}
