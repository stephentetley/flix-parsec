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

namespace TextParser/Internal/Common {



    pub def textPos(i: Int32, tabsize: Int32, s: String): Option[(Int32, Int32)] = 
        textPosHelper(String.length(s), i, tabsize, s, 1, 1, 0) 

    def textPosHelper(len: Int32, pos: Int32, tabsize: Int32, src: String, linenum: Int32, colnum: Int32, ix: Int32): Option[(Int32, Int32)] = 
        if (ix == pos)
            Some (linenum, colnum)
        else { 
            if (ix > len)
                None        
            else {
                match String.charAt(ix, src) {
                    case '\n' => textPosHelper(len, pos, tabsize, src, linenum+1, 1, ix+1)
                    case '\r' =>
                        if (ix+1 <= pos && ix + 1 <= len) {
                            if (String.charAt(ix+1,src) == '\n') { 
                                // carriage retrun & line feed
                                textPosHelper(len, pos, tabsize, src, linenum+1, 1, ix+2)
                            } else {
                                // jsut carraige return
                                textPosHelper(len, pos, tabsize, src, linenum+1, 1, ix+1)
                            }
                        } else {
                            // At end anyway, just seen newline so increment to next
                            Some(linenum+1, 1)
                        }
                    case '\t' => textPosHelper(len, pos, tabsize, src, linenum, colnum+tabsize, ix+1)
                    case _ => textPosHelper(len, pos, tabsize, src, linenum, colnum+1, ix+1)
                }
            }
        }

    pub def getBeforeAndAfter(pos: Int32, s: String): (String,String) = 
        getBeforeAndAfterHelper(pos, s) as & Pure 

    def getBeforeAndAfterHelper(pos: Int32, s: String): (String,String) & Impure = 
        let (before, after) = String.splitAt(pos, s);
        let beforeArr = String.split(before, "\R{1}");
        let afterArr = String.split(after, "\R{1}");
        let after1 = { 
            if (Array.length(afterArr) > 3) 
                String.unlines(Array.toList(Array.slice(afterArr, 0, 2)))
            else 
                after
        };
        let before1 ={ 
            let len = Array.length(beforeArr);
            if (len > 3) 
                String.unlines(Array.toList(Array.slice(beforeArr, len - 3, len)))
            else 
                before
        }; 
        (before1, after1)


}