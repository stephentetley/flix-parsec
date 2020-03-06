

namespace Internal {

    pub def charAtSafe(i: Int32, s: String) : Option[Char] = try {
        String.charAt(i, s) |> Some
    } catch {
        case _: ##java.lang.StringIndexOutOfBoundsException => None
    }

    pub def trySlice(b: Int32, e: Int32, s: String) : Option[String] & Pure = try {
        import java.lang.String.substring(Int32,Int32);
        let s1 = s.substring(b, e) as & Pure;
        Some(s1)
    } catch {
        case _: ##java.lang.IndexOutOfBoundsException => None
    }


    pub def makeString(cs: List[Char]): String = 
        makeStringHelper(cs) as & Pure

    def makeStringHelper(chars: List[Char]): String & Impure = 
        let sb = StringBuilder.new();
        List.foldLeft((_,x) -> StringBuilder.appendChar(sb, x), (), chars);
        StringBuilder.toString(sb)

    pub def prefix(c: Char, s: String): String = Char.toString(c) + s



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

    pub def justSeen(pos: Int32, s: String): String = 
        let len = String.length(s);
        if (pos > len) 
            ""
        else { 
            if (pos > 40) String.slice(pos-40, pos, s) else String.slice(0, pos, s)
        }
            
    pub def pendingInput(pos: Int32, s: String): String = 
        let len = String.length(s);
        if (pos > len) 
            ""
        else { 
            if (pos + 40 < len) String.slice(pos, pos+40, s) else String.slice(pos, len, s)
        }


}