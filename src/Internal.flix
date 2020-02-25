

namespace Internal {

    pub def charAtSafe(i: Int32, s: String) : Option[Char] = try {
        String.charAt(i, s) |> Some
    } catch {
        case _: ##java.lang.StringIndexOutOfBoundsException => None
    }

    pub def trySlice(b: Int32, e: Int32, s: String) : Option[String] = try {
        import java.lang.String.substring(Int32,Int32);
        let s1 = s.substring(b, e);
        Some(s1)
    } catch {
        case _: ##java.lang.IndexOutOfBoundsException => None
    }

    /// Being added to Array
    pub def arrayMemberOf(x: a, a: Array[a]): Bool = 
        arrayMemberOfHelper(x, a, 0)

    ///
    /// Helper function for `arrayMemberOf`.
    ///
    def arrayMemberOfHelper(x: a, a: Array[a], i: Int): Bool = 
        if (i >= Array.length(a))
            false
        else { 
            if (a[i] == x) 
                true 
            else 
                arrayMemberOfHelper(x, a, i+1)
        }


    pub def makeString(chars: List[Char]): String = 
        let sb = StringBuilder.new();
        List.foldLeft((_,x) -> StringBuilder.appendChar(sb, x), (), chars);
        StringBuilder.toString(sb)

    pub def prefix(c: Char, s: String): String = Char.toString(c) + s




}