
def main(_args: Array[String]) : Int32 & Impure = 
    test01();
    test02();
    0

def test01(): Unit & Impure =     
    let parser = FlixParsec/Combinators.many(FlixParsec/Char.anyChar());
    FlixParsec/Prim.parsesString(parser, "ABC") |> println;
    ()

def test02(): Unit & Impure =     
    let parser = FlixParsec/Literal.charLiteral();
    FlixParsec/Prim.parsesString(parser, "'+'") |> println;
    FlixParsec/Prim.parsesString(parser, "'\u00B1'") |> println;        /// Plus_minus
    FlixParsec/Prim.parsesString(parser, "'\u0041'") |> println;        /// upper A
    FlixParsec/Prim.parsesString(parser, "'\u00CA'") |> println;        /// upper E^ circumflex
    FlixParsec/Prim.parsesString(parser, "'\u0077'") |> println;        /// lower w
    ()