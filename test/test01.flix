
def main(_args: Array[String]) : Int32 & Impure = 
    let parser = FlixParsec/Combinators.many(FlixParsec/Prim.anyChar());
    match FlixParsec/Prim.parsesString(parser, "ABC") { 
        case Ok(xs) => {List.foreach(x -> Console.printLine("${x}"), xs); 0}
        case Err(msg) => {Console.printLine(msg); 1}
    }
