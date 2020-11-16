use TextParser.TextParser;

def p1(): TextParser[List[Char], t] = 
    use TextParser.{anyChar};
    use TextParser/Combinators.{satisfies, sepEndBy};
    sepEndBy(satisfies(anyChar(), x -> x == 'a'), satisfies(anyChar(), x -> x == '-'))

def main(): Result[List[Char], String] & Impure =     
    TextParser.runs(p1(), "a-a-a-a-")