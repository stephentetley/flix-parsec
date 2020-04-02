use TextParser.Parser;

@test
def fail01() : Result[Unit, String] = 
    TextParser.runs(TextParser.fail("fail"), "")


@test
def test01(): Result[Char, String] = 
    use TextParser.satisfy;
    let p1: Parser[Char] = satisfy(_ -> true);
    TextParser.runs(p1, "abcdefghij")

@test
def test02(): Result[List[Char], String] = 
    use TextParser.satisfy;
    use TextParser/Combinators.count;
    let p1: Parser[List[Char]] = count(10, satisfy(_ -> true));
    TextParser.runs(p1, "abcdefghij")

@test
def test03(): Result[List[Char], String] = 
    use TextParser.satisfy;
    use TextParser/Combinators.count;
    let p1: Parser[List[Char]] = count(10, satisfy(_ -> true));
    TextParser.runs(p1, "abcde")