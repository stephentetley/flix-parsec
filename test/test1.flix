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


@test
def test04(): Result[List[Char], String] = 
    use TextParser.satisfy;
    use TextParser/Combinators.many;
    let p1: Parser[List[Char]] = many(satisfy(x -> x == 'a'));
    TextParser.runs(p1, "aaaa")

@test
def test05(): Result[List[Char], String] = 
    use TextParser.satisfy;
    use TextParser/Combinators.many;
    let p1: Parser[List[Char]] = many(satisfy(x -> x == 'a'));
    TextParser.runs(p1, "aaaab")

@test
def test06(): Result[List[Char], String] = 
    use TextParser.satisfy;
    use TextParser/Combinators.many;
    let p1: Parser[List[Char]] = many(satisfy(x -> x == 'a'));
    TextParser.runs(p1, "b")

@test
def test07(): Result[List[Char], String] = 
    use TextParser.satisfy;
    use TextParser/Combinators.manyTill;
    let p1: Parser[List[Char]] = manyTill(satisfy(x -> x == 'a'), satisfy(x -> x == 'b'));
    TextParser.runs(p1, "aaab")

@test
def test08(): Result[Char, String] = 
    use TextParser.satisfy;
    use TextParser/Combinators.choice;
    let p1: Parser[Char] = choice(satisfy(x -> x == 'a') :: satisfy(x -> x == 'b') :: Nil);
    TextParser.runs(p1, "abc")