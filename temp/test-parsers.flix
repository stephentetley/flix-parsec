@test
def fail01() : Result[Unit, String] = 
    TextParser.runs(TextParser.fail(), "")


@test
def bind01() : Result[Int32, String] =
    TextParser.runs(TextParser.bind(TextParser.return(1), v -> TextParser.return(v)), "")




 @test
def throwError01() : Result[Unit, String] = 
    TextParser.runs(TextParser.throwError("** parser error**"), "")



@test
def anyChar01() : Result[Char, String] = 
    TextParser.runs(TextParser.anyChar(), "abc")

@test
def alt01() : Result[Char, String] = 
    let parser = TextParser.alt(TextParser.char('A'), TextParser.char('a'));
    TextParser.runs(parser, "abc")

@test
def alt02(): Bool =
    let prefix: TextParser.Parser[String] = TextParser.string("// ");
    let p1 = TextParser.seqRight(prefix, TextParser.char('A'));
    let p2 = TextParser.seqRight(prefix, TextParser.char('B'));
    let parser = TextParser.alt(p1, p2);
    TextParser.runs(parser, "// B") == Ok('B')

@test
def count01() : Result[List[Char], String] = 
    let parser = TextParser.count(3, TextParser.letter());
    TextParser.runs(parser, "ABCdefg")


@test
def many01() : Result[List[Char], String] = 
    let parser = TextParser.many(TextParser.letter());
    TextParser.runs(parser, "ABC") 

@test
def many101() : Result[List[Char], String] = 
    let parser = TextParser.many1(TextParser.letter());
    TextParser.runs(parser, "ABC") 

@test
def many102() : Result[List[Char], String] = 
    let parser = TextParser.many1(TextParser.letter());
    TextParser.runs(parser, "123") 


@test
def skipMany01() : Result[Char, String] = 
    let parser = 
        TextParser.seqRight(TextParser.skipMany(TextParser.digit()), TextParser.letter());
    TextParser.runs(parser, "123A") 

@test
def skipMany101() : Result[Char, String] = 
    let parser = 
        TextParser.seqRight(TextParser.skipMany1(TextParser.digit()), TextParser.letter());
    TextParser.runs(parser, "123A") 

@test
def sepBy01() : Result[List[Char], String] = 
    let parser = 
        TextParser.sepBy(TextParser.digit(), TextParser.char('#'));
    TextParser.runs(parser, "1#2#3") 

@test
def sepBy101() : Result[List[Char], String] = 
    let parser = 
        TextParser.sepBy1(TextParser.digit(), TextParser.char('#'));
    TextParser.runs(parser, "1#2#3") 

@test
def endBy01() : Result[List[Char], String] = 
    let parser = 
        TextParser.endBy(TextParser.digit(), TextParser.char('#'));
    TextParser.runs(parser, "1#2#3#") 

@test
def endBy101() : Result[List[Char], String] = 
    let parser = 
        TextParser.endBy1(TextParser.digit(), TextParser.char('#'));
    TextParser.runs(parser, "1#2#3#") 

@test
def sepEndBy01() : Result[List[Char], String] = 
    let parser = 
        TextParser.sepEndBy(TextParser.digit(), TextParser.char('#'));
    TextParser.runs(parser, "1#2#3#") 

@test
def sepEndBy02() : Result[List[Char], String] = 
    let parser = 
        TextParser.sepEndBy(TextParser.digit(), TextParser.char('#'));
    TextParser.runs(parser, "1#2#3") 

@test
def sepEndBy101() : Result[List[Char], String] = 
    let parser = 
        TextParser.sepEndBy1(TextParser.digit(), TextParser.char('#'));
    TextParser.runs(parser, "1#2#3#") 

@test
def sepEndBy102() : Result[List[Char], String] = 
    let parser = 
        TextParser.sepEndBy1(TextParser.digit(), TextParser.char('#'));
    TextParser.runs(parser, "1#2#3") 

// fails
@test
def sepEndBy103(): Result[List[Char], String] = 
    let parser = 
        TextParser.sepEndBy1(TextParser.digit(), TextParser.char('#'));
    TextParser.runs(parser, "This fails...") 

@test
def choice01(): Result[Char, String] = 
    let parser = 
        TextParser.choice(TextParser.digit() :: TextParser.char('#') :: Nil);
    TextParser.runs(parser, "#") 

@test
def manyTill01(): Result[List[Char], String] = 
    let parser = 
        TextParser.manyTill(TextParser.digit(), TextParser.char('#'));
    TextParser.runs(parser, "12345#") 

@test
def endOfInput01(): Result[Unit, String] = 
    let parser = TextParser.endOfInput();
    TextParser.runs(parser, "")

// fails
@test
def notFollowedBy01(): Result[Char, String] = 
    let parser = TextParser.seqLeft(TextParser.digit(), TextParser.notFollowedBy(TextParser.char('#')));
    TextParser.runs(parser, "1#")

@test
def oneOf01(): Result[Char, String] = 
    let parser = TextParser.oneOf('a' :: 'b' :: 'c' :: Nil);
    TextParser.runs(parser, "c")

@test
def noneOf01(): Result[Char, String] = 
    let parser = TextParser.noneOf('a' :: 'b' :: 'c' :: Nil);
    TextParser.runs(parser, "d")


@test
def upper01(): Result[Char, String] = 
    let parser = TextParser.upper();
    TextParser.runs(parser, "A")

@test
def alphaNum01(): Result[Char, String] = 
    let parser = TextParser.alphaNum();
    TextParser.runs(parser, "A")

@test
def alphaNum02(): Result[Char, String] = 
    let parser = TextParser.alphaNum();
    TextParser.runs(parser, "0")


@test
def octDigit01(): Result[List[Char], String] = 
    let parser = TextParser.many1(TextParser.octDigit());
    TextParser.runs(parser, "012345678")

@test
def hexDigit01(): Result[List[Char], String] = 
    let parser = TextParser.many1(TextParser.hexDigit());
    TextParser.runs(parser, "0123ABCDEFGH")

@test
def spaces01(): Result[Char, String] = 
    let parser = TextParser.seqRight(TextParser.spaces(), TextParser.alphaNum());
    TextParser.runs(parser, "        0")

@test
def int3201(): Result[Int32, String] = 
    let parser = TextParser.int32();
    TextParser.runs(parser, "-1")

@test
def int3202(): Result[List[Int32], String] = 
    let parser = TextParser.sepBy(TextParser.int32(), TextParser.comma());
    TextParser.runs(parser, "-1,0,1000,999,3,-10,99,6")


@test
def float3201(): Result[List[Float32], String] = 
    let parser = TextParser.sepBy(TextParser.float32(), TextParser.comma());
    TextParser.runs(parser, "-1.0,0.0,1000.1,999.0003,3.007,-10.9,99.67543,6.0")



@test
def bounded01(): Result[(String, String), String] = 
    let token = TextParser.lexeme(TextParser.string("1234567"));
    let parser = TextParser.pipe2(
                    TextParser.bounded(token, 10), 
                    TextParser.restOfInput(),
                    (x,y) -> (x,y));
    TextParser.runs(parser, "1234567     rest")




@test
def restOfInput01(): Bool = 
    let src = "";
    match TextParser.runs(TextParser.restOfInput(), src) {
        case Ok(s) => s == src
        case Err(_) => false
    }

@test
def restOfInput02(): Bool = 
    let src = "1";
    match TextParser.runs(TextParser.restOfInput(), src) {
        case Ok(s) => s == src
        case Err(_) => false
    }

@test
def restOfInput03(): Bool = 
    let src = "123";
    match TextParser.runs(TextParser.restOfInput(), src) {
        case Ok(s) => s == src
        case Err(_) => false
    }

def makeString(xs: List[String]): String & Impure = 
    let sb = StringBuilder.new();
    List.foldLeft((_,s) -> StringBuilder.appendLine(sb, s), (), xs);
    StringBuilder.toString(sb)

@test 
def restOfLine01(): Bool & Impure = 
    let src = makeString("123    " :: "Hello" :: Nil);
    let p = TextParser.pipe2(
                TextParser.restOfLine(true),
                TextParser.symbol("Hello"),
                (_,y) -> y == "Hello"
            );
    match TextParser.runs(p, src) {
        case Ok(x) => x
        case Err(_) => false
    }

@test 
def restOfLine02(): Bool & Impure = 
    let src = makeString("123    " :: "Hello" :: Nil);
    let p = TextParser.pipe3(
                TextParser.restOfLine(false),
                TextParser.newline(),
                TextParser.symbol("Hello"),
                (_,_,z) -> z == "Hello"
            );
    match TextParser.runs(p, src) {
        case Ok(x) => x
        case Err(_) => false
    }

@test 
def numberParser01(): Float64 = 
    let src = "123.56 ";
    let SymbolParser(body) = TextParser.makeSymbolParser(TextParser.spaces());

    let myFloat64 = body.f64;
    match TextParser.runs(myFloat64, src) {
        case Ok(x) => x
        case Err(_) => 0.0f64
    }

def numParsers(): TextParser.SymbolParser = TextParser.makeSymbolParser(TextParser.spaces())
def pFloat64(): TextParser.Parser[Float64] = 
    let SymbolParser(body) = numParsers();
    body.f64


@test 
def numberParser02(): Float64 = 
    let src = "123.56 ";
    
    match TextParser.runs(pFloat64(), src) {
        case Ok(x) => x
        case Err(_) => 0.0f64
    }