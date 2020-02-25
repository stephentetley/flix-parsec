@test
def fail01() : Result[Unit, String] = 
    SmallParser.run(SmallParser.fail(), "")

@test
def return01() : Result[Int32, String] =
    SmallParser.run(SmallParser.return(1), "")

@test
def bind01() : Result[Int32, String] =
    SmallParser.run(SmallParser.bind(SmallParser.return(1), v -> SmallParser.return(v)), "")

@test
def fmap01() : Result[Int32, String] =
    SmallParser.run(SmallParser.fmap(x -> x + 1, SmallParser.return(1)), "")


 @test
def throwError01() : Result[Unit, String] = 
    SmallParser.run(SmallParser.throwError("** parser error**"), "")



@test
def anyChar01() : Result[Char, String] = 
    SmallParser.run(SmallParser.anyChar(), "abc")

@test
def alt01() : Result[Char, String] = 
    let parser = SmallParser.alt(SmallParser.char('A'), SmallParser.char('a'));
    SmallParser.run(parser, "abc")

@test
def alt02(): Bool =
    let prefix: SmallParser.Parser[String] = SmallParser.string("// ");
    let p1 = SmallParser.seqRight(prefix, SmallParser.char('A'));
    let p2 = SmallParser.seqRight(prefix, SmallParser.char('B'));
    let parser = SmallParser.alt(p1, p2);
    SmallParser.run(parser, "// B") == Ok('B')

@test
def count01() : Result[List[Char], String] = 
    let parser = SmallParser.count(3, SmallParser.letter());
    SmallParser.run(parser, "ABCdefg")


@test
def many01() : Result[List[Char], String] = 
    let parser = SmallParser.many(SmallParser.letter());
    SmallParser.run(parser, "ABC") 

@test
def many101() : Result[List[Char], String] = 
    let parser = SmallParser.many1(SmallParser.letter());
    SmallParser.run(parser, "ABC") 

@test
def many102() : Result[List[Char], String] = 
    let parser = SmallParser.many1(SmallParser.letter());
    SmallParser.run(parser, "123") 


@test
def skipMany01() : Result[Char, String] = 
    let parser = 
        SmallParser.seqRight(SmallParser.skipMany(SmallParser.digit()), SmallParser.letter());
    SmallParser.run(parser, "123A") 

@test
def skipMany101() : Result[Char, String] = 
    let parser = 
        SmallParser.seqRight(SmallParser.skipMany1(SmallParser.digit()), SmallParser.letter());
    SmallParser.run(parser, "123A") 

@test
def sepBy01() : Result[List[Char], String] = 
    let parser = 
        SmallParser.sepBy(SmallParser.digit(), SmallParser.char('#'));
    SmallParser.run(parser, "1#2#3") 

@test
def sepBy101() : Result[List[Char], String] = 
    let parser = 
        SmallParser.sepBy1(SmallParser.digit(), SmallParser.char('#'));
    SmallParser.run(parser, "1#2#3") 

@test
def endBy01() : Result[List[Char], String] = 
    let parser = 
        SmallParser.endBy(SmallParser.digit(), SmallParser.char('#'));
    SmallParser.run(parser, "1#2#3#") 

@test
def endBy101() : Result[List[Char], String] = 
    let parser = 
        SmallParser.endBy1(SmallParser.digit(), SmallParser.char('#'));
    SmallParser.run(parser, "1#2#3#") 

@test
def sepEndBy01() : Result[List[Char], String] = 
    let parser = 
        SmallParser.sepEndBy(SmallParser.digit(), SmallParser.char('#'));
    SmallParser.run(parser, "1#2#3#") 

@test
def sepEndBy02() : Result[List[Char], String] = 
    let parser = 
        SmallParser.sepEndBy(SmallParser.digit(), SmallParser.char('#'));
    SmallParser.run(parser, "1#2#3") 

@test
def sepEndBy101() : Result[List[Char], String] = 
    let parser = 
        SmallParser.sepEndBy1(SmallParser.digit(), SmallParser.char('#'));
    SmallParser.run(parser, "1#2#3#") 

@test
def sepEndBy102() : Result[List[Char], String] = 
    let parser = 
        SmallParser.sepEndBy1(SmallParser.digit(), SmallParser.char('#'));
    SmallParser.run(parser, "1#2#3") 

// fails
@test
def sepEndBy103(): Result[List[Char], String] = 
    let parser = 
        SmallParser.sepEndBy1(SmallParser.digit(), SmallParser.char('#'));
    SmallParser.run(parser, "This fails...") 

@test
def choice01(): Result[Char, String] = 
    let parser = 
        SmallParser.choice([SmallParser.digit(), SmallParser.char('#')]);
    SmallParser.run(parser, "#") 

@test
def manyTill01(): Result[List[Char], String] = 
    let parser = 
        SmallParser.manyTill(SmallParser.digit(), SmallParser.char('#'));
    SmallParser.run(parser, "12345#") 

@test
def eof01(): Result[Unit, String] = 
    let parser = SmallParser.eof();
    SmallParser.run(parser, "")

// fails
@test
def notFollowedBy01(): Result[Char, String] = 
    let parser = SmallParser.seqLeft(SmallParser.digit(), SmallParser.notFollowedBy(SmallParser.char('#')));
    SmallParser.run(parser, "1#")

@test
def oneOf01(): Result[Char, String] = 
    let parser = SmallParser.oneOf(['a', 'b', 'c']);
    SmallParser.run(parser, "c")

@test
def noneOf01(): Result[Char, String] = 
    let parser = SmallParser.noneOf(['a', 'b', 'c']);
    SmallParser.run(parser, "d")


@test
def upper01(): Result[Char, String] = 
    let parser = SmallParser.upper();
    SmallParser.run(parser, "A")

@test
def alphaNum01(): Result[Char, String] = 
    let parser = SmallParser.alphaNum();
    SmallParser.run(parser, "A")

@test
def alphaNum02(): Result[Char, String] = 
    let parser = SmallParser.alphaNum();
    SmallParser.run(parser, "0")


@test
def octDigit01(): Result[List[Char], String] = 
    let parser = SmallParser.many1(SmallParser.octDigit());
    SmallParser.run(parser, "012345678")

@test
def hexDigit01(): Result[List[Char], String] = 
    let parser = SmallParser.many1(SmallParser.hexDigit());
    SmallParser.run(parser, "0123ABCDEFGH")

@test
def spaces01(): Result[Char, String] = 
    let parser = SmallParser.seqRight(SmallParser.spaces(), SmallParser.alphaNum());
    SmallParser.run(parser, "        0")

@test
def int3201(): Result[Int32, String] = 
    let parser = SmallParser.int32();
    SmallParser.run(parser, "-1")

@test
def tempReadInt3201(): Result[Int32, String] = Int32.fromString("-1")

@test
def int3202(): Result[List[Int32], String] = 
    let parser = SmallParser.sepBy(SmallParser.int32(), SmallParser.comma());
    SmallParser.run(parser, "-1,0,1000,999,3,-10,99,6")


@test
def float3201(): Result[List[Float32], String] = 
    let parser = SmallParser.sepBy(SmallParser.float32(), SmallParser.comma());
    SmallParser.run(parser, "-1.0,0.0,1000.1,999.0003,3.007,-10.9,99.67543,6.0")



@test
def bounded01(): Result[(String, String), String] = 
    let token = SmallParser.lexeme(SmallParser.string("1234567"));
    let parser = SmallParser.pipe2(
                    SmallParser.bounded(token, 10), 
                    SmallParser.restOfInput(),
                    (x,y) -> (x,y));
    SmallParser.parseString(parser, "1234567     rest")



/// temp

@test
def temp01(): Float64 = 
    Float64.fromString("2000.01") |> x -> Result.getWithDefault(x, 0.0f64)

@test
def temp02(): Float64 = 
    Float64.fromString("2000") |> x -> Result.getWithDefault(x, 0.0f64)


@test
def restOfInput01(): Bool = 
    let src = "";
    match SmallParser.parseString(SmallParser.restOfInput(), src) {
        case Ok(s) => s == src
        case Err(_) => false
    }

@test
def restOfInput02(): Bool = 
    let src = "1";
    match SmallParser.parseString(SmallParser.restOfInput(), src) {
        case Ok(s) => s == src
        case Err(_) => false
    }

@test
def restOfInput03(): Bool = 
    let src = "123";
    match SmallParser.parseString(SmallParser.restOfInput(), src) {
        case Ok(s) => s == src
        case Err(_) => false
    }

@test
def alt02()