
namespace SkipManyTest {

    use FlixParsec.Parser;

    def parser1(): Parser[ka1, Char] = 
        use FlixParsec/Prim.{flatMap, point};
        use FlixParsec/Char.{char, anyChar};
        let* _  = FlixParsec/Combinators.skipManyTill(anyChar(), char('F'));
        let* c1 = anyChar();
        point(c1) 

    @test    
    def test01(): Unit & Impure =     
        FlixParsec/Prim.parsesString(parser1(), "ABCDEFG") |> println;
        ()

    @test
    def test02(): Unit & Impure =
        let input = String.repeat(20000, "AAAAAAAAAA") + "FGHI";
        FlixParsec/Prim.parsesString(parser1(), input) |> println;
        ()    

}
