
mod SkipManyTest {

    use FlixParsec.Parser
    use GradedMonad.{>>=}

    def parser1(): Parser[ka1, Char] = 
        use FlixParsec.Char.{char, anyChar};
        FlixParsec.Combinators.skipManyTill(anyChar(), char('F'))   >>= _  ->
        anyChar()                                                   >>= c1 ->
        point(c1) 

    @test    
    def test01(): Unit & Impure =     
        FlixParsec.Prim.parsesString(parser1(), "ABCDEFG") |> println;
        ()

    @test
    def test02(): Unit & Impure =
        let input = String.repeat(20000, "AAAAAAAAAA") + "FGHI";
        FlixParsec.Prim.parsesString(parser1(), input) |> println;
        ()    

}
