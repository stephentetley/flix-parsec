
mod SkipManyTest {

    use GradedApplicative.{point}
    use GradedMonad.{>>=}
    use FlixParsec.{Parser1}

    def parser1(): Parser1[ef, Char] = 
        use FlixParsec.Char.{char, anyChar};
        FlixParsec.Combinators.skipManyTill(anyChar(), char('F'))   >>= _  ->
        anyChar()                                                   >>= c1 ->
        point(c1) 

    @test    
    def test01(): Unit \ IO =     
        FlixParsec.Prim.parsesString(parser1(), "ABCDEFG") |> println;
        ()

    @test
    def test02(): Unit \ IO =
        let input = String.repeat(20000, "AAAAAAAAAA") + "FGHI";
        FlixParsec.Prim.parsesString(parser1(), input) |> println;
        ()    

}
