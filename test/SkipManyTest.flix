
pub mod SkipManyTest {

    use Applicative.{point}
    use Monad.{>>=}
    use FlixParsec.GenParser.{Parser1}

    def parser1(): Parser1[Char] = 
        use FlixParsec.Char.{char, anyChar};
        FlixParsec.Combinators.skipManyTill(anyChar(), char('F'))   >>= _  ->
        anyChar()                                                   >>= c1 ->
        point(c1) 

    @Test    
    def test01(): Unit \ IO =     
        FlixParsec.GenParser.parsesString(parser1(), "ABCDEFG") |> println;
        ()

    // @Test
    // def test02(): Unit \ IO =
    //     let input = String.repeat(20000, "AAAAAAAAAA") + "FGHI";
    //     FlixParsec.GenParser.parsesString(parser1(), input) |> println;
    //     ()    

}
