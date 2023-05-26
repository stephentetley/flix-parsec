# FlixParsec

Parser combinator library for Flix targeting text input.

FlixParsec is implemented by moving a cursor within a constant string - it 
should not tax the garbage collector by generating intermediate input strings, 
but the original string will remain in memory until parsing has finished. 

26 May 2023 - Flix-Parsec has reverted to a "2-CPS" parser monad after 
experimenting with a codensity-error monad. The codensity-error monad was
failing on a sample input file of 1MB in size.
