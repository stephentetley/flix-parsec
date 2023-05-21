# FlixParsec

Parser combinator library for Flix targeting text input.

FlixParsec is implemented by moving a cursor within a constant string - it 
should not tax the garbage collector by generating intermediate input strings, 
but the original string will remain in memory until parsing has finished. 

May 2023 - Flix-Parsec rewritten use a new Graded Parser Monad that works with 
effects. The new version 
