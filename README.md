# FlixParsec

Parser combinator library for Flix targeting text input.

FlixParsec is implemented by moving a cursor within a constant string - it 
should not tax the garbage collector by generating intermediate input strings, 
but the original string will remain in memory until parsing has finished. 

The parsing monad is written in __2-CPS__ (CPS with success and failure 
continuations), this appears to make it fast enough to parse medium sized 
files (e.g 1MB).

FlixParsec depends on some code in `flix-sandbox` [1] particularly Text/Regex and Chain.

[1] https://github.com/stephentetley/flix-sandbox 
