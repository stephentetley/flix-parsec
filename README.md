# FlixParsec

Parser combinator library for Flix targeting text input.

FlixParsec is implemented by moving a cursor within a constant string - it 
should not tax the garbage collector by generating intermediate input strings, 
but the original string will remain in memory until parsing has finished. 

The parsing monad is written in __2-CPS__ (CPS with success and failure 
continuations), this appears to make it fast enough to comfortably parse 
"medium sized" files (e.g 1MB).

FlixParsec depends on the Regex package `flix-regex`.

The optional module `FlixParsec/Extras` depends on code from `flix-sandbox` 
for file handling and dealing with BOMs (byte order marks).

[1] https://github.com/stephentetley/flix-regex

[2] https://github.com/stephentetley/flix-sandbox 
