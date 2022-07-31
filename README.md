# FlixParsec

Parser combinator library for Flix targeting text input.

FlixParsec is implemented by moving a cursor within a constant string - it 
should not tax the garbage collector by generating intermediate input strings, 
but the original string will remain in memory until parsing has finished. 

The parsing monad is written in __2-CPS__ (CPS with success and failure 
continuations), this appears to make it fast enough to comfortably parse 
"medium sized" files (e.g 1MB).


Dependencies: 

https://github.com/stephentetley/flix-regex

https://github.com/stephentetley/flix-sandbox

https://github.com/stephentetley/interop-filepath

https://github.com/stephentetley/interop-charset-locale

https://github.com/stephentetley/interop-collections

Apache Commons IO

Compatible *.pkg and *.jar files are included in the folder `lib`.
