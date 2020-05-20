# text-parser

Parser combinator library for Flix targeting text input.

Text Parser is implemented by moving a cursor within a constant string. This means it
should be quite efficient for its use-case of parsing short strings - it should not tax
the garbage collector by generating intermediate results, but the original string will
remain in memory until parsing has finished. Work is ongoing to make `text-parser`
fast enough to parse medium sized files (e.g 1MB).

Text Parser is intended for parsing string data, although it shares many of the
combinators from Parsec, FParsec, etc. it is not ideal for parsing highly
structured / recursive text like program code.

Text-Parser depends on code in `flix-sandbox` particular Regex and System.Error.
