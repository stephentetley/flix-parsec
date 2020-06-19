# text-parser

Parser combinator library for Flix targeting text input.

Text Parser is implemented by moving a cursor within a constant string. This means it
should be quite efficient for its use-case of parsing short strings - it should not tax
the garbage collector by generating intermediate results, but the original string will
remain in memory until parsing has finished. Work is ongoing to make `text-parser`
fast enough to parse medium sized files (e.g 1MB).

Text-Parser depends on code in `flix-sandbox` particularly Text/Regex and System.Error.
