
# Elm Syntax Parser in Rust

This project intends to implement a parser for the [Elm language](http://elm-lang.org/) in
[Rust](https://www.rust-lang.org/) using the [nom](https://github.com/Geal/nom) parsing libary.


## Status

It is currently incomplete. Any help with completing it or guidance on Rust or writing parses would
be very welcome.

Still needs support for:

- Lists
- Tuples
- Strings
- Records
- Record updates
- Types declarations
- Type aliases
- Ports
- If statments
- Case statements
- Lambdas

And more, I imagine. The current code will also need to be hardened against varying whitespace,
though ideally we'd assume [elm-format](https://github.com/avh4/elm-format) formatted code.


## Reference

- [elm-ast](https://github.com/Bogdanp/elm-ast)
