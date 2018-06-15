
# Elm Syntax Parser in Rust

This project is a port of [elm-ast](https://github.com/Bogdanp/elm-ast) to
[Rust](https://www.rust-lang.org/) using the [nom](https://github.com/Geal/nom) parsing libary.


## Status

The port is complete. Further testing is required. It will almost certainly prove fragile at the
moment.

The elm-ast library is based on 0.18 syntax though it possibly has one or two
out of date parts. More testing is required.


## Development

You can run the tests with:

```
cargo test
```

Please raise issues with any syntax that is not handled properly.


## Commands

There is an `elm-parser-dump` command that will pretty print the generated AST
for any file that you give it as an argument:

```
cargo run examples/Basic.elm
```


## Known Issues

- Only supports top level comments. Possibly requires a macro approach to
accept comments every where like in this [lua parser](https://github.com/doomrobo/nom-lua53/blob/db1d25fb0b143441c0b9c4421cbcd0db56320a11/src/utils.rs#L36).


## Future Direction

The plan is to write a basic linter using this parser. It will detect certain
issues that the compiler does not report such as:

- Unused variables
- Unused named imports


## Reference

- [elm-ast](https://github.com/Bogdanp/elm-ast)
- [elm-analyse](https://github.com/stil4m/elm-analyse)
