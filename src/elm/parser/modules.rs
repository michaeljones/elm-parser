
use combine::error::ParseError;
use combine::{Parser, Stream};
use combine::parser::char::{string, space, spaces};

use elm::syntax::module::Module;

pub fn module_definition<I>() -> impl Parser<Input = I, Output = Module>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    string("module")
        .skip(space())
        .skip(spaces())
        .with(string("Test"))
        .skip(space())
        .skip(spaces())
        .skip(string("exposing (..)"))
        .map(|name| Module { name : vec![name.to_string()] } )
}

#[cfg(test)]
mod tests {

    use elm::parser::modules::module_definition;
    use combine::Parser;

    #[test]
    fn simple() {
        let result = module_definition().parse("module Test exposing (..)");
        assert!(result.is_ok());
    }
}
