
use combine::error::ParseError;
use combine::{Parser, Stream};

use elm::syntax::file::File;
use elm::parser::modules::module_definition;

pub fn file<I>() -> impl Parser<Input = I, Output = File>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    module_definition().map(|module| File { module_definition: module })
}

#[cfg(test)]
mod tests {

    use elm::parser::file::file;
    use combine::Parser;

    #[test]
    fn simple_file() {
        let result = file().parse("module Test exposing (..)");
        assert!(result.is_ok());
    }
}
