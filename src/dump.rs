#[macro_use]
extern crate combine;

mod elm;

use combine::stream::easy;
use combine::stream::position::SourcePosition;
use combine::{stream::position, *};

#[derive(Debug)]
enum Error<E> {
    Io(std::io::Error),
    Parse(E),
}

fn main() {
    let result = match std::env::args().nth(1) {
        Some(file) => std::fs::File::open(file)
            .map_err(Error::Io)
            .and_then(read_contents),
        None => read_contents(std::io::stdin()),
    };

    println!("{:?}", result);
}

fn read_contents<R>(mut read: R) -> Result<(), Error<easy::Errors<char, String, SourcePosition>>>
where
    R: std::io::Read,
{
    let mut text = String::new();
    read.read_to_string(&mut text).map_err(Error::Io)?;
    let result = elm::parser::file::file()
        .easy_parse(position::Stream::new(&*text))
        .map_err(|err| Error::Parse(err.map_range(|s| s.to_string())))?;

    println!("{:?}", result);

    Ok(())
}
