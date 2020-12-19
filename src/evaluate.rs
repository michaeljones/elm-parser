use parser::{Module, Stmt};

#[derive(Debug)]
pub enum Error {
    NoMain,
}

pub fn evaluate(module: &Module) -> Result<(), Error> {
    let main_expr = module
        .statements
        .iter()
        .find_map(|stmt| match stmt {
            Stmt::Function { name, expr } => {
                if name == &"main" {
                    Some(expr)
                } else {
                    None
                }
            }
        })
        .ok_or(Error::NoMain)?;

    println!("{:?}", &main_expr);

    Ok(())
}
