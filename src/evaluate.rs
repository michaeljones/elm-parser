use parser::{Expr, Module, Stmt};

#[derive(Debug)]
pub enum Error {
    NoMain,
    UnsupportedOperation,
}

#[derive(Debug)]
enum Value {
    Integer(i32),
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

    let value = evaluate_expression(&main_expr);

    println!("{:?}", value);

    Ok(())
}

fn evaluate_expression<'a>(expr: &Expr<'a>) -> Result<Value, Error> {
    match expr {
        Expr::Integer(int) => Ok(Value::Integer(*int)),
        Expr::BinOp {
            operator,
            left,
            right,
        } => evaluate_binary_expression(operator, left, right),
    }
}

fn evaluate_binary_expression<'a>(
    operator: &'a str,
    left: &Expr<'a>,
    right: &Expr<'a>,
) -> Result<Value, Error> {
    let left_value = evaluate_expression(left)?;
    let right_value = evaluate_expression(right)?;

    match (operator, left_value, right_value) {
        ("+", Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l + r)),
        ("*", Value::Integer(l), Value::Integer(r)) => Ok(Value::Integer(l * r)),
        _ => Err(Error::UnsupportedOperation),
    }
}
