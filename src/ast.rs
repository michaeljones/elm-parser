#[derive(Debug, PartialEq)]
pub enum Expr {
    BinOp(Box<Expr>, Op, Box<Expr>),
    Float(f64),
    Int(i64),
}

#[derive(Debug, PartialEq)]
pub enum Op {
    Divide,
    Multiply,
    Add,
    Subtract,
}
