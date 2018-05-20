use ast::helpers::Name;

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Character(char),
    String(String),
    Integer(i64),
    Float(f64),
    Variable(Vec<Name>),
    List(Vec<Expression>),
    Tuple(Vec<Expression>),
    Access(Box<Expression>, Vec<Name>),
    AccessFunction(Name),
    Record(Vec<(Name, Expression)>),
    RecordUpdate(Name, Vec<(Name, Expression)>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Let(Vec<(Expression, Expression)>, Box<Expression>),
    Case(Box<Expression>, Vec<(Expression, Expression)>),
    Lambda(Vec<Expression>, Box<Expression>),
    Application(Box<Expression>, Box<Expression>),
    BinOp(Box<Expression>, Box<Expression>, Box<Expression>),
}
