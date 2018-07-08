use ast::helpers::Name;
use ast::type_::core::Type;

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionSignature {
    pub name: Name,
    pub type_: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefinition {
    pub name: Name,
    pub args: Vec<Expression>,
    pub body: Expression,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub signature: Option<FunctionSignature>,
    pub definition: FunctionDefinition,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LetEntry {
    LetFunction(Function),
    LetBinding(Expression, Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Character(char),
    String(String),
    Integer(String),
    Float(String),
    Variable(Vec<Name>),
    List(Vec<Expression>),
    Tuple(Vec<Expression>),
    Access(Box<Expression>, Vec<Name>),
    AccessFunction(Name),
    Record(Vec<(Name, Expression)>),
    RecordUpdate(Name, Vec<(Name, Expression)>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    Let(Vec<LetEntry>, Box<Expression>),
    Case(Box<Expression>, Vec<(Expression, Expression)>),
    Lambda(Vec<Expression>, Box<Expression>),
    Application(Box<Expression>, Box<Expression>),
    BinOp(Box<Expression>, Box<Expression>, Box<Expression>),
}
