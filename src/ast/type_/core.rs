use ast::helpers::{Name, QualifiedType};

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    TypeConstructor(QualifiedType, Vec<Type>),
    TypeVariable(Name),
    TypeRecordConstructor(Box<Type>, Vec<(Name, Type)>),
    TypeRecord(Vec<(Name, Type)>),
    TypeTuple(Vec<Type>),
    TypeApplication(Box<Type>, Box<Type>),
}
