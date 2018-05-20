use ast::helpers::{Alias, ModuleName, Name, QualifiedType};
use ast::expression::Expression;
use ast::binop::Assoc;

#[derive(Debug, PartialEq, Clone)]
pub enum ExportSet {
    AllExport(Vec<Expression>),
    SubsetExport(Vec<ExportSet>),
    FunctionExport(Name),
    TypeExport(Name, Option<Box<ExportSet>>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    TypeConstructor(QualifiedType, Vec<Type>),
    TypeVariable(Name),
    TypeRecordConstructor(Box<Type>, Vec<(Name, Type)>),
    TypeRecord(Vec<(Name, Box<Type>)>),
    TypeTuple(Vec<Type>),
    TypeApplication(Box<Type>, Box<Type>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    ModuleDeclaration(ModuleName, ExportSet),
    PortModuleDeclaration(ModuleName, ExportSet),
    EffectModuleDeclaration(ModuleName, Vec<(Name, Name)>, ExportSet),
    ImportStatement(ModuleName, Option<Alias>, Option<ExportSet>),
    TypeAliasDeclaration(Type, Type),
    TypeDeclaration(Type, Vec<Type>),
    PortTypeDeclaration(Name, Type),
    PortDeclaration(Name, Vec<Name>, Expression),
    FunctionTypeDeclaration(Name, Type),
    FunctionDeclaration(Name, Vec<Expression>, Expression),
    InfixDeclaratiok(Assoc, i64, Name),
    Comment(String),
}
