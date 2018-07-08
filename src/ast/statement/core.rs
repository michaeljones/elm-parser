use ast::binop::Assoc;
use ast::expression::Expression;
use ast::type_::core::Type;
use ast::helpers::{Alias, ModuleName, Name};

#[derive(Debug, PartialEq, Clone)]
pub enum ExportSet {
    AllExport,
    SubsetExport(Vec<ExportSet>),
    FunctionExport(Name),
    TypeExport(Name, Option<Box<ExportSet>>),
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
    InfixDeclaration(Assoc, i64, Name),
    Comment(String),
}
