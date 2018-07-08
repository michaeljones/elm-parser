use ast::binop::Assoc;
use ast::expression::core::Function;
use ast::expression::Expression;
use ast::helpers::{Alias, ModuleName, Name};
use ast::type_::core::Type;

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
    Function(Function),
    InfixDeclaration(Assoc, i64, Name),
    Comment(String),
}
