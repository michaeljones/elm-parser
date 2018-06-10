use ast::expression::expression;
use ast::helpers::{lo_name, spaces, spaces_and_newlines};
use ast::statement::core::Statement;
use ast::statement::type_::{type_, type_annotation, type_constructor};

use nom::types::CompleteStr;

named!(pub port_type_declaration<CompleteStr, Statement>,
  do_parse!(
    tag!("port") >>
    spaces >>
    name: lo_name >>
    spaces >>
    char!(':') >>
    spaces_and_newlines >>
    type_: type_annotation >>
    (Statement::PortTypeDeclaration(name, type_))
  )
);

named!(pub port_declaration<CompleteStr, Statement>,
  do_parse!(
    tag!("port") >>
    spaces >>
    name: lo_name >>
    args: many0!(preceded!(spaces, lo_name)) >>
    spaces >>
    char!('=') >>
    spaces_and_newlines >>
    exp: call!(expression, 0) >>
    (Statement::PortDeclaration(name, args, exp))
  )
);

#[cfg(test)]
mod tests {

    use ast::expression::Expression;
    use ast::statement::core::*;
    use ast::statement::port::*;
    use nom::types::CompleteStr;

    fn tvar(name: &str) -> Type {
        Type::TypeVariable(name.to_string())
    }

    fn tapp(a: Type, b: Type) -> Type {
        Type::TypeApplication(Box::new(a), Box::new(b))
    }

    fn tcon(name: &str, v: Vec<Type>) -> Type {
        Type::TypeConstructor(vec![name.to_string()], v)
    }

    #[test]
    fn constant() {
        assert_eq!(
            port_type_declaration(CompleteStr("port focus : String -> Cmd msg")),
            Ok((
                CompleteStr(""),
                Statement::PortTypeDeclaration(
                    "focus".to_string(),
                    tapp(tcon("String", vec![]), tcon("Cmd", vec![tvar("msg")]))
                )
            ))
        );
    }

    #[test]
    fn another_port_type_declaration() {
        assert_eq!(
            port_type_declaration(CompleteStr("port users : (User -> msg) -> Sub msg")),
            Ok((
                CompleteStr(""),
                Statement::PortTypeDeclaration(
                    "users".to_string(),
                    tapp(
                        tapp(tcon("User", vec![]), tvar("msg")),
                        tcon("Sub", vec![tvar("msg")])
                    )
                )
            ))
        );
    }

    #[test]
    fn port_definition() {
        assert_eq!(
            port_declaration(CompleteStr("port focus = Cmd.none")),
            Ok((
                CompleteStr(""),
                Statement::PortDeclaration(
                    "focus".to_string(),
                    vec![],
                    Expression::Access(
                        Box::new(Expression::Variable(vec!["Cmd".to_string()])),
                        vec!["none".to_string()],
                    )
                )
            ))
        );
    }
}
