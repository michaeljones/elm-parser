#[macro_use]
extern crate lalrpop_util;

mod ast;

lalrpop_mod!(pub elm); // synthesized by LALRPOP

// List Constructor

#[test]
fn list_constructor_parser_test() {
    let code = "[1,2,3]";
    assert_eq!(
        elm::ExprParser::new().parse(code),
        Ok(ast::Expr::List(vec![
            ast::Expr::Int(1),
            ast::Expr::Int(2),
            ast::Expr::Int(3)
        ]))
    );
}

// Type Constructor

#[test]
fn type_constructor_parser_test() {
    let code = "Abc";
    assert_eq!(
        elm::ExprParser::new().parse(code),
        Ok(ast::Expr::TypeConstructor("Abc".to_string()))
    );
}

// Variables

#[test]
fn variable_parser_test() {
    let code = "abc";
    assert_eq!(
        elm::ExprParser::new().parse(code),
        Ok(ast::Expr::Variable("abc".to_string()))
    );
}

#[test]
fn variable_in_expression_parser_test() {
    let code = "abc + 3";
    assert_eq!(
        elm::ExprParser::new().parse(code),
        Ok(ast::Expr::BinOp(
            Box::new(ast::Expr::Variable("abc".to_string())),
            ast::Op::Add,
            Box::new(ast::Expr::Int(3))
        ))
    );
}

// Char

#[test]
fn char_parser_test() {
    let code = "'a'";
    assert_eq!(
        elm::ExprParser::new().parse(code),
        Ok(ast::Expr::Char("a".to_string()))
    );
}

// Numbers

#[test]
fn int_parser_test() {
    let code = "2";
    assert_eq!(elm::ExprParser::new().parse(code), Ok(ast::Expr::Int(2)));
}

#[test]
fn float_parser_test() {
    let code = "2.3";
    assert_eq!(
        elm::ExprParser::new().parse(code),
        Ok(ast::Expr::Float(2.3))
    );
}

// Math Operations

#[test]
fn add_parser_test() {
    let code = "2 + 3";
    assert_eq!(
        elm::ExprParser::new().parse(code),
        Ok(ast::Expr::BinOp(
            Box::new(ast::Expr::Int(2)),
            ast::Op::Add,
            Box::new(ast::Expr::Int(3))
        ))
    );
}

#[test]
fn subtract_parser_test() {
    let code = "2 - 3";
    assert_eq!(
        elm::ExprParser::new().parse(code),
        Ok(ast::Expr::BinOp(
            Box::new(ast::Expr::Int(2)),
            ast::Op::Subtract,
            Box::new(ast::Expr::Int(3))
        ))
    );
}

#[test]
fn multiply_parser_test() {
    let code = "2 * 3";
    assert_eq!(
        elm::ExprParser::new().parse(code),
        Ok(ast::Expr::BinOp(
            Box::new(ast::Expr::Int(2)),
            ast::Op::Multiply,
            Box::new(ast::Expr::Int(3))
        ))
    );
}

#[test]
fn divide_parser_test() {
    let code = "2 // 3";
    assert_eq!(
        elm::ExprParser::new().parse(code),
        Ok(ast::Expr::BinOp(
            Box::new(ast::Expr::Int(2)),
            ast::Op::Divide,
            Box::new(ast::Expr::Int(3))
        ))
    );
}

#[test]
fn math_order_parser_test() {
    let code = "1 - 4 + 2 // 3 * 5";
    assert_eq!(
        elm::ExprParser::new().parse(code),
        Ok(ast::Expr::BinOp(
            Box::new(ast::Expr::Int(1)),
            ast::Op::Subtract,
            Box::new(ast::Expr::BinOp(
                Box::new(ast::Expr::Int(4)),
                ast::Op::Add,
                Box::new(ast::Expr::BinOp(
                    Box::new(ast::Expr::BinOp(
                        Box::new(ast::Expr::Int(2)),
                        ast::Op::Divide,
                        Box::new(ast::Expr::Int(3))
                    )),
                    ast::Op::Multiply,
                    Box::new(ast::Expr::Int(5))
                ))
            ))
        ))
    );
}

#[test]
fn paren_order_parser_test() {
    let code = "(4 + 2) // 3";
    assert_eq!(
        elm::ExprParser::new().parse(code),
        Ok(ast::Expr::BinOp(
            Box::new(ast::Expr::BinOp(
                Box::new(ast::Expr::Int(4)),
                ast::Op::Add,
                Box::new(ast::Expr::Int(2))
            )),
            ast::Op::Divide,
            Box::new(ast::Expr::Int(3))
        ))
    );
}

fn main() {}
