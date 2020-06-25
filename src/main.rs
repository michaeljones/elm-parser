#[macro_use]
extern crate lalrpop_util;

mod ast;

lalrpop_mod!(pub elm); // synthesized by LALRPOP

#[test]
fn int_parser_test() {
    let code = "2";
    assert_eq!(elm::IntParser::new().parse(code), Ok(2));
}

#[test]
fn float_parser_test() {
    let code = "2.3";
    assert_eq!(elm::FloatParser::new().parse(code), Ok(2.3));
}

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
