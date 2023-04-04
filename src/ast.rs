use std::iter::Peekable;

use crate::token::{Token, TokenStream};

#[allow(dead_code)]
#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Expr {
    Id(String) = 1,
    Num(i64),
    Assign(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    NEq(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    LeEq(Box<Expr>, Box<Expr>),
    Gr(Box<Expr>, Box<Expr>),
    GrEq(Box<Expr>, Box<Expr>),
    Not(Box<Expr>),
    UnaryAdd(Box<Expr>),
    UnarySub(Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    IfElse(Box<Expr>, Vec<Expr>, Vec<Expr>),
    Loop(Box<Expr>, Vec<Expr>),
    Call(String, Vec<Expr>),
    StaticData(String),
    Tail(Box<Expr>),
}

/// Parse a block, starting at the iterator pointing at the `{` token
#[must_use]
fn parse_block(tokens: &mut Peekable<TokenStream>) -> Expr {
    let mut body = Vec::<Expr>::new();
    while tokens.peek().map_or(false, |t| !t.is_brace_close()) {
        body.push(parse_expr(15, tokens).expect("Unexpected EOF"));
    }
    tokens.next(); // the `}` token
    Expr::Block(body)
}

#[must_use]
fn parse_expr(precedence: u8, tokens: &mut Peekable<TokenStream>) -> Option<Expr> {
    macro_rules! parse_unary_rtl {
        ($precedence:expr,$expr_ty:path) => {{
            let expr = parse_expr($precedence, tokens).expect("Unexpected EOF");
            $expr_ty(Box::new(expr))
        }};
    }
    let expr = match tokens.next()? {
        Token::Id(id) => Expr::Id(id),
        Token::Num(num) => Expr::Num(num),
        Token::Exc => parse_unary_rtl!(2, Expr::Not),
        Token::Add => parse_unary_rtl!(2, Expr::UnaryAdd),
        Token::Sub => parse_unary_rtl!(2, Expr::UnarySub),
        Token::If => todo!(),
        Token::Loop => todo!(),
        Token::Fn => todo!(),
        Token::ExcEq => panic!("Unexpected `!=`"),
        Token::Eq => panic!("Unexpected `=`"),
        Token::EqEq => panic!("Unexpected `==`"),
        Token::Le => panic!("Unexpected `<`"),
        Token::LeEq => panic!("Unexpected `<=`"),
        Token::Gr => panic!("Unexpected `>`"),
        Token::GrEq => panic!("Unexpected `>=`"),
        Token::Mul => panic!("Unexpected `*`"),
        Token::Div => panic!("Unexpected `/`"),
        Token::Mod => panic!("Unexpected `%`"),
        Token::And => panic!("Unexpected `&`"),
        Token::Or => panic!("Unexpected `|`"),
        Token::Semicolon => parse_expr(15, tokens)?,
        Token::ParenOpen => parse_expr(15, tokens).expect("Unexpected EOF"),
        Token::ParenClose => todo!(),
        Token::SquareOpen => unimplemented!(),
        Token::SquareClose => unimplemented!(),
        Token::BraceOpen => parse_block(tokens),
        Token::BraceClose => panic!("Unexpected `}}`"),
    };
    macro_rules! parse_bin_op {
        ($precedence:expr, $expr_ty:path) => {
            if precedence >= $precedence {
                tokens.next();
                let rhs = parse_expr($precedence, tokens).expect("Incomplete RHS");
                $expr_ty(Box::new(expr), Box::new(rhs))
            } else {
                expr
            }
        };
    }
    let expr = match tokens.peek() {
        Some(Token::Eq) => parse_bin_op!(14, Expr::Assign),
        Some(Token::EqEq) => parse_bin_op!(7, Expr::Eq),
        Some(Token::ExcEq) => parse_bin_op!(7, Expr::NEq),
        Some(Token::Le) => parse_bin_op!(6, Expr::Le),
        Some(Token::LeEq) => parse_bin_op!(6, Expr::LeEq),
        Some(Token::Gr) => parse_bin_op!(6, Expr::Gr),
        Some(Token::GrEq) => parse_bin_op!(6, Expr::GrEq),
        Some(Token::Add) => parse_bin_op!(4, Expr::Add),
        Some(Token::Sub) => parse_bin_op!(4, Expr::Sub),
        Some(Token::Mul) => parse_bin_op!(3, Expr::Mul),
        Some(Token::Div) => parse_bin_op!(3, Expr::Div),
        Some(Token::Mod) => parse_bin_op!(3, Expr::Mod),
        Some(Token::And) => parse_bin_op!(11, Expr::And),
        Some(Token::Or) => parse_bin_op!(13, Expr::Or),
        Some(Token::BraceClose) => Expr::Tail(Box::new(expr)),
        Some(Token::ParenClose) | Some(Token::Semicolon) => {
            tokens.next();
            expr
        }
        Some(Token::Id(..))
        | Some(Token::Num(..))
        | Some(Token::If)
        | Some(Token::Loop)
        | Some(Token::Fn)
        | Some(Token::Exc)
        | Some(Token::ParenOpen)
        | Some(Token::SquareOpen)
        | Some(Token::SquareClose)
        | Some(Token::BraceOpen)
        | None => expr,
    };
    Some(expr)
}

#[must_use]
pub fn parse(mut tokens: Peekable<TokenStream>) -> Vec<Expr> {
    let mut ast = Vec::<Expr>::new();
    while let Some(expr) = parse_expr(15, &mut tokens) {
        ast.push(expr);
    }
    ast
}
