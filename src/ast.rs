use std::iter::Peekable;

use crate::{
    token::{Token, TokenStream},
    ExpectTrue,
};

#[derive(Debug, Clone)]
pub struct Block {
    pub body: Vec<Expr>,
}

#[derive(Debug, Clone)]
#[repr(u8)]
pub enum Expr {
    Id(String) = 1,
    Num(i64),
    Tuple(Vec<Expr>),
    Let(Box<Expr>, Box<Expr>),
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
    Block(Block),
    Fn(String, Vec<String>, Option<Box<Expr>>),
    IfElse(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Loop(Box<Expr>),
    Break(Option<Box<Expr>>),
    Return(Option<Box<Expr>>),
    Continue,
    Call(Box<Expr>, Vec<Expr>),
    #[allow(dead_code)]
    StaticData(String),
    Tail(Box<Expr>),
}

impl Expr {
    #[must_use]
    #[inline(always)]
    pub fn into_fn(self) -> Option<(String, Vec<String>, Option<Box<Expr>>)> {
        match self {
            Self::Fn(name, args, body) => Some((name, args, body)),
            _ => None,
        }
    }

    #[allow(dead_code)]
    #[must_use]
    #[inline(always)]
    pub fn as_id(&self) -> Option<&str> {
        if let Self::Id(v) = self {
            Some(v.as_str())
        } else {
            None
        }
    }

    #[must_use]
    #[inline(always)]
    pub fn as_block(&self) -> Option<&Block> {
        if let Self::Block(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

/// Parse a block, starting at the iterator pointing at the `{` token
#[must_use]
fn parse_block(tokens: &mut Peekable<TokenStream>) -> Option<Expr> {
    let mut body = Vec::<Expr>::new();
    while !tokens.peek()?.is_brace_close() {
        let expr = parse_expr(15, tokens).expect("Unexpected EOF");
        match tokens.peek()? {
            Token::Semicolon => {
                tokens.next();
            }
            Token::BraceClose => {
                body.push(Expr::Tail(Box::new(expr)));
                break;
            }
            _ => (),
        }
        body.push(expr);
    }
    tokens.next(); // the `}` token
    Some(Expr::Block(Block { body }))
}

/// Parse a function definition, starting from the iterator pointing at the `fn` token
#[inline(always)]
#[must_use]
fn parse_fn(tokens: &mut Peekable<TokenStream>) -> Option<Expr> {
    let name = tokens
        .next()?
        .into_id()
        .expect("Expects identifier after `fn`");
    tokens
        .next()?
        .is_paren_open()
        .expect_true("Expects `(` after function name at function definition");
    let mut args = Vec::<String>::new();
    while !tokens.peek()?.is_paren_close() {
        args.push(tokens.next()?.into_id().expect("Expects argument name"));
        match tokens.peek()? {
            Token::Comma => {
                tokens.next();
            }
            Token::ParenClose => (),
            _ => panic!("Expects `,` or `)`"),
        }
    }
    tokens.next();
    let rhs = match tokens.next()? {
        Token::Semicolon => None,
        Token::Eq => Some(parse_expr(15, tokens)?),
        _ => panic!("Expects `=` or `;`"),
    };
    Some(Expr::Fn(name, args, rhs.map(Box::new)))
}

/// Parse a let expression, starting from the iterator pointing at the `let` token
#[inline(always)]
#[must_use]
fn parse_let(tokens: &mut Peekable<TokenStream>) -> Option<Expr> {
    let lhs = parse_expr(13, tokens)?;
    tokens
        .next()?
        .is_eq()
        .expect_true("Expects `=` after LHS of a `let` expression");
    let rhs = parse_expr(15, tokens)?;
    Some(Expr::Let(Box::new(lhs), Box::new(rhs)))
}

/// Parse arguments to a function call, starting from the iterator pointing at the token before `(`
#[inline(always)]
#[must_use]
fn parse_call_args(tokens: &mut Peekable<TokenStream>) -> Option<Vec<Expr>> {
    tokens.next();
    let mut args = Vec::<Expr>::new();
    while !tokens.peek()?.is_paren_close() {
        args.push(parse_expr(15, tokens)?);
        match tokens.peek()? {
            Token::Comma => {
                tokens.next();
            }
            Token::ParenClose => (),
            _ => panic!("Expects `,` or `)`"),
        }
    }
    tokens.next();
    Some(args)
}

/// Parse if statements, starting from the iterator pointing at the token `if`
#[inline(always)]
#[must_use]
fn parse_if(tokens: &mut Peekable<TokenStream>) -> Option<Expr> {
    let condition = parse_expr(15, tokens)?;
    tokens.next()?.is_brace_open().expect_true("Expect `{`");
    let if_body = parse_block(tokens)?;
    let else_body = match tokens.peek() {
        Some(Token::Else) => {
            tokens.next();
            tokens.next()?.is_brace_open().expect_true("Expect `{`");
            Some(parse_block(tokens)?)
        }
        _ => None,
    };
    Some(Expr::IfElse(
        Box::new(condition),
        Box::new(if_body),
        else_body.map(Box::new),
    ))
}

/// Parse a loop block, starting from the iterator pointing at the token `if`
#[inline(always)]
#[must_use]
fn parse_loop(tokens: &mut Peekable<TokenStream>) -> Option<Expr> {
    tokens.next()?.is_brace_open().expect_true("Expect `{`");
    let body = parse_block(tokens)?;
    Some(Expr::Loop(Box::new(body)))
}

#[inline(always)]
#[must_use]
fn parse_paren(tokens: &mut Peekable<TokenStream>) -> Option<Expr> {
    let expr = parse_expr(15, tokens)?;
    match tokens.next()? {
        Token::Comma => {
            // is a tuple
            let mut fields = vec![expr];
            while !tokens.peek()?.is_paren_close() {
                fields.push(parse_expr(15, tokens)?);
                match tokens.peek()? {
                    Token::Comma => {
                        tokens.next();
                    }
                    Token::ParenClose => (),
                    _ => panic!("Expects `)` or `,`"),
                }
            }
            tokens.next();
            Some(Expr::Tuple(fields))
        }
        Token::ParenOpen => Some(expr),
        _ => panic!("Expects `)` or `,`"),
    }
}

#[inline(always)]
fn parse_break(tokens: &mut Peekable<TokenStream>) -> Option<Expr> {
    match tokens.peek()? {
        Token::Semicolon | Token::BraceClose => Some(Expr::Break(None)),
        _ => Some(Expr::Break(Some(Box::new(parse_expr(15, tokens)?)))),
    }
}

#[inline(always)]
fn parse_return(tokens: &mut Peekable<TokenStream>) -> Option<Expr> {
    match tokens.peek()? {
        Token::Semicolon | Token::BraceClose => Some(Expr::Return(None)),
        _ => Some(Expr::Return(Some(Box::new(parse_expr(15, tokens)?)))),
    }
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
        Token::If => parse_if(tokens)?,
        Token::Else => panic!("Unexpected `else`"),
        Token::Loop => parse_loop(tokens)?,
        Token::Break => parse_break(tokens)?,
        Token::Continue => Expr::Continue,
        Token::Fn => parse_fn(tokens)?,
        Token::Let => parse_let(tokens)?,
        Token::Return => parse_return(tokens)?,
        Token::ExcEq => panic!("Unexpected `!=`"),
        Token::Eq => panic!("Unexpected `=`"),
        Token::EqEq => panic!("Unexpected `==`"),
        Token::Le => panic!("Unexpected `<`"),
        Token::LeEq => panic!("Unexpected `<=`"),
        Token::Gr => panic!("Unexpected `>`"),
        Token::GrEq => panic!("Unexpected `>=`"),
        Token::Mul => unimplemented!("Dereference expressions (`*expr`) is not supported, or maybe it was just an accidental `*`?"),
        Token::Div => panic!("Unexpected `/`"),
        Token::Mod => panic!("Unexpected `%`"),
        Token::And => unimplemented!("Address expressions (`&expr`) is not supported, or maybe it was just an accidental `&`?"),
        Token::Or => panic!("Unexpected `|`"),
        Token::Comma => panic!("Unexpected `,`"),
        Token::Dot => panic!("Unexpected `.`"),
        Token::Semicolon => parse_expr(15, tokens)?,
        Token::ParenOpen => parse_paren(tokens)?,
        Token::ParenClose => panic!("Unexpected `)`"),
        Token::SquareOpen => unimplemented!(),
        Token::SquareClose => panic!("Unexpected `]`"),
        Token::BraceOpen => parse_block(tokens)?,
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
        Some(Token::Dot) => panic!("Member accessing is not supported"),
        Some(Token::ParenOpen) => Expr::Call(Box::new(expr), parse_call_args(tokens)?),
        _ => expr,
    };
    Some(expr)
}

#[derive(Clone, Debug)]
pub struct AstParser {
    tokens: Peekable<TokenStream>,
}

impl AstParser {
    pub fn new(tokens: Peekable<TokenStream>) -> Self {
        Self { tokens }
    }
}

impl Iterator for AstParser {
    type Item = Expr;

    fn next(&mut self) -> Option<Self::Item> {
        let expr = parse_expr(15, &mut self.tokens);
        if let Some(Token::Semicolon) = self.tokens.peek() {
            self.tokens.next();
        }
        expr
    }
}
