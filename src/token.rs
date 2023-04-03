use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Id(String),
    Num(i64),

    Let,
    If,
    Loop,
    Fn,

    Eq,
    EqEq,
    Exc,
    ExcEq,
    Le,
    LeEq,
    Gr,
    GrEq,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Semicolon,
}

pub struct TokenStream {
    chars: Peekable<Chars<'static>>,
}

impl TokenStream {
    pub fn from_string(s: String) -> Self {
        Self {
            chars: String::leak(s).chars().peekable(),
        }
    }
}

impl Iterator for TokenStream {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        Some(match self.chars.next()? {
            c if c.is_numeric() => take_num(c, &mut self.chars),
            c if is_alphanumeric_or_underscore(c) => take_id(c, &mut self.chars),
            '=' => match self.chars.next_if(|&c| c == '=') {
                Some(..) => Token::EqEq,
                None => Token::Eq,
            },
            '!' => match self.chars.next_if(|&c| c == '=') {
                Some(..) => Token::ExcEq,
                None => Token::Exc,
            },
            '<' => match self.chars.next_if(|&c| c == '=') {
                Some(..) => Token::LeEq,
                None => Token::Le,
            },
            '>' => match self.chars.next_if(|&c| c == '=') {
                Some(..) => Token::GrEq,
                None => Token::Gr,
            },
            '+' => Token::Add,
            '-' => Token::Sub,
            '*' => Token::Mul,
            '/' => Token::Div,
            '%' => Token::Mod,
            '&' => Token::And,
            '|' => Token::Or,
            '^' => Token::Xor,
            ';' => Token::Semicolon,
            ' ' => return self.next(),
            _ => unimplemented!(),
        })
    }
}

#[inline(always)]
#[must_use]
fn is_alphanumeric_or_underscore(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

#[inline(always)]
#[must_use]
fn take_id(first: char, chars: &mut Peekable<Chars<'_>>) -> Token {
    let mut id = first.to_string();
    while let Some(c) = chars.next_if(|&c| is_alphanumeric_or_underscore(c)) {
        id.push(c);
    }
    match id.as_str() {
        "let" => Token::Let,
        "if" => Token::If,
        "loop" => Token::Loop,
        "fn" => Token::Fn,
        _ => Token::Id(id),
    }
}

#[inline(always)]
#[must_use]
fn take_num(first: char, chars: &mut Peekable<Chars<'_>>) -> Token {
    let mut num = parse_digit(first);
    while let Some(c) = chars.next_if(|&c| c.is_numeric()) {
        let digit = parse_digit(c);
        num *= 10;
        num += digit;
    }
    Token::Num(num)
}

#[inline(always)]
#[must_use]
fn parse_digit(c: char) -> i64 {
    (u32::from(c) - u32::from('0')) as i64
}


