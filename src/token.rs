use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Id(String),
    Num(i64),

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
    Semicolon,

    ParenOpen,
    ParenClose,
    SquareOpen,
    SquareClose,
    BraceOpen,
    BraceClose,
}

impl Token {
    /// Returns `true` if the token is [`BraceClose`].
    ///
    /// [`BraceClose`]: Token::BraceClose
    #[inline(always)]
    #[must_use]
    pub fn is_brace_close(&self) -> bool {
        matches!(self, Self::BraceClose)
    }
}

/// Parses a new token on every call of `next`,
/// Takes ownership of the string to simplify things
#[derive(Debug, Clone)]
pub struct TokenStream {
    chars: Peekable<Chars<'static>>,
}

impl TokenStream {
    pub fn from_string(s: String) -> Self {
        Self {
            chars: String::leak(s).chars().peekable(),
        }
    }

    /// Parses a token forward
    ///
    /// # SAFETY
    ///
    /// The function assumes:
    /// - The first char returned from `self.chars.next()` is not whitespace
    /// - Does not immediately encounter EOF
    #[inline(always)]
    #[must_use]
    pub unsafe fn take_token(&mut self) -> Option<Token> {
        Some(match self.chars.next().unwrap_unchecked() {
            c if c.is_ascii_digit() => take_num(c, &mut self.chars),
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
            ';' => Token::Semicolon,
            '(' => Token::ParenOpen,
            ')' => Token::ParenClose,
            '[' => Token::SquareOpen,
            ']' => Token::SquareClose,
            '{' => Token::BraceOpen,
            '}' => Token::BraceClose,
            _ => unimplemented!(),
        })
    }
}

impl Iterator for TokenStream {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while self.chars.peek()?.is_whitespace() {
            self.chars.next();
        }
        unsafe { self.take_token() }
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
    while let Some(&c) = chars.peek() {
        let digit = parse_digit(c);
        if digit > 9 || digit < 0 {
            break;
        }
        chars.next();
        num *= 10;
        num += digit;
    }
    Token::Num(num)
}

/// Parse a single decimal digit from a character to a `i64` value
/// Returns number greater than `9` or lower than `0` if the character is not an ASCII numeric digit
#[inline(always)]
#[must_use]
fn parse_digit(c: char) -> i64 {
    u32::from(c).wrapping_sub(u32::from('0')) as i64
}
