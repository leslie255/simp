use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Id(String),
    Num(i64),

    If,
    Else,
    Loop,
    Break,
    Continue,
    Fn,
    Let,
    Return,

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
    Comma,
    Dot,

    ParenOpen,
    ParenClose,
    SquareOpen,
    SquareClose,
    BraceOpen,
    BraceClose,
}

impl Default for Token {
    fn default() -> Self {
        Self::ParenClose
    }
}

impl Token {
    #[inline(always)]
    #[must_use]
    pub fn into_id(self) -> Option<String> {
        if let Self::Id(v) = self {
            Some(v)
        } else {
            None
        }
    }

    /// Returns `true` if the token is [`ParenOpen`].
    ///
    /// [`ParenOpen`]: Token::ParenOpen
    #[inline(always)]
    #[must_use]
    pub fn is_paren_open(&self) -> bool {
        matches!(self, Self::ParenOpen)
    }

    /// Returns `true` if the token is [`ParenClose`].
    ///
    /// [`ParenClose`]: Token::ParenClose
    #[inline(always)]
    #[must_use]
    pub fn is_paren_close(&self) -> bool {
        matches!(self, Self::ParenClose)
    }

    /// Returns `true` if the token is [`BraceOpen`].
    ///
    /// [`BraceOpen`]: Token::BraceOpen
    #[must_use]
    pub fn is_brace_open(&self) -> bool {
        matches!(self, Self::BraceOpen)
    }

    /// Returns `true` if the token is [`BraceClose`].
    ///
    /// [`BraceClose`]: Token::BraceClose
    #[must_use]
    pub fn is_brace_close(&self) -> bool {
        matches!(self, Self::BraceClose)
    }

    /// Returns `true` if the token is [`Eq`].
    ///
    /// [`Eq`]: Token::Eq
    #[must_use]
    pub fn is_eq(&self) -> bool {
        matches!(self, Self::Eq)
    }
}

/// Parses a new token on every call of `next`,
/// Takes ownership of the string to simplify things
#[derive(Debug, Clone)]
pub struct TokenStream {
    chars: Peekable<Chars<'static>>,
}

impl TokenStream {
    #[allow(dead_code)]
    pub fn from_string(s: String) -> Self {
        Self {
            chars: String::leak(s).chars().peekable(),
        }
    }

    #[allow(dead_code)]
    pub fn from_str(s: &'static str) -> Self {
        Self {
            chars: s.chars().peekable(),
        }
    }

    /// Parses a token forward
    ///
    /// # SAFETY
    ///
    /// The function assumes:
    /// - The first char returned from `self.chars.next()` is not whitespace
    /// - Does not immediately encounter EOF
    ///
    /// # Returns
    /// `Some(token)` if a token is successfully retrieved
    /// `None` if a comment is encountered
    #[inline(always)]
    #[must_use]
    pub unsafe fn take_token(&mut self) -> Option<Token> {
        Some(match self.chars.next().unwrap_unchecked() {
            c if c.is_ascii_digit() => take_num(c, &mut self.chars),
            c if is_alphanumeric_or_underscore(c) => take_id(c, &mut self.chars),
            '=' => match self.chars.next_if_eq(&'=') {
                Some(..) => Token::EqEq,
                None => Token::Eq,
            },
            '!' => match self.chars.next_if_eq(&'=') {
                Some(..) => Token::ExcEq,
                None => Token::Exc,
            },
            '<' => match self.chars.next_if_eq(&'=') {
                Some(..) => Token::LeEq,
                None => Token::Le,
            },
            '>' => match self.chars.next_if_eq(&'=') {
                Some(..) => Token::GrEq,
                None => Token::Gr,
            },
            '+' => Token::Add,
            '-' => Token::Sub,
            '*' => Token::Mul,
            '/' => match self.chars.peek() {
                Some('/') => {
                    self.chars.next();
                    while self.chars.next_if(|&c| c != '\n').is_some() {}
                    self.chars.next();
                    return None;
                }
                Some('*') => {
                    self.chars.next();
                    let mut depth = 1u32;
                    while depth != 0 {
                        match self.chars.next()? {
                            '*' => match self.chars.peek()? {
                                '/' => {
                                    self.chars.next();
                                    depth -= 1;
                                }
                                _ => (),
                            },
                            '/' => match self.chars.peek()? {
                                '*' => {
                                    self.chars.next();
                                    depth += 1;
                                }
                                _ => (),
                            },
                            _ => (),
                        }
                    }
                    self.chars.next();
                    return None;
                }
                _ => Token::Div,
            },
            '%' => Token::Mod,
            '&' => Token::And,
            '|' => Token::Or,
            ';' => Token::Semicolon,
            ',' => Token::Comma,
            '.' => Token::Dot,
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
        loop {
            while self.chars.peek()?.is_whitespace() {
                self.chars.next();
            }
            match unsafe { self.take_token() } {
                Some(t) => break Some(t),
                None => continue,
            }
        }
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
        "else" => Token::Else,
        "loop" => Token::Loop,
        "break" => Token::Break,
        "continue" => Token::Continue,
        "fn" => Token::Fn,
        "let" => Token::Let,
        "return" => Token::Return,
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
