//! Tokenizer for the Wabbit language.
//!
//! The tokenizer can be tested using the following commands:
//! ```shell
//! cargo run --bin wabbit -- tok -c '1 + 2'
//! cargo run --bin wabbit -- tok input.wb
//! ```

use crate::errors::{SyntaxError, TokenizerError};
use crate::input::{ErrorContext, Input};
use crate::location::{Loc, Span};
use crate::model::{BinOpKind, CompOpKind, UnaryOpKind};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Name(String),
    Int(i32),
    Float(f64),
    Char(char),
    Bool(bool),

    // misc
    Semi,
    Comma,
    Assign,
    LParen,
    RParen,
    LBrace,
    RBrace,

    // operators
    Not,
    Plus,
    Minus,
    Star,
    Slash,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,

    Equal,
    NotEqual,
    And,
    Or,

    // keywords
    Var,
    Const,
    Print,
    Break,
    Continue,
    If,
    Else,
    While,
    Func,
    Return,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;
        match self.kind {
            Name(ref s) => write!(f, "'{}'", s),
            Int(i) => write!(f, "'{}'", i),
            Float(fl) => write!(f, "'{:?}'", fl),
            Bool(b) => write!(f, "'{}'", b),
            Char(c) => write!(f, "'{}'", c),
            Semi => write!(f, "';'"),
            Comma => write!(f, "','"),
            Assign => write!(f, "'='"),
            LParen => write!(f, "'('"),
            RParen => write!(f, "')'"),
            LBrace => write!(f, "'{{'"),
            RBrace => write!(f, "'}}'"),
            Not => write!(f, "'!'"),
            Plus => write!(f, "'+'"),
            Minus => write!(f, "'-'"),
            Star => write!(f, "'*'"),
            Slash => write!(f, "'/'"),
            Less => write!(f, "'<'"),
            LessEqual => write!(f, "'<='"),
            Greater => write!(f, "'>'"),
            GreaterEqual => write!(f, "'>='"),
            Equal => write!(f, "'=='"),
            NotEqual => write!(f, "'!='"),
            And => write!(f, "'&&'"),
            Or => write!(f, "'||'"),
            Var => write!(f, "'var'"),
            Const => write!(f, "'const'"),
            Print => write!(f, "'print'"),
            Break => write!(f, "'break'"),
            Continue => write!(f, "'continue'"),
            If => write!(f, "'if'"),
            Else => write!(f, "'else'"),
            While => write!(f, "'while'"),
            Func => write!(f, "'func'"),
            Return => write!(f, "'return'"),
        }
    }
}

impl From<TokenKind> for BinOpKind {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Plus => Self::Add,
            TokenKind::Minus => Self::Sub,
            TokenKind::Star => Self::Mul,
            TokenKind::Slash => Self::Div,
            TokenKind::And => Self::And,
            TokenKind::Or => Self::Or,
            _ => panic!("Invalid token kind: {:?}", value),
        }
    }
}

impl From<TokenKind> for UnaryOpKind {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Plus => Self::Pos,
            TokenKind::Minus => Self::Neg,
            TokenKind::Not => Self::Not,
            _ => panic!("Invalid token kind: {:?}", value),
        }
    }
}

impl From<TokenKind> for CompOpKind {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Less => Self::Lt,
            TokenKind::LessEqual => Self::Le,
            TokenKind::Greater => Self::Gt,
            TokenKind::GreaterEqual => Self::Ge,
            TokenKind::Equal => Self::Eq,
            TokenKind::NotEqual => Self::Ne,
            _ => panic!("Invalid token kind: {:?}", value),
        }
    }
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    input: &'a Input<'a>,

    /// current position in the input, updated by [`next()`]
    pos: usize,

    /// current location in the input, updated by [`next()`]
    loc: Loc,

    /// starting location of the current token, updated by the lexer loop
    start_loc: Loc,

    /// current stream of token
    tokens: Vec<Token>,
}

pub type Result<T> = std::result::Result<T, TokenizerError>;

impl<'a> Tokenizer<'a> {
    /// Tokenize an input string
    pub fn tokenize(input: &'a Input<'a>) -> Result<Vec<Token>> {
        let mut lexer = Self::new(input);
        lexer.run()?;
        Ok(lexer.tokens)
    }

    fn new(input: &'a Input<'a>) -> Self {
        Self {
            input,
            pos: 0,
            loc: Loc::default(),
            start_loc: Loc::default(),
            tokens: Vec::new(),
        }
    }

    /// Push a token into the token stream.
    fn push(&mut self, kind: TokenKind) {
        self.tokens.push(Token {
            kind,
            span: Span::new(self.start_loc, self.loc),
        });
    }

    /// Return the next character in the input stream and update the current location.
    ///
    /// Returns `None` if the end of the input is reached.
    fn next(&mut self) -> Option<char> {
        let c = self.input.source.chars().nth(self.pos);
        if let Some(c) = c {
            self.pos += 1;
            if c == '\n' {
                self.loc.line += 1;
                self.loc.col = 0;
            } else {
                self.loc.col += 1;
            }
        }

        c
    }

    /// Return the next character in the input stream without updating the current location.
    ///
    /// Returns `None` if the end of the input is reached.
    fn peek(&self) -> Option<char> {
        self.input.source.chars().nth(self.pos)
    }

    /// Return the next character in the input stream if it matches `c` and update the current
    /// location.
    fn accept(&mut self, c: char) -> bool {
        if self.peek() == Some(c) {
            self.next();
            true
        } else {
            false
        }
    }

    /// Build an [`TokenizerError`] from a [`SyntaxError`] and a [`Span`] and return it as a
    /// [`Result`].
    ///
    /// This function is intended as a shorthand for returning an error that will be displayed with
    /// suitable context of the user.
    fn err<T>(&self, err: SyntaxError) -> std::result::Result<T, TokenizerError> {
        let err = TokenizerError::SyntaxError(
            err,
            ErrorContext::new(self.input, Span::new(self.loc, self.loc)),
        );

        Err(err)
    }

    /// Run the tokenizer on the input stream.
    fn run(&mut self) -> Result<()> {
        while let Some(c) = self.next() {
            self.start_loc = self.loc;

            match c {
                // whitespace
                c if c.is_whitespace() => continue,
                // integer/float
                c if c.is_ascii_digit() => {
                    let mut num = c.to_string();
                    while let Some(c) = self.peek() {
                        if c.is_ascii_digit() {
                            num.push(c);
                            self.next();
                        } else {
                            break;
                        }
                    }
                    if let Some(c) = self.peek() {
                        if c == '.' {
                            num.push(c);
                            self.next();
                            while let Some(c) = self.peek() {
                                if c.is_ascii_digit() {
                                    num.push(c);
                                    self.next();
                                } else {
                                    break;
                                }
                            }
                            self.push(TokenKind::Float(num.parse().unwrap()));
                        } else {
                            self.push(TokenKind::Int(num.parse().unwrap()));
                        }
                    } else {
                        self.push(TokenKind::Int(num.parse().unwrap()));
                    }
                }
                // character literal
                '\'' => {
                    let character = match self.next() {
                        Some('\\') => match self.next() {
                            Some('n') => '\n',
                            Some('t') => '\t',
                            Some('r') => '\r',
                            Some('\\') => '\\',
                            Some('\'') => '\'',
                            Some(c) => {
                                return self.err(SyntaxError::UnexpectedCharacter(c));
                            }
                            None => return self.err(SyntaxError::UnexpectedEndOfFile),
                        },
                        Some(c) if c != '\'' => c,
                        Some(c) => {
                            return self.err(SyntaxError::UnexpectedCharacter(c));
                        }
                        None => {
                            return self.err(SyntaxError::UnexpectedEndOfFile);
                        }
                    };

                    // closing quote
                    match self.next() {
                        Some('\'') => (),
                        Some(c) => {
                            return self.err(SyntaxError::UnexpectedCharacter(c));
                        }
                        None => {
                            return self.err(SyntaxError::UnexpectedEndOfFile);
                        }
                    }
                    self.push(TokenKind::Char(character));
                }
                // names/keywords
                c if c.is_ascii_alphabetic() || c == '_' => {
                    let mut name = c.to_string();
                    while let Some(c) = self.peek() {
                        if c.is_ascii_alphanumeric() || c == '_' {
                            name.push(c);
                            self.next();
                        } else {
                            break;
                        }
                    }
                    match name.as_str() {
                        // keywords
                        "var" => self.push(TokenKind::Var),
                        "const" => self.push(TokenKind::Const),
                        "print" => self.push(TokenKind::Print),
                        "break" => self.push(TokenKind::Break),
                        "continue" => self.push(TokenKind::Continue),
                        "if" => self.push(TokenKind::If),
                        "else" => self.push(TokenKind::Else),
                        "while" => self.push(TokenKind::While),
                        "func" => self.push(TokenKind::Func),
                        "return" => self.push(TokenKind::Return),
                        "true" => self.push(TokenKind::Bool(true)),
                        "false" => self.push(TokenKind::Bool(false)),
                        _ => self.push(TokenKind::Name(name)),
                    }
                }
                // misc
                ';' => self.push(TokenKind::Semi),
                ',' => self.push(TokenKind::Comma),
                '(' => self.push(TokenKind::LParen),
                ')' => self.push(TokenKind::RParen),
                '{' => self.push(TokenKind::LBrace),
                '}' => self.push(TokenKind::RBrace),
                '=' => {
                    if self.accept('=') {
                        self.push(TokenKind::Equal);
                    } else {
                        self.push(TokenKind::Assign);
                    }
                }
                '!' => {
                    if self.accept('=') {
                        self.push(TokenKind::NotEqual);
                    } else {
                        self.push(TokenKind::Not);
                    }
                }
                '+' => self.push(TokenKind::Plus),
                '-' => self.push(TokenKind::Minus),
                '*' => self.push(TokenKind::Star),
                '/' => {
                    if self.accept('/') {
                        while let Some(c) = self.next() {
                            if c == '\n' {
                                break;
                            }
                        }
                    } else if self.accept('*') {
                        while let Some(c) = self.next() {
                            if c == '*' && self.peek() == Some('/') {
                                self.next();
                                break;
                            }
                        }
                    } else {
                        self.push(TokenKind::Slash);
                    }
                }
                '<' => {
                    if self.accept('=') {
                        self.push(TokenKind::LessEqual);
                    } else {
                        self.push(TokenKind::Less);
                    }
                }
                '>' => {
                    if self.accept('=') {
                        self.push(TokenKind::GreaterEqual);
                    } else {
                        self.push(TokenKind::Greater);
                    }
                }
                '&' => {
                    if self.accept('&') {
                        self.push(TokenKind::And);
                    } else {
                        return self.err(SyntaxError::UnexpectedCharacter(c));
                    }
                }
                '|' => {
                    if self.accept('|') {
                        self.push(TokenKind::Or);
                    } else {
                        return self.err(SyntaxError::UnexpectedCharacter(c));
                    }
                }

                c => return self.err(SyntaxError::UnexpectedCharacter(c)),
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn playground() {
        let input =
            Input::new("var test int = 3.2; // yo\nvar test2 int /* random comment */ = 3.2;");
        let tokens = Tokenizer::tokenize(&input).unwrap();

        for token in &tokens {
            println!("{}", token);
        }

        println!();

        for token in &tokens {
            println!("{}", ErrorContext::new(&input, token.span));
        }
    }

    #[test]
    fn test_error_context() {
        let input =
            Input::new("var test int = 3.2; // yo\nvar test2 int /* random comment */ = 3.2;");
        let tokens = Tokenizer::tokenize(&input).unwrap();
        let expected = "\n\n   2 | var test2 int /* random comment */ = 3.2;\n     |                                      ^^^\n\n";

        assert_eq!(
            format!("{}", ErrorContext::new(&input, tokens[10].span)),
            expected
        );
    }

    #[test]
    fn test_error_context_multiline() {
        let input = Input::new("hello world\nhello world\nhello world");
        let expected = "\n\n   1 | hello world
     |       ^^^^^
   2 | hello world
     | ^^^^^^^^^^^
   3 | hello world
     | ^^^^^
\n";
        assert_eq!(
            format!(
                "{}",
                ErrorContext::new(&input, Span::new(Loc::new(1, 7), Loc::new(3, 5)))
            ),
            expected
        );
    }

    fn assert_syntax_error(ierr: TokenizerError, err: SyntaxError) {
        assert!(matches!(ierr, TokenizerError::SyntaxError(_, _)));

        #[allow(irrefutable_let_patterns)]
        if let TokenizerError::SyntaxError(e, _) = ierr {
            assert_eq!(e, err);
        }
    }

    #[test]
    fn test_tokenize_fail() {
        fn tok(input: &str) -> TokenizerError {
            Tokenizer::tokenize(&Input::new(input)).unwrap_err()
        }

        assert_syntax_error(tok("1.2.3"), SyntaxError::UnexpectedCharacter('.'));
        assert_syntax_error(tok("'a"), SyntaxError::UnexpectedEndOfFile);
        assert_syntax_error(tok("|"), SyntaxError::UnexpectedCharacter('|'));
        assert_syntax_error(tok("&"), SyntaxError::UnexpectedCharacter('&'));
        //TODO: unexpected EOF with non-closing comment
    }

    #[test]
    fn test_tokenize_single_line_success() {
        fn tok(input: &str) -> Vec<Token> {
            Tokenizer::tokenize(&Input::new(input)).unwrap()
        }

        fn token(kind: TokenKind, start: usize, len: usize) -> Token {
            Token {
                kind,
                span: Span::new(Loc::new(1, start), Loc::new(1, start + len - 1)),
            }
        }

        assert_eq!(
            tok("1 2 123 1.3 1.0 1. true false 'c' 'z' '\\n' '\\t' '\\r' '\\'' '\\\\'"),
            vec![
                token(TokenKind::Int(1), 1, 1),
                token(TokenKind::Int(2), 3, 1),
                token(TokenKind::Int(123), 5, 3),
                token(TokenKind::Float(1.3), 9, 3),
                token(TokenKind::Float(1.0), 13, 3),
                token(TokenKind::Float(1.0), 17, 2),
                token(TokenKind::Bool(true), 20, 4),
                token(TokenKind::Bool(false), 25, 5),
                token(TokenKind::Char('c'), 31, 3),
                token(TokenKind::Char('z'), 35, 3),
                token(TokenKind::Char('\n'), 39, 4),
                token(TokenKind::Char('\t'), 44, 4),
                token(TokenKind::Char('\r'), 49, 4),
                token(TokenKind::Char('\''), 54, 4),
                token(TokenKind::Char('\\'), 59, 4),
            ]
        );
        //TODO: .234

        assert_eq!(
            tok("; , = ( ) { }"),
            [
                token(TokenKind::Semi, 1, 1),
                token(TokenKind::Comma, 3, 1),
                token(TokenKind::Assign, 5, 1),
                token(TokenKind::LParen, 7, 1),
                token(TokenKind::RParen, 9, 1),
                token(TokenKind::LBrace, 11, 1),
                token(TokenKind::RBrace, 13, 1),
            ]
        );

        assert_eq!(
            tok("! + - * / < <= > >= == != && ||"),
            [
                token(TokenKind::Not, 1, 1),
                token(TokenKind::Plus, 3, 1),
                token(TokenKind::Minus, 5, 1),
                token(TokenKind::Star, 7, 1),
                token(TokenKind::Slash, 9, 1),
                token(TokenKind::Less, 11, 1),
                token(TokenKind::LessEqual, 13, 2),
                token(TokenKind::Greater, 16, 1),
                token(TokenKind::GreaterEqual, 18, 2),
                token(TokenKind::Equal, 21, 2),
                token(TokenKind::NotEqual, 24, 2),
                token(TokenKind::And, 27, 2),
                token(TokenKind::Or, 30, 2),
            ]
        );

        assert_eq!(
            tok("var const print break continue if else while func return true false"),
            [
                token(TokenKind::Var, 1, 3),
                token(TokenKind::Const, 5, 5),
                token(TokenKind::Print, 11, 5),
                token(TokenKind::Break, 17, 5),
                token(TokenKind::Continue, 23, 8),
                token(TokenKind::If, 32, 2),
                token(TokenKind::Else, 35, 4),
                token(TokenKind::While, 40, 5),
                token(TokenKind::Func, 46, 4),
                token(TokenKind::Return, 51, 6),
                token(TokenKind::Bool(true), 58, 4),
                token(TokenKind::Bool(false), 63, 5),
            ]
        );

        assert_eq!(
            tok("var my_name int = /* 1 + */ true;"),
            [
                token(TokenKind::Var, 1, 3),
                token(TokenKind::Name("my_name".to_string()), 5, 7),
                token(TokenKind::Name("int".to_string()), 13, 3),
                token(TokenKind::Assign, 17, 1),
                token(TokenKind::Bool(true), 29, 4),
                token(TokenKind::Semi, 33, 1),
            ]
        );

        assert_eq!(
            tok("var    my_name  int\t=  \t  true;"),
            [
                token(TokenKind::Var, 1, 3),
                token(TokenKind::Name("my_name".to_string()), 8, 7),
                token(TokenKind::Name("int".to_string()), 17, 3),
                token(TokenKind::Assign, 21, 1),
                token(TokenKind::Bool(true), 27, 4),
                token(TokenKind::Semi, 31, 1),
            ]
        );
    }

    #[test]
    fn test_tokenize_multi_line_success() {
        fn tok(input: &str) -> Vec<Token> {
            Tokenizer::tokenize(&Input::new(input)).unwrap()
        }

        fn token(kind: TokenKind, line: usize, start: usize, len: usize) -> Token {
            Token {
                kind,
                span: Span::new(Loc::new(line, start), Loc::new(line, start + len - 1)),
            }
        }

        assert_eq!(
            tok("var name float = 1.0; // this is garbage\nconst c = 10;"),
            [
                token(TokenKind::Var, 1, 1, 3),
                token(TokenKind::Name("name".to_string()), 1, 5, 4),
                token(TokenKind::Name("float".to_string()), 1, 10, 5),
                token(TokenKind::Assign, 1, 16, 1),
                token(TokenKind::Float(1.0), 1, 18, 3),
                token(TokenKind::Semi, 1, 21, 1),
                token(TokenKind::Const, 2, 1, 5),
                token(TokenKind::Name("c".to_string()), 2, 7, 1),
                token(TokenKind::Assign, 2, 9, 1),
                token(TokenKind::Int(10), 2, 11, 2),
                token(TokenKind::Semi, 2, 13, 1),
            ]
        );

        assert_eq!(
            tok("var name = /* 1.0; \n \n hello */ 3.9;"),
            [
                token(TokenKind::Var, 1, 1, 3),
                token(TokenKind::Name("name".to_string()), 1, 5, 4),
                token(TokenKind::Assign, 1, 10, 1),
                token(TokenKind::Float(3.9), 3, 11, 3),
                token(TokenKind::Semi, 3, 14, 1),
            ]
        );
    }
}
