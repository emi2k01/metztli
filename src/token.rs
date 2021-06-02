#[derive(Debug, PartialEq)]
pub struct Token {
    kind: TokenKind,
    span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: impl Into<Span>) -> Self {
        Self {
            kind,
            span: span.into(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Identifier,
    Integer,
    Float,
    Boolean,
    String,

    // Assignment operators
    Assignment,
    AdditionAssignment,
    SubtractionAssignment,
    MultiplicationAssignment,
    DivisionAssignment,
    ModuloAssignment,
    BitwiseAndAssignment,
    BitwiseOrAssignment,
    BitwiseXorAssignment,
    BitwiseLeftShiftAssignment,
    BitwiseRightShiftAssignment,

    // Increment and decrement operators
    IncrementOne,
    SubtractOne,

    // Arithmetic operators
    Plus,
    Minus,
    Asterisk,
    Slash,
    Modulo,

    // Bitwise operators
    BitwiseNot,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseLeftShift,
    BitwiseRightShift,

    // Logical operators
    Not,
    And,
    Or,

    // Comparison operators
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,

    // Punctuation
    Period,
    Comma,
    Semicolon,
    Colon,

    // Delimeters
    LeftParenthesis,
    RightParenthesis,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
}

#[derive(Debug, PartialEq)]
pub struct Span {
    pub offset: usize,
    pub width: usize,
}

impl Span {
    pub fn new(offset: usize, width: usize) -> Self {
        Self { offset, width }
    }
}

impl From<(usize, usize)> for Span {
    fn from((offset, width): (usize, usize)) -> Self {
        Self::new(offset, width)
    }
}
