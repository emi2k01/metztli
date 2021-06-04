#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Token {
    kind: TokenKind,
    pub len: usize,
}

impl Token {
    pub fn new(kind: TokenKind, len: usize) -> Self {
        Self { kind, len }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u16)]
pub enum TokenKind {
    Identifier,
    Integer,
    Float,
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

    // Trivia
    Whitespace,

    Root,
}

impl From<TokenKind> for rowan::SyntaxKind {
    fn from(kind: TokenKind) -> Self {
        Self(kind as u16)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Lang {}

impl rowan::Language for Lang {
    type Kind = TokenKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        assert!(raw.0 <= TokenKind::Root as u16);
        unsafe { std::mem::transmute::<u16, TokenKind>(raw.0) }
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}
