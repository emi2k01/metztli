#[derive(Debug, PartialEq)]
pub struct Token {
    kind: TokenKind,
    span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Ident,
    Integer,
    Float,
    Boolean,
    String,
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
