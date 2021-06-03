use crate::token::{Token, TokenKind};

pub struct Lexer<'a> {
    input: &'a [char],
    pos: usize,
    errors: Vec<String>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a [char]) -> Self {
        Self {
            input,
            pos: 0,
            errors: Vec::new(),
        }
    }

    pub fn tokens(self) -> Vec<Token> {
        todo!()
    }

    pub fn next(&mut self) -> Option<Token> {
        let ch0 = self.ch0();
        let ch1 = self.ch1();
        let ch2 = self.ch2();

        // Match trivia
        //TODO: Remove clippy attribute when we match other trivia like comments.
        #[allow(clippy::single_match)]
        match (ch0, ch1) {
            (Some(' '), _) => return Some(self.lex_whitespace()),
            _ => {}
        }

        // Match 3-char tokens
        let token_kind = match (ch0, ch1, ch2) {
            (Some('<'), Some('<'), Some('=')) => Some(TokenKind::BitwiseLeftShiftAssignment),
            (Some('>'), Some('>'), Some('=')) => Some(TokenKind::BitwiseRightShiftAssignment),
            _ => None,
        };

        if let Some(token_kind) = token_kind {
            let offset = self.pos;
            self.advance();
            self.advance();
            self.advance();

            return Some(Token::new(token_kind, (offset, 3)));
        }

        // Match 2-char tokens
        let token_kind = match (ch0, ch1) {
            // Assignment operators
            (Some('+'), Some('=')) => Some(TokenKind::AdditionAssignment),
            (Some('-'), Some('=')) => Some(TokenKind::SubtractionAssignment),
            (Some('*'), Some('=')) => Some(TokenKind::MultiplicationAssignment),
            (Some('/'), Some('=')) => Some(TokenKind::DivisionAssignment),
            (Some('%'), Some('=')) => Some(TokenKind::ModuloAssignment),
            (Some('&'), Some('=')) => Some(TokenKind::BitwiseAndAssignment),
            (Some('|'), Some('=')) => Some(TokenKind::BitwiseOrAssignment),
            (Some('^'), Some('=')) => Some(TokenKind::BitwiseXorAssignment),
            (Some('+'), Some('+')) => Some(TokenKind::IncrementOne),
            (Some('-'), Some('-')) => Some(TokenKind::SubtractOne),

            // Bitwise operators
            (Some('<'), Some('<')) => Some(TokenKind::BitwiseLeftShift),
            (Some('>'), Some('>')) => Some(TokenKind::BitwiseRightShift),

            // Logical operators
            (Some('&'), Some('&')) => Some(TokenKind::And),
            (Some('|'), Some('|')) => Some(TokenKind::Or),

            // Comparison operators
            (Some('='), Some('=')) => Some(TokenKind::Equal),
            (Some('!'), Some('=')) => Some(TokenKind::NotEqual),
            (Some('<'), Some('=')) => Some(TokenKind::LessThanEqual),
            (Some('>'), Some('=')) => Some(TokenKind::GreaterThanEqual),

            _ => None,
        };

        if let Some(token_kind) = token_kind {
            let offset = self.pos;
            self.advance();
            self.advance();

            return Some(Token::new(token_kind, (offset, 2)));
        }

        // Match 1-char tokens
        let token_kind = match ch0 {
            // Assignment operators
            Some('=') => Some(TokenKind::Assignment),

            // Arithmetic operators
            Some('+') => Some(TokenKind::Plus),
            Some('-') => Some(TokenKind::Minus),
            Some('*') => Some(TokenKind::Asterisk),
            Some('/') => Some(TokenKind::Slash),
            Some('%') => Some(TokenKind::Modulo),

            // Bitwise operators
            Some('~') => Some(TokenKind::BitwiseNot),
            Some('&') => Some(TokenKind::BitwiseAnd),
            Some('|') => Some(TokenKind::BitwiseOr),
            Some('^') => Some(TokenKind::BitwiseXor),

            // Logical operators
            Some('!') => Some(TokenKind::Not),

            // Comparison operators
            Some('<') => Some(TokenKind::LessThan),
            Some('>') => Some(TokenKind::GreaterThan),

            // Punctuation
            Some('.') => Some(TokenKind::Period),
            Some(',') => Some(TokenKind::Comma),
            Some(';') => Some(TokenKind::Semicolon),
            Some(':') => Some(TokenKind::Colon),

            // Delimeters
            Some('(') => Some(TokenKind::LeftParenthesis),
            Some(')') => Some(TokenKind::RightParenthesis),
            Some('[') => Some(TokenKind::LeftBracket),
            Some(']') => Some(TokenKind::RightBracket),
            Some('{') => Some(TokenKind::LeftBrace),
            Some('}') => Some(TokenKind::RightBrace),
            _ => None,
        };

        if let Some(token_kind) = token_kind {
            let offset = self.pos;
            self.advance();

            return Some(Token::new(token_kind, (offset, 1)));
        }

        // Match n-char tokens
        match (ch0, ch1) {
            (Some('0'), Some('b')) => Some(self.lex_binary_number_literal()),
            (Some('0'), Some('o')) => Some(self.lex_octal_number_literal()),
            (Some('0'), Some('x')) => Some(self.lex_hex_number_literal()),
            (Some(ch0), _) if ch0.is_ascii_digit() => Some(self.lex_decimal_number_literal()),
            (None, None) => None,
            _ => todo!(),
        }
    }

    fn lex_decimal_number_literal(&mut self) -> Token {
        //TODO: Add diagnostics for malformed literals

        let offset = self.pos;
        let mut width = 0;
        let mut token_kind = TokenKind::Integer;

        let dec_pred = |ch0: char| ch0.is_ascii_digit() || ch0 == '_';

        // Read integer part
        width += self.advance_while(dec_pred);

        // Case where the number is a float
        if let Some('.') = self.ch0() {
            token_kind = TokenKind::Float;
            width += 1;
            self.advance();
            width += self.advance_while(dec_pred)
        } else if let Some('E') = self.ch0() {
            token_kind = TokenKind::Float;
            width += 1;
            self.advance();

            if let Some('+' | '-') = self.ch0() {
                width += 1;
                self.advance();
            }

            width += self.advance_while(dec_pred);
        }

        Token::new(token_kind, (offset, width))
    }

    fn lex_binary_number_literal(&mut self) -> Token {
        //TODO: Add diagnostic for invalid digits

        let offset = self.pos;
        // skip 0b123
        self.advance();
        self.advance();

        let mut width = 2;
        width += self.advance_while(|ch0| ch0.is_ascii_digit() || ch0 == '_');

        Token::new(TokenKind::Integer, (offset, width))
    }

    fn lex_octal_number_literal(&mut self) -> Token {
        //TODO: Add diagnostic for invalid digits

        let offset = self.pos;
        // skip 0o123
        self.advance();
        self.advance();

        let mut width = 2;
        width += self.advance_while(|ch0| ch0.is_ascii_digit() || ch0 == '_');

        Token::new(TokenKind::Integer, (offset, width))
    }

    fn lex_hex_number_literal(&mut self) -> Token {
        //TODO: Add diagnostic for invalid digits

        let offset = self.pos;
        // skip 0x123
        self.advance();
        self.advance();

        let mut width = 2;
        width += self.advance_while(|ch0| {
            ch0.is_ascii_digit()
                || ch0 == '_'
                || ('a'..='f').contains(&ch0)
                || ('A'..='F').contains(&ch0)
        });

        Token::new(TokenKind::Integer, (offset, width))
    }

    fn lex_whitespace(&mut self) -> Token {
        let offset = self.pos;
        let mut width = 0;

        while let Some(' ') = self.ch0() {
            width += 1;
            self.advance();
        }

        Token::new(TokenKind::Whitespace, (offset, width))
    }

    fn ch0(&self) -> Option<char> {
        self.input.get(self.pos).copied()
    }

    fn ch1(&self) -> Option<char> {
        self.input.get(self.pos + 1).copied()
    }

    fn ch2(&self) -> Option<char> {
        self.input.get(self.pos + 2).copied()
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    /// Advances the lexer position while `pred` evaluates to true and returns the number of
    /// positions it advanced
    fn advance_while(&mut self, mut pred: impl FnMut(char) -> bool) -> usize {
        let mut n = 0;
        while let Some(ch) = self.ch0() {
            if pred(ch) {
                self.advance();
                n += 1;
            } else {
                break;
            }
        }

        n
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decimal_number_literal() {
        let input = "921_213_203".chars().collect::<Vec<_>>();
        let mut lexer = Lexer::new(&input);
        assert_eq!(lexer.next(), Some(Token::new(TokenKind::Integer, (0, 11))),);
    }

    #[test]
    fn test_binary_number_literal() {
        let input = "0b1001_0110_0010".chars().collect::<Vec<_>>();
        let mut lexer = Lexer::new(&input);
        assert_eq!(lexer.next(), Some(Token::new(TokenKind::Integer, (0, 16))),);
    }

    #[test]
    fn test_octal_number_literal() {
        let input = "0o123_456_712".chars().collect::<Vec<_>>();
        let mut lexer = Lexer::new(&input);
        assert_eq!(lexer.next(), Some(Token::new(TokenKind::Integer, (0, 13))),);
    }

    #[test]
    fn test_hexadecimal_number_literal() {
        let input = "0x10_bD_Ac_0F".chars().collect::<Vec<_>>();
        let mut lexer = Lexer::new(&input);
        assert_eq!(lexer.next(), Some(Token::new(TokenKind::Integer, (0, 13))),);
    }

    #[test]
    fn test_float_number_literal_decimal_notation() {
        let input = "100_000.234_213".chars().collect::<Vec<_>>();
        let mut lexer = Lexer::new(&input);
        assert_eq!(lexer.next(), Some(Token::new(TokenKind::Float, (0, 15))),);
    }

    #[test]
    fn test_float_number_literal_exponential_notation() {
        let input = "2E+1_3".chars().collect::<Vec<_>>();
        let mut lexer = Lexer::new(&input);
        assert_eq!(lexer.next(), Some(Token::new(TokenKind::Float, (0, 6))),);
    }

    #[test]
    fn test_delimeters() {
        let input = "( { [ ) } ]".chars().collect::<Vec<_>>();
        let mut lexer = Lexer::new(&input);
        assert_eq!(
            lexer.next(),
            Some(Token::new(TokenKind::LeftParenthesis, (0, 1))),
        );
        assert_eq!(
            lexer.next(),
            Some(Token::new(TokenKind::Whitespace, (1, 1))),
        );
        assert_eq!(lexer.next(), Some(Token::new(TokenKind::LeftBrace, (2, 1))),);
        assert_eq!(
            lexer.next(),
            Some(Token::new(TokenKind::Whitespace, (3, 1))),
        );
        assert_eq!(
            lexer.next(),
            Some(Token::new(TokenKind::LeftBracket, (4, 1))),
        );
        assert_eq!(
            lexer.next(),
            Some(Token::new(TokenKind::Whitespace, (5, 1))),
        );
        assert_eq!(
            lexer.next(),
            Some(Token::new(TokenKind::RightParenthesis, (6, 1))),
        );
        assert_eq!(
            lexer.next(),
            Some(Token::new(TokenKind::Whitespace, (7, 1))),
        );
        assert_eq!(
            lexer.next(),
            Some(Token::new(TokenKind::RightBrace, (8, 1))),
        );
        assert_eq!(
            lexer.next(),
            Some(Token::new(TokenKind::Whitespace, (9, 1))),
        );
        assert_eq!(
            lexer.next(),
            Some(Token::new(TokenKind::RightBracket, (10, 1))),
        );
    }
}
