use std::str::Chars;

use crate::token::{Token, TokenKind};

pub struct Cursor<'a> {
    initial_len: usize,
    chars: Chars<'a>,
}

impl<'a> Cursor<'a> {
    pub fn new(input: &'a str) -> Cursor<'a> {
        Self {
            initial_len: input.len(),
            chars: input.chars(),
        }
    }

    pub fn advance_token(&mut self) -> Token {
        // Match trivia
        // TODO: Remove clippy attribute when we match other trivia like comments.
        let ch0 = self.bump().unwrap();
        let ch1 = self.nth_char(0);
        let ch2 = self.nth_char(1);

        #[allow(clippy::single_match)]
        match (ch0, ch1) {
            (' ', _) => return Token::new(self.lex_whitespace(), self.len_consumed()),
            _ => {},
        }

        // Match 3-char tokens
        let token_kind = match (ch0, ch1, ch2) {
            ('<', '<', '=') => Some(TokenKind::BitwiseLeftShiftAssignment),
            ('>', '>', '=') => Some(TokenKind::BitwiseRightShiftAssignment),
            _ => None,
        };

        if let Some(token_kind) = token_kind {
            self.bump();
            self.bump();
            return Token::new(token_kind, 3);
        }

        // Match 2-char tokens
        let token_kind = match (ch0, ch1) {
            // Assignment operators
            ('+', '=') => Some(TokenKind::AdditionAssignment),
            ('-', '=') => Some(TokenKind::SubtractionAssignment),
            ('*', '=') => Some(TokenKind::MultiplicationAssignment),
            ('/', '=') => Some(TokenKind::DivisionAssignment),
            ('%', '=') => Some(TokenKind::ModuloAssignment),
            ('&', '=') => Some(TokenKind::BitwiseAndAssignment),
            ('|', '=') => Some(TokenKind::BitwiseOrAssignment),
            ('^', '=') => Some(TokenKind::BitwiseXorAssignment),
            ('+', '+') => Some(TokenKind::IncrementOne),
            ('-', '-') => Some(TokenKind::SubtractOne),

            // Bitwise operators
            ('<', '<') => Some(TokenKind::BitwiseLeftShift),
            ('>', '>') => Some(TokenKind::BitwiseRightShift),

            // Logical operators
            ('&', '&') => Some(TokenKind::And),
            ('|', '|') => Some(TokenKind::Or),

            // Comparison operators
            ('=', '=') => Some(TokenKind::Equal),
            ('!', '=') => Some(TokenKind::NotEqual),
            ('<', '=') => Some(TokenKind::LessThanEqual),
            ('>', '=') => Some(TokenKind::GreaterThanEqual),

            _ => None,
        };

        if let Some(token_kind) = token_kind {
            self.bump();
            return Token::new(token_kind, 2);
        }

        // Match 1-char tokens
        let token_kind = match ch0 {
            // Assignment operators
            '=' => Some(TokenKind::Assignment),

            // Arithmetic operators
            '+' => Some(TokenKind::Plus),
            '-' => Some(TokenKind::Minus),
            '*' => Some(TokenKind::Asterisk),
            '/' => Some(TokenKind::Slash),
            '%' => Some(TokenKind::Modulo),

            // Bitwise operators
            '~' => Some(TokenKind::BitwiseNot),
            '&' => Some(TokenKind::BitwiseAnd),
            '|' => Some(TokenKind::BitwiseOr),
            '^' => Some(TokenKind::BitwiseXor),

            // Logical operators
            '!' => Some(TokenKind::Not),

            // Comparison operators
            '<' => Some(TokenKind::LessThan),
            '>' => Some(TokenKind::GreaterThan),

            // Punctuation
            '.' => Some(TokenKind::Period),
            ',' => Some(TokenKind::Comma),
            ';' => Some(TokenKind::Semicolon),
            ':' => Some(TokenKind::Colon),

            // Delimeters
            '(' => Some(TokenKind::LeftParenthesis),
            ')' => Some(TokenKind::RightParenthesis),
            '[' => Some(TokenKind::LeftBracket),
            ']' => Some(TokenKind::RightBracket),
            '{' => Some(TokenKind::LeftBrace),
            '}' => Some(TokenKind::RightBrace),
            _ => None,
        };

        if let Some(token_kind) = token_kind {
            return Token::new(token_kind, 1);
        }

        // Match n-char tokens
        let token_kind = match (ch0, ch1) {
            ('0', 'b') => self.lex_binary_number_literal(),
            ('0', 'o') => self.lex_octal_number_literal(),
            ('0', 'x') => self.lex_hex_number_literal(),
            ('"', _) => self.lex_string_literal(),
            (ch0, _) if ch0.is_ascii_digit() => self.lex_decimal_number_literal(),
            (ch0, _) if is_ident_start(ch0) => self.lex_identifier(),
            _ => unreachable!(),
        };

        Token::new(token_kind, self.len_consumed())
    }

    fn len_consumed(&self) -> usize {
        self.initial_len - self.chars.as_str().len()
    }

    fn nth_char(&self, n: usize) -> char {
        self.chars().nth(n).unwrap_or('\0')
    }

    fn chars(&self) -> Chars<'a> {
        self.chars.clone()
    }

    fn bump(&mut self) -> Option<char> {
        self.chars.next()
    }

    fn lex_decimal_number_literal(&mut self) -> TokenKind {
        // TODO: Add diagnostics for malformed literals
        let mut token_kind = TokenKind::Integer;

        let dec_pred = |ch0: char| ch0.is_ascii_digit() || ch0 == '_';

        // Read integer part
        self.advance_while(dec_pred);

        // Case where the number is a float
        if self.ch0() == '.' {
            token_kind = TokenKind::Float;
            self.bump();
            self.advance_while(dec_pred);
        } else if self.ch0() == 'E' {
            token_kind = TokenKind::Float;
            self.bump();

            if self.ch0() == '+' || self.ch0() == '-' {
                self.bump();
            }

            self.advance_while(dec_pred);
        }

        token_kind
    }

    fn lex_binary_number_literal(&mut self) -> TokenKind {
        // TODO: Add diagnostic for invalid digits

        // skip 0b123
        self.bump();
        self.advance_while(|ch0| ch0.is_ascii_digit() || ch0 == '_');

        TokenKind::Integer
    }

    fn lex_octal_number_literal(&mut self) -> TokenKind {
        // TODO: Add diagnostic for invalid digits

        // skip 0o123
        self.bump();
        self.advance_while(|ch0| ch0.is_ascii_digit() || ch0 == '_');

        TokenKind::Integer
    }

    fn lex_hex_number_literal(&mut self) -> TokenKind {
        // TODO: Add diagnostic for invalid digits

        // skip 0x123
        self.bump();

        self.advance_while(|ch0| {
            ch0.is_ascii_digit()
                || ch0 == '_'
                || ('a'..='f').contains(&ch0)
                || ('A'..='F').contains(&ch0)
        });

        TokenKind::Integer
    }

    fn lex_string_literal(&mut self) -> TokenKind {
        self.advance_while(|ch| ch != '"');
        self.bump();

        TokenKind::String
    }

    fn lex_whitespace(&mut self) -> TokenKind {
        while self.ch0() == ' ' {
            self.bump();
        }

        TokenKind::Whitespace
    }

    fn lex_identifier(&mut self) -> TokenKind {
        self.advance_while(|ch| is_ident_start(ch) || ch.is_ascii_digit());

        TokenKind::Identifier
    }

    fn ch0(&self) -> char {
        self.nth_char(0)
    }

    fn ch1(&self) -> char {
        self.nth_char(1)
    }

    fn ch2(&self) -> char {
        self.nth_char(2)
    }

    /// Advances the lexer position while `pred` evaluates to true and returns
    /// the number of positions it advanced
    fn advance_while(&mut self, mut pred: impl FnMut(char) -> bool) {
        while pred(self.ch0()) {
            self.bump();
        }
    }
}

pub fn tokenize(mut input: &str) -> impl Iterator<Item = Token> + '_ {
    std::iter::from_fn(move || {
        if input.is_empty() {
            return None;
        }

        let token = first_token(input);
        input = &input[token.len..];
        Some(token)
    })
}

fn first_token(input: &str) -> Token {
    let mut cursor = Cursor::new(input);
    cursor.advance_token()
}

fn is_ident_start(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == 'ñ' || ch == '_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decimal_number_literal() {
        let input = "921_213_203";
        let tokens = tokenize(&input).collect::<Vec<_>>();
        assert_eq!(
            tokens.first().copied(),
            Some(Token::new(TokenKind::Integer, 11)),
        );
    }

    #[test]
    fn test_binary_number_literal() {
        let input = "0b1001_0110_0010";
        let tokens = tokenize(&input).collect::<Vec<_>>();
        assert_eq!(
            tokens.first().copied(),
            Some(Token::new(TokenKind::Integer, 16)),
        );
    }

    #[test]
    fn test_octal_number_literal() {
        let input = "0o123_456_712";
        let tokens = tokenize(&input).collect::<Vec<_>>();
        assert_eq!(
            tokens.first().copied(),
            Some(Token::new(TokenKind::Integer, 13)),
        );
    }

    #[test]
    fn test_hexadecimal_number_literal() {
        let input = "0x10_bD_Ac_0F";
        let tokens = tokenize(&input).collect::<Vec<_>>();
        assert_eq!(
            tokens.first().copied(),
            Some(Token::new(TokenKind::Integer, 13)),
        );
    }

    #[test]
    fn test_float_number_literal_decimal_notation() {
        let input = "100_000.234_213";
        let tokens = tokenize(&input).collect::<Vec<_>>();
        assert_eq!(
            tokens.first().copied(),
            Some(Token::new(TokenKind::Float, 15)),
        );
    }

    #[test]
    fn test_float_number_literal_exponential_notation() {
        let input = "2E+1_3";

        let tokens = tokenize(&input).collect::<Vec<_>>();

        assert_eq!(
            tokens.first().copied(),
            Some(Token::new(TokenKind::Float, 6)),
        );
    }

    #[test]
    fn test_string_literal() {
        let input = r#""€l níño esŧa en\tfer\nmo""#;

        let tokens = tokenize(&input).collect::<Vec<_>>();

        let expected = vec![Token::new(TokenKind::String, 31)];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_assignment_operators() {
        let input = "= += -= *= /= %= &= |= ^= <<= >>=";

        let tokens = tokenize(&input).collect::<Vec<_>>();

        let expected = vec![
            Token::new(TokenKind::Assignment, 1),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::AdditionAssignment, 2),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::SubtractionAssignment, 2),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::MultiplicationAssignment, 2),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::DivisionAssignment, 2),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::ModuloAssignment, 2),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::BitwiseAndAssignment, 2),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::BitwiseOrAssignment, 2),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::BitwiseXorAssignment, 2),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::BitwiseLeftShiftAssignment, 3),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::BitwiseRightShiftAssignment, 3),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_increment_operators() {
        let input = "++ --";

        let tokens = tokenize(&input).collect::<Vec<_>>();

        let expected = vec![
            Token::new(TokenKind::IncrementOne, 2),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::SubtractOne, 2),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_bitwise_operators() {
        let input = "~ & | ^ << >>";

        let tokens = tokenize(&input).collect::<Vec<_>>();

        let expected = vec![
            Token::new(TokenKind::BitwiseNot, 1),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::BitwiseAnd, 1),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::BitwiseOr, 1),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::BitwiseXor, 1),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::BitwiseLeftShift, 2),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::BitwiseRightShift, 2),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_logical_operators() {
        let input = "! && ||";

        let tokens = tokenize(&input).collect::<Vec<_>>();

        let expected = vec![
            Token::new(TokenKind::Not, 1),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::And, 2),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::Or, 2),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_comparison_operators() {
        let input = "== != < <= > >=";

        let tokens = tokenize(&input).collect::<Vec<_>>();

        let expected = vec![
            Token::new(TokenKind::Equal, 2),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::NotEqual, 2),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::LessThan, 1),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::LessThanEqual, 2),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::GreaterThan, 1),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::GreaterThanEqual, 2),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_punctuation_operators() {
        let input = ". , ; :";

        let tokens = tokenize(&input).collect::<Vec<_>>();

        let expected = vec![
            Token::new(TokenKind::Period, 1),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::Comma, 1),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::Semicolon, 1),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::Colon, 1),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_delimeters() {
        let input = "( { [ ) } ]";

        let tokens = tokenize(&input).collect::<Vec<_>>();

        let expected = vec![
            Token::new(TokenKind::LeftParenthesis, 1),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::LeftBrace, 1),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::LeftBracket, 1),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::RightParenthesis, 1),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::RightBrace, 1),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::RightBracket, 1),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_trivia() {
        let input = "    |    ";

        let tokens = tokenize(&input).collect::<Vec<_>>();

        let expected = vec![
            Token::new(TokenKind::Whitespace, 4),
            Token::new(TokenKind::BitwiseOr, 1),
            Token::new(TokenKind::Whitespace, 4),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_identifier() {
        let input = "edad_niño si MyEstructura RFC_932";

        let tokens = tokenize(&input).collect::<Vec<_>>();

        let expected = vec![
            Token::new(TokenKind::Identifier, 10),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::Identifier, 2),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::Identifier, 12),
            Token::new(TokenKind::Whitespace, 1),
            Token::new(TokenKind::Identifier, 7),
        ];

        assert_eq!(tokens, expected);
    }
}
