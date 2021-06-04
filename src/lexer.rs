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

    pub fn tokens(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();

        while let Some(token) = self.next() {
            tokens.push(token);
        }

        tokens
    }

    pub fn next(&mut self) -> Option<Token> {
        let ch0 = self.ch0();
        let ch1 = self.ch1();
        let ch2 = self.ch2();

        // Match trivia
        // TODO: Remove clippy attribute when we match other trivia like comments.
        #[allow(clippy::single_match)]
        match (ch0, ch1) {
            (Some(' '), _) => return Some(self.lex_whitespace()),
            _ => {},
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

            return Some(Token::new(
                token_kind,
                self.input[offset..offset + 3].iter().collect(),
                (offset, 3),
            ));
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

            return Some(Token::new(
                token_kind,
                self.input[offset..offset + 2].iter().collect(),
                (offset, 2),
            ));
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

            return Some(Token::new(
                token_kind,
                self.input[offset..offset + 1].iter().collect(),
                (offset, 1),
            ));
        }

        // Match n-char tokens
        let mut token = match (ch0, ch1) {
            (Some('0'), Some('b')) => self.lex_binary_number_literal(),
            (Some('0'), Some('o')) => self.lex_octal_number_literal(),
            (Some('0'), Some('x')) => self.lex_hex_number_literal(),
            (Some('"'), _) => self.lex_string_literal(),
            (Some(ch0), _) if ch0.is_ascii_digit() => self.lex_decimal_number_literal(),
            (Some(ch0), _) if is_ident_start(ch0) => self.lex_identifier(),
            (None, None) => return None,
            _ => todo!(),
        };

        let span = token.span();

        token.set_text(
            self.input[span.offset..span.offset + span.width]
                .iter()
                .collect(),
        );

        Some(token)
    }

    fn lex_decimal_number_literal(&mut self) -> Token {
        // TODO: Add diagnostics for malformed literals

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

        Token::new(token_kind, String::new(), (offset, width))
    }

    fn lex_binary_number_literal(&mut self) -> Token {
        // TODO: Add diagnostic for invalid digits

        let offset = self.pos;
        // skip 0b123
        self.advance();
        self.advance();

        let mut width = 2;
        width += self.advance_while(|ch0| ch0.is_ascii_digit() || ch0 == '_');

        Token::new(TokenKind::Integer, String::new(), (offset, width))
    }

    fn lex_octal_number_literal(&mut self) -> Token {
        // TODO: Add diagnostic for invalid digits

        let offset = self.pos;
        // skip 0o123
        self.advance();
        self.advance();

        let mut width = 2;
        width += self.advance_while(|ch0| ch0.is_ascii_digit() || ch0 == '_');

        Token::new(TokenKind::Integer, String::new(), (offset, width))
    }

    fn lex_hex_number_literal(&mut self) -> Token {
        // TODO: Add diagnostic for invalid digits

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

        Token::new(TokenKind::Integer, String::new(), (offset, width))
    }

    fn lex_string_literal(&mut self) -> Token {
        // Skip opening quote
        self.advance();

        let offset = self.pos;
        let width = self.advance_while(|ch| ch != '"');

        // Skip closing quote
        self.advance();

        Token::new(TokenKind::String, String::new(), (offset, width))
    }

    fn lex_whitespace(&mut self) -> Token {
        let offset = self.pos;
        let mut width = 0;

        while let Some(' ') = self.ch0() {
            width += 1;
            self.advance();
        }

        Token::new(
            TokenKind::Whitespace,
            std::iter::repeat(' ').take(width).collect(),
            (offset, width),
        )
    }

    fn lex_identifier(&mut self) -> Token {
        let offset = self.pos;
        let mut width = 1;
        self.advance();

        width += self.advance_while(|ch| is_ident_start(ch) || ch.is_ascii_digit());

        Token::new(TokenKind::Identifier, String::new(), (offset, width))
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

    /// Advances the lexer position while `pred` evaluates to true and returns
    /// the number of positions it advanced
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

fn is_ident_start(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == 'ñ' || ch == '_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_decimal_number_literal() {
        let input = "921_213_203".chars().collect::<Vec<_>>();
        let mut lexer = Lexer::new(&input);
        assert_eq!(
            lexer.next(),
            Some(Token::new(
                TokenKind::Integer,
                String::from("921_213_203"),
                (0, 11)
            )),
        );
    }

    #[test]
    fn test_binary_number_literal() {
        let input = "0b1001_0110_0010".chars().collect::<Vec<_>>();
        let mut lexer = Lexer::new(&input);
        assert_eq!(
            lexer.next(),
            Some(Token::new(
                TokenKind::Integer,
                String::from("0b1001_0110_0010"),
                (0, 16)
            )),
        );
    }

    #[test]
    fn test_octal_number_literal() {
        let input = "0o123_456_712".chars().collect::<Vec<_>>();
        let mut lexer = Lexer::new(&input);
        assert_eq!(
            lexer.next(),
            Some(Token::new(
                TokenKind::Integer,
                String::from("0o123_456_712"),
                (0, 13)
            )),
        );
    }

    #[test]
    fn test_hexadecimal_number_literal() {
        let input = "0x10_bD_Ac_0F".chars().collect::<Vec<_>>();
        let mut lexer = Lexer::new(&input);
        assert_eq!(
            lexer.next(),
            Some(Token::new(
                TokenKind::Integer,
                String::from("0x10_bD_Ac_0F"),
                (0, 13)
            )),
        );
    }

    #[test]
    fn test_float_number_literal_decimal_notation() {
        let input = "100_000.234_213".chars().collect::<Vec<_>>();
        let mut lexer = Lexer::new(&input);
        assert_eq!(
            lexer.next(),
            Some(Token::new(
                TokenKind::Float,
                String::from("100_000.234_213"),
                (0, 15)
            )),
        );
    }

    #[test]
    fn test_float_number_literal_exponential_notation() {
        let input = "2E+1_3".chars().collect::<Vec<_>>();
        let mut lexer = Lexer::new(&input);
        assert_eq!(
            lexer.next(),
            Some(Token::new(TokenKind::Float, String::from("2E+1_3"), (0, 6))),
        );
    }

    #[test]
    fn test_string_literal() {
        let input = r#""€l níño esŧa en\tfer\nmo""#.chars().collect::<Vec<_>>();

        let mut lexer = Lexer::new(&input);
        let tokens = lexer.tokens();

        let expected = vec![Token::new(
            TokenKind::String,
            String::from("€l níño esŧa en\\tfer\\nmo"),
            (1, 24),
        )];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_assignment_operators() {
        let input = "= += -= *= /= %= &= |= ^= <<= >>="
            .chars()
            .collect::<Vec<_>>();

        let mut lexer = Lexer::new(&input);
        let tokens = lexer.tokens();

        let expected = vec![
            Token::new(TokenKind::Assignment, String::from("="), (0, 1)),
            Token::new(TokenKind::Whitespace, String::from(" "), (1, 1)),
            Token::new(TokenKind::AdditionAssignment, String::from("+="), (2, 2)),
            Token::new(TokenKind::Whitespace, String::from(" "), (4, 1)),
            Token::new(TokenKind::SubtractionAssignment, String::from("-="), (5, 2)),
            Token::new(TokenKind::Whitespace, String::from(" "), (7, 1)),
            Token::new(
                TokenKind::MultiplicationAssignment,
                String::from("*="),
                (8, 2),
            ),
            Token::new(TokenKind::Whitespace, String::from(" "), (10, 1)),
            Token::new(TokenKind::DivisionAssignment, String::from("/="), (11, 2)),
            Token::new(TokenKind::Whitespace, String::from(" "), (13, 1)),
            Token::new(TokenKind::ModuloAssignment, String::from("%="), (14, 2)),
            Token::new(TokenKind::Whitespace, String::from(" "), (16, 1)),
            Token::new(TokenKind::BitwiseAndAssignment, String::from("&="), (17, 2)),
            Token::new(TokenKind::Whitespace, String::from(" "), (19, 1)),
            Token::new(TokenKind::BitwiseOrAssignment, String::from("|="), (20, 2)),
            Token::new(TokenKind::Whitespace, String::from(" "), (22, 1)),
            Token::new(TokenKind::BitwiseXorAssignment, String::from("^="), (23, 2)),
            Token::new(TokenKind::Whitespace, String::from(" "), (25, 1)),
            Token::new(
                TokenKind::BitwiseLeftShiftAssignment,
                String::from("<<="),
                (26, 3),
            ),
            Token::new(TokenKind::Whitespace, String::from(" "), (29, 1)),
            Token::new(
                TokenKind::BitwiseRightShiftAssignment,
                String::from(">>="),
                (30, 3),
            ),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_increment_operators() {
        let input = "++ --".chars().collect::<Vec<_>>();

        let mut lexer = Lexer::new(&input);
        let tokens = lexer.tokens();

        let expected = vec![
            Token::new(TokenKind::IncrementOne, String::from("++"), (0, 2)),
            Token::new(TokenKind::Whitespace, String::from(" "), (2, 1)),
            Token::new(TokenKind::SubtractOne, String::from("--"), (3, 2)),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_bitwise_operators() {
        let input = "~ & | ^ << >>".chars().collect::<Vec<_>>();

        let mut lexer = Lexer::new(&input);
        let tokens = lexer.tokens();

        let expected = vec![
            Token::new(TokenKind::BitwiseNot, String::from("~"), (0, 1)),
            Token::new(TokenKind::Whitespace, String::from(" "), (1, 1)),
            Token::new(TokenKind::BitwiseAnd, String::from("&"), (2, 1)),
            Token::new(TokenKind::Whitespace, String::from(" "), (3, 1)),
            Token::new(TokenKind::BitwiseOr, String::from("|"), (4, 1)),
            Token::new(TokenKind::Whitespace, String::from(" "), (5, 1)),
            Token::new(TokenKind::BitwiseXor, String::from("^"), (6, 1)),
            Token::new(TokenKind::Whitespace, String::from(" "), (7, 1)),
            Token::new(TokenKind::BitwiseLeftShift, String::from("<<"), (8, 2)),
            Token::new(TokenKind::Whitespace, String::from(" "), (10, 1)),
            Token::new(TokenKind::BitwiseRightShift, String::from(">>"), (11, 2)),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_logical_operators() {
        let input = "! && ||".chars().collect::<Vec<_>>();

        let mut lexer = Lexer::new(&input);
        let tokens = lexer.tokens();

        let expected = vec![
            Token::new(TokenKind::Not, String::from("!"), (0, 1)),
            Token::new(TokenKind::Whitespace, String::from(" "), (1, 1)),
            Token::new(TokenKind::And, String::from("&&"), (2, 2)),
            Token::new(TokenKind::Whitespace, String::from(" "), (4, 1)),
            Token::new(TokenKind::Or, String::from("||"), (5, 2)),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_comparison_operators() {
        let input = "== != < <= > >=".chars().collect::<Vec<_>>();

        let mut lexer = Lexer::new(&input);
        let tokens = lexer.tokens();

        let expected = vec![
            Token::new(TokenKind::Equal, String::from("=="), (0, 2)),
            Token::new(TokenKind::Whitespace, String::from(" "), (2, 1)),
            Token::new(TokenKind::NotEqual, String::from("!="), (3, 2)),
            Token::new(TokenKind::Whitespace, String::from(" "), (5, 1)),
            Token::new(TokenKind::LessThan, String::from("<"), (6, 1)),
            Token::new(TokenKind::Whitespace, String::from(" "), (7, 1)),
            Token::new(TokenKind::LessThanEqual, String::from("<="), (8, 2)),
            Token::new(TokenKind::Whitespace, String::from(" "), (10, 1)),
            Token::new(TokenKind::GreaterThan, String::from(">"), (11, 1)),
            Token::new(TokenKind::Whitespace, String::from(" "), (12, 1)),
            Token::new(TokenKind::GreaterThanEqual, String::from(">="), (13, 2)),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_punctuation_operators() {
        let input = ". , ; :".chars().collect::<Vec<_>>();

        let mut lexer = Lexer::new(&input);
        let tokens = lexer.tokens();

        let expected = vec![
            Token::new(TokenKind::Period, String::from("."), (0, 1)),
            Token::new(TokenKind::Whitespace, String::from(" "), (1, 1)),
            Token::new(TokenKind::Comma, String::from(","), (2, 1)),
            Token::new(TokenKind::Whitespace, String::from(" "), (3, 1)),
            Token::new(TokenKind::Semicolon, String::from(";"), (4, 1)),
            Token::new(TokenKind::Whitespace, String::from(" "), (5, 1)),
            Token::new(TokenKind::Colon, String::from(":"), (6, 1)),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_delimeters() {
        let input = "( { [ ) } ]".chars().collect::<Vec<_>>();

        let mut lexer = Lexer::new(&input);
        let tokens = lexer.tokens();

        let expected = vec![
            Token::new(TokenKind::LeftParenthesis, String::from("("), (0, 1)),
            Token::new(TokenKind::Whitespace, String::from(" "), (1, 1)),
            Token::new(TokenKind::LeftBrace, String::from("{"), (2, 1)),
            Token::new(TokenKind::Whitespace, String::from(" "), (3, 1)),
            Token::new(TokenKind::LeftBracket, String::from("["), (4, 1)),
            Token::new(TokenKind::Whitespace, String::from(" "), (5, 1)),
            Token::new(TokenKind::RightParenthesis, String::from(")"), (6, 1)),
            Token::new(TokenKind::Whitespace, String::from(" "), (7, 1)),
            Token::new(TokenKind::RightBrace, String::from("}"), (8, 1)),
            Token::new(TokenKind::Whitespace, String::from(" "), (9, 1)),
            Token::new(TokenKind::RightBracket, String::from("]"), (10, 1)),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_trivia() {
        let input = "    |    ".chars().collect::<Vec<_>>();

        let mut lexer = Lexer::new(&input);
        let tokens = lexer.tokens();

        let expected = vec![
            Token::new(TokenKind::Whitespace, String::from("    "), (0, 4)),
            Token::new(TokenKind::BitwiseOr, String::from("|"), (4, 1)),
            Token::new(TokenKind::Whitespace, String::from("    "), (5, 4)),
        ];

        assert_eq!(tokens, expected);
    }

    #[test]
    fn test_identifier() {
        let input = "edad_niño si MyEstructura RFC_932"
            .chars()
            .collect::<Vec<_>>();

        let mut lexer = Lexer::new(&input);
        let tokens = lexer.tokens();

        let expected = vec![
            Token::new(TokenKind::Identifier, String::from("edad_niño"), (0, 9)),
            Token::new(TokenKind::Whitespace, String::from(" "), (9, 1)),
            Token::new(TokenKind::Identifier, String::from("si"), (10, 2)),
            Token::new(TokenKind::Whitespace, String::from(" "), (12, 1)),
            Token::new(
                TokenKind::Identifier,
                String::from("MyEstructura"),
                (13, 12),
            ),
            Token::new(TokenKind::Whitespace, String::from(" "), (25, 1)),
            Token::new(TokenKind::Identifier, String::from("RFC_932"), (26, 7)),
        ];

        assert_eq!(tokens, expected);
    }
}
