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

        match (ch0, ch1) {
            (Some('0'), Some('b')) => Some(self.lex_bin_num_lit()),
            (Some('0'), Some('o')) => Some(self.lex_octal_num_lit()),
            (Some('0'), Some('x')) => Some(self.lex_hex_num_lit()),
            (Some(ch0), _) if ch0.is_ascii_digit() => Some(self.lex_dec_num_lit()),
            (None, None) => None,
            _ => todo!(),
        }
    }

    fn lex_dec_num_lit(&mut self) -> Token {
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
        }

        Token::new(token_kind, (offset, width))
    }

    fn lex_bin_num_lit(&mut self) -> Token {
        //TODO: Add diagnostic for invalid digits

        let offset = self.pos;
        // skip 0b123
        self.advance();
        self.advance();

        let mut width = 2;
        width += self.advance_while(|ch0| ch0.is_ascii_digit() || ch0 == '_');

        Token::new(TokenKind::Integer, (offset, width))
    }

    fn lex_octal_num_lit(&mut self) -> Token {
        //TODO: Add diagnostic for invalid digits

        let offset = self.pos;
        // skip 0o123
        self.advance();
        self.advance();

        let mut width = 2;
        width += self.advance_while(|ch0| ch0.is_ascii_digit() || ch0 == '_');

        Token::new(TokenKind::Integer, (offset, width))
    }

    fn lex_hex_num_lit(&mut self) -> Token {
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

    fn ch0(&self) -> Option<char> {
        self.input.get(self.pos).copied()
    }

    fn ch1(&self) -> Option<char> {
        self.input.get(self.pos + 1).copied()
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
}
