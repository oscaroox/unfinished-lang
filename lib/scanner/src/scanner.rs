use crate::{position::Position, token::Token};

#[derive(Debug)]
pub struct Scanner {
    source: Vec<char>,
    line: isize,
    col: isize,
    pos: isize,
    ch: char,
}

impl Scanner {
    pub fn new(source: String) -> Scanner {
        let mut scanner = Scanner {
            source: source.chars().collect(),
            line: 0,
            col: 0,
            pos: 0,
            ch: '\0',
        };

        scanner.ch = scanner.source[0];
        scanner
    }

    fn advance(&mut self) {
        self.pos += 1;
        self.col += 1;
        if self.is_end() {
            self.ch = '\0';
            return;
        }

        self.ch = self.source[self.pos as usize];
    }

    fn is_end(&self) -> bool {
        self.pos >= self.source.len() as isize
    }

    fn is_digit(&self, ch: char) -> bool {
        ('0'..'9').contains(&ch)
    }

    fn is_alpha(&self, ch: char) -> bool {
        ch == '_' || ('a'..'z').contains(&ch) || ('A'..'Z').contains(&ch)
    }

    fn is_alphanumeric(&self, ch: char) -> bool {
        self.is_digit(ch) || self.is_alpha(ch)
    }

    fn is_newline(&self, ch: char) -> bool {
        ch == '\n' || ch == '\r'
    }

    fn is_whitespace(&self, ch: char) -> bool {
        ch == ' ' || ch == '\t'
    }

    fn peek(&self) -> char {
        let pos = (self.pos + 1) as usize;
        if pos >= self.source.len() {
            return '\0';
        }
        self.source[pos]
    }

    fn skip_whitespace(&mut self) {
        while !self.is_end() && self.is_whitespace(self.ch) || self.is_newline(self.ch) {
            if self.is_newline(self.ch) {
                // TODO: shouldnt the col be set back to 0?
                self.line += 1;
                self.advance();
                self.col = 0;
            } else {
                self.advance();
            }
        }
    }

    fn read_digit(&mut self) -> Token {
        let mut res = vec![];
        let pos = Position::new(self.col, self.line);

        while !self.is_end() && self.is_digit(self.ch) {
            res.push(self.ch);
            self.advance();
        }

        if self.ch == '.' {
            res.push(self.ch);
            self.advance();
            while !self.is_end() && self.is_digit(self.ch) {
                res.push(self.ch);
                self.advance();
            }

            return Token::FloatConst(res.into_iter().collect(), pos);
        }

        Token::IntConst(res.into_iter().collect(), pos)
    }

    fn read_string(&mut self) -> Token {
        let mut res = vec![];
        let pos = Position::new(self.col, self.line);
        self.advance();

        while self.peek() != '"' && !self.is_end() {
            res.push(self.ch);
            if self.peek() == '\n' {
                self.line += 1;
                self.advance();
                self.col = 0;
            } else {
                self.advance();
            }
        }

        if self.is_end() {
            return Token::BadToken("Unterminated string".to_string(), pos);
        }

        // push last char
        res.push(self.ch);
        self.advance();

        Token::StringConst(res.into_iter().collect(), pos)
    }

    fn read_identifier(&mut self) -> Token {
        let mut res = vec![];
        let pos = Position::new(self.col, self.line);
        while !self.is_end() && self.is_alphanumeric(self.ch) {
            res.push(self.ch);
            self.advance();
        }

        let value: String = res.into_iter().collect();

        match value.as_str() {
            "let" => Token::Let(pos),
            "fun" => Token::Fun(pos),
            "true" => Token::True(pos),
            "false" => Token::False(pos),
            "null" => Token::Null(pos),
            _ => Token::Identifier(value, pos),
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let curr_ch = self.ch;
        let pos = Position::new(self.col, self.line);
        let token = match curr_ch {
            '+' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::AssignPlus(pos);
                }
                Token::Plus(pos)
            }
            ':' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::AssignColon(pos);
                }
                Token::Illegal(curr_ch.to_string(), pos)
            }
            '-' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::AssignMinus(pos);
                }
                Token::Minus(pos)
            }
            '*' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::AssignStar(pos);
                }
                Token::Star(pos)
            }
            '/' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::AssignSlash(pos);
                }
                Token::Slash(pos)
            }
            '=' => Token::Assign(pos),
            '(' => Token::LeftParen(pos),
            ')' => Token::RightParen(pos),
            '{' => Token::LeftBrace(pos),
            '}' => Token::RightBrace(pos),
            '[' => Token::LeftBracket(pos),
            ']' => Token::RightBracket(pos),
            ',' => Token::Comma(pos),
            '\0' => Token::EOF(pos),
            '"' => self.read_string(),
            _ => {
                if self.is_digit(curr_ch) {
                    return self.read_digit();
                } else if self.is_alpha(curr_ch) {
                    return self.read_identifier();
                } else {
                    Token::Illegal(curr_ch.to_string(), pos)
                }
            }
        };

        self.advance();
        token
    }
}

#[cfg(test)]
mod tests {
    use crate::token::Token;

    use super::Scanner;

    fn scan(src: &str) -> Scanner {
        Scanner::new(src.to_string())
    }

    #[test]
    fn arithmetic_tokens() {
        let mut res = scan("+-*/");
        assert_eq!(res.next_token(), Token::plus(0, 0));
        assert_eq!(res.next_token(), Token::minus(1, 0));
        assert_eq!(res.next_token(), Token::star(2, 0));
        assert_eq!(res.next_token(), Token::slash(3, 0));
        assert_eq!(res.next_token(), Token::eof(4, 0));
    }

    #[test]
    fn other_tokens() {
        let mut res = scan("{},()[]");
        assert_eq!(res.next_token(), Token::left_brace(0, 0));
        assert_eq!(res.next_token(), Token::right_brace(1, 0));
        assert_eq!(res.next_token(), Token::comma(2, 0));
        assert_eq!(res.next_token(), Token::left_paren(3, 0));
        assert_eq!(res.next_token(), Token::right_paren(4, 0));
        assert_eq!(res.next_token(), Token::left_bracket(5, 0));
        assert_eq!(res.next_token(), Token::right_bracket(6, 0));
        assert_eq!(res.next_token(), Token::eof(7, 0));
    }

    #[test]
    fn illegal_tokens() {
        let mut res = scan("$@");
        assert_eq!(res.next_token(), Token::illegal("$".to_string(), 0, 0));
        assert_eq!(res.next_token(), Token::illegal("@".to_string(), 1, 0));
        assert_eq!(res.next_token(), Token::eof(2, 0));
    }

    #[test]
    fn skip_whitespace() {
        let mut res = scan("+ - * /");

        assert_eq!(res.next_token(), Token::plus(0, 0));
        assert_eq!(res.next_token(), Token::minus(2, 0));
        assert_eq!(res.next_token(), Token::star(4, 0));
        assert_eq!(res.next_token(), Token::slash(6, 0));
        assert_eq!(res.next_token(), Token::eof(7, 0));
    }

    #[test]
    fn line_pos() {
        let mut res = scan(
            "+
-
*
/",
        );
        assert_eq!(res.next_token(), Token::plus(0, 0));
        assert_eq!(res.next_token(), Token::minus(0, 1));
        assert_eq!(res.next_token(), Token::star(0, 2));
        assert_eq!(res.next_token(), Token::slash(0, 3));
        assert_eq!(res.next_token(), Token::eof(1, 3));
    }

    #[test]
    fn read_digits() {
        let mut res = scan("123 23 34.2");

        assert_eq!(res.next_token(), Token::int_const("123".to_string(), 0, 0));
        assert_eq!(res.next_token(), Token::int_const("23".to_string(), 4, 0));
        assert_eq!(
            res.next_token(),
            Token::float_const("34.2".to_string(), 7, 0)
        );
        assert_eq!(res.next_token(), Token::eof(11, 0));
    }

    #[test]
    fn read_string() {
        let mut res = scan(r#""this is a string""#);

        assert_eq!(
            res.next_token(),
            Token::string_const("this is a string".to_string(), 0, 0)
        );
        assert_eq!(res.next_token(), Token::eof(18, 0));
    }

    #[test]
    fn unterminated_string() {
        let mut res = scan(r#""this is astring"#);

        assert_eq!(
            res.next_token(),
            Token::bad_token("Unterminated string".to_string(), 0, 0)
        );
        assert_eq!(res.next_token(), Token::eof(17, 0));
    }

    #[test]
    fn read_identifier() {
        let mut res = scan("test test2 _test");

        assert_eq!(
            res.next_token(),
            Token::identifier("test".to_string(), 0, 0)
        );
        assert_eq!(
            res.next_token(),
            Token::identifier("test2".to_string(), 5, 0)
        );

        assert_eq!(
            res.next_token(),
            Token::identifier("_test".to_string(), 11, 0)
        );
        assert_eq!(res.next_token(), Token::eof(16, 0));
    }

    #[test]
    fn keywords() {
        let mut res = scan("let fun true false null");

        assert_eq!(res.next_token(), Token::let_token(0, 0));
        assert_eq!(res.next_token(), Token::fun_token(4, 0));
        assert_eq!(res.next_token(), Token::true_token(8, 0));
        assert_eq!(res.next_token(), Token::false_token(13, 0));
        assert_eq!(res.next_token(), Token::null(19, 0));
        assert_eq!(res.next_token(), Token::eof(23, 0));
    }

    #[test]
    fn assignment() {
        let mut res = scan("= += -= *= /= :=");

        assert_eq!(res.next_token(), Token::assign(0, 0));
        assert_eq!(res.next_token(), Token::assign_plus(2, 0));
        assert_eq!(res.next_token(), Token::assign_minus(5, 0));
        assert_eq!(res.next_token(), Token::assign_star(8, 0));
        assert_eq!(res.next_token(), Token::assign_slash(11, 0));
        assert_eq!(res.next_token(), Token::assign_colon(14, 0));
        assert_eq!(res.next_token(), Token::eof(16, 0));
    }
}
