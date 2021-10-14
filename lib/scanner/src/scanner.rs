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
        let pos = (self.col, self.line);

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

            return Token::float_const(res.into_iter().collect(), pos);
        }

        Token::int_const(res.into_iter().collect(), pos)
    }

    fn read_string(&mut self) -> Token {
        let mut res = vec![];
        let pos = (self.col, self.line);
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
            return Token::bad_token("Unterminated string".to_string(), pos);
        }

        // push last char
        res.push(self.ch);
        self.advance();

        Token::string_const(res.into_iter().collect(), pos)
    }

    fn read_identifier(&mut self) -> Token {
        let mut res = vec![];
        let pos = (self.col, self.line);
        while !self.is_end() && self.is_alphanumeric(self.ch) {
            res.push(self.ch);
            self.advance();
        }

        let value: String = res.into_iter().collect();

        match value.as_str() {
            "let" => Token::let_token(pos),
            "fun" => Token::fun_token(pos),
            "true" => Token::true_token(pos),
            "false" => Token::false_token(pos),
            "null" => Token::null(pos),
            _ => Token::identifier(value, pos),
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let curr_ch = self.ch;
        let pos = (self.col, self.line);
        let token = match curr_ch {
            '+' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::assign_plus(pos);
                }
                Token::plus(pos)
            }
            ':' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::assign_colon(pos);
                }
                Token::illegal(curr_ch.to_string(), pos)
            }
            '-' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::assign_minus(pos);
                }
                Token::minus(pos)
            }
            '*' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::assign_star(pos);
                }
                Token::star(pos)
            }
            '/' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::assign_slash(pos);
                }
                Token::slash(pos)
            }
            '=' => Token::assign(pos),
            '(' => Token::left_paren(pos),
            ')' => Token::right_paren(pos),
            '{' => Token::left_brace(pos),
            '}' => Token::right_brace(pos),
            '[' => Token::left_bracket(pos),
            ']' => Token::right_bracket(pos),
            ',' => Token::comma(pos),
            '\0' => Token::eof(pos),
            '"' => self.read_string(),
            _ => {
                if self.is_digit(curr_ch) {
                    return self.read_digit();
                } else if self.is_alpha(curr_ch) {
                    return self.read_identifier();
                } else {
                    Token::illegal(curr_ch.to_string(), pos)
                }
            }
        };

        self.advance();
        token
    }
}

#[cfg(test)]
mod tests {
    use crate::{token::Token, TokenType};

    use super::Scanner;

    fn scan(src: &str) -> Scanner {
        Scanner::new(src.to_string())
    }

    fn assert_token(token: Token, token_type: TokenType, value: Option<&str>) {
        assert_eq!(token.token_type, token_type);
        if let Some(val) = value {
            assert_eq!(token.value, val.to_string());
        }
    }

    #[test]
    fn arithmetic_tokens() {
        let mut res = scan("+-*/");
        assert_token(res.next_token(), TokenType::Plus, None);
        assert_token(res.next_token(), TokenType::Minus, None);
        assert_token(res.next_token(), TokenType::Star, None);
        assert_token(res.next_token(), TokenType::Slash, None);
        assert_token(res.next_token(), TokenType::EOF, None);
    }

    #[test]
    fn other_tokens() {
        let mut res = scan("{},()[]");
        assert_token(res.next_token(), TokenType::LeftBrace, None);
        assert_token(res.next_token(), TokenType::RightBrace, None);
        assert_token(res.next_token(), TokenType::Comma, None);
        assert_token(res.next_token(), TokenType::LeftParen, None);
        assert_token(res.next_token(), TokenType::RightParen, None);
        assert_token(res.next_token(), TokenType::LeftBracket, None);
        assert_token(res.next_token(), TokenType::RightBracket, None);
        assert_token(res.next_token(), TokenType::EOF, None);
    }

    #[test]
    fn illegal_tokens() {
        let mut res = scan("$@");
        assert_token(res.next_token(), TokenType::Illegal, Some("$"));
        assert_token(res.next_token(), TokenType::Illegal, Some("@"));
        assert_token(res.next_token(), TokenType::EOF, None);
    }

    #[test]
    fn skip_whitespace() {
        let mut res = scan("+ - * /");
        assert_token(res.next_token(), TokenType::Plus, None);
        assert_token(res.next_token(), TokenType::Minus, None);
        assert_token(res.next_token(), TokenType::Star, None);
        assert_token(res.next_token(), TokenType::Slash, None);
        assert_token(res.next_token(), TokenType::EOF, None);
    }

    #[test]
    fn line_pos() {
        let mut res = scan(
            "+
-
*
/",
        );
        assert_eq!(res.next_token(), Token::plus((0, 0)));
        assert_eq!(res.next_token(), Token::minus((0, 1)));
        assert_eq!(res.next_token(), Token::star((0, 2)));
        assert_eq!(res.next_token(), Token::slash((0, 3)));
        assert_eq!(res.next_token(), Token::eof((1, 3)));
    }

    #[test]
    fn read_digits() {
        let mut res = scan("123 23 34.2");

        assert_token(res.next_token(), TokenType::IntConst, Some("123"));
        assert_token(res.next_token(), TokenType::IntConst, Some("23"));
        assert_token(res.next_token(), TokenType::FloatConst, Some("34.2"));
        assert_token(res.next_token(), TokenType::EOF, None);
    }

    #[test]
    fn read_string() {
        let mut res = scan(r#""this is a string""#);

        assert_token(
            res.next_token(),
            TokenType::StringConst,
            Some("this is a string"),
        );
        assert_token(res.next_token(), TokenType::EOF, None);
    }

    #[test]
    fn unterminated_string() {
        let mut res = scan(r#""this is astring"#);

        assert_token(
            res.next_token(),
            TokenType::BadToken,
            Some("Unterminated string"),
        );
        assert_token(res.next_token(), TokenType::EOF, None);
    }

    #[test]
    fn read_identifier() {
        let mut res = scan("test test2 _test");

        assert_token(res.next_token(), TokenType::Identifier, Some("test"));
        assert_token(res.next_token(), TokenType::Identifier, Some("test2"));
        assert_token(res.next_token(), TokenType::Identifier, Some("_test"));
        assert_token(res.next_token(), TokenType::EOF, None);
    }

    #[test]
    fn keywords() {
        let mut res = scan("let fun true false null");

        assert_token(res.next_token(), TokenType::Let, None);
        assert_token(res.next_token(), TokenType::Fun, None);
        assert_token(res.next_token(), TokenType::True, None);
        assert_token(res.next_token(), TokenType::False, None);
        assert_token(res.next_token(), TokenType::Null, None);
        assert_token(res.next_token(), TokenType::EOF, None);
    }

    #[test]
    fn assignment() {
        let mut res = scan("= += -= *= /= :=");

        assert_token(res.next_token(), TokenType::Assign, None);
        assert_token(res.next_token(), TokenType::AssignPlus, None);
        assert_token(res.next_token(), TokenType::AssignMinus, None);
        assert_token(res.next_token(), TokenType::AssignStar, None);
        assert_token(res.next_token(), TokenType::AssignSlash, None);
        assert_token(res.next_token(), TokenType::AssignColon, None);
        assert_token(res.next_token(), TokenType::EOF, None);
    }
}
