use super::{Token, TokenType};

#[derive(Debug)]
pub enum ScannerMode {
    Default,
    String,
}

#[derive(Debug)]
pub struct Scanner {
    source: Vec<char>,
    mode: ScannerMode,
    line: usize,
    pos: usize,
    ch: char,
    checkpoint: usize,
}

impl Scanner {
    pub fn new(source: String) -> Scanner {
        let mut scanner = Scanner {
            source: source.chars().collect(),
            mode: ScannerMode::Default,
            line: 0,
            pos: 0,
            ch: '\0',
            checkpoint: 0,
        };

        scanner.ch = scanner.source.get(0).unwrap_or(&scanner.ch).to_owned();
        scanner
    }

    pub fn set_mode(&mut self, mode: ScannerMode) {
        self.mode = mode;
    }

    pub fn set_checkpoint(&mut self) {
        self.checkpoint = self.pos;
    }

    fn advance(&mut self) {
        self.pos += 1;
        if self.is_end() {
            self.ch = '\0';
            return;
        }

        self.ch = self.source[self.pos];
    }

    /**
     * backtrack the scanner to the checkpoint position
     * checkpoint is set before a token is generated
     * backtracking is possible by only one token
     */
    pub fn backtrack(&mut self) {
        if self.checkpoint > 0 && self.checkpoint < self.source.len() {
            self.pos = self.checkpoint;
            self.ch = self.source[self.pos];
        }
    }

    fn is_end(&self) -> bool {
        self.pos >= self.source.len()
    }

    fn is_digit(&self, ch: char) -> bool {
        "0123456789".contains(ch)
    }

    fn is_alpha(&self, ch: char) -> bool {
        ch == '_' || ('a'..='z').contains(&ch) || ('A'..='Z').contains(&ch)
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
        let pos = self.pos + 1;
        if pos >= self.source.len() {
            return '\0';
        }
        self.source[pos]
    }

    fn skip_whitespace(&mut self) {
        while !self.is_end() && self.is_whitespace(self.ch) || self.is_newline(self.ch) {
            if self.is_newline(self.ch) {
                self.line += 1;
            }
            self.advance();
        }
    }

    fn read_digit(&mut self) -> Token {
        let mut res = vec![];
        let pos = self.pos;
        while !self.is_end() && self.is_digit(self.ch) {
            res.push(self.ch);
            self.advance();
        }

        match self.ch {
            '.' => {
                res.push(self.ch);
                self.advance();

                while !self.is_end() && self.is_digit(self.ch) {
                    res.push(self.ch);
                    self.advance();
                }
                if let Some(last) = res.last() {
                    if *last == '.' {
                        res.push('0')
                    }
                }
                Token::float_const(res.into_iter().collect(), pos..self.pos)
            }
            _ => Token::int_const(res.into_iter().collect(), pos..self.pos),
        }
    }

    fn read_identifier(&mut self) -> Token {
        let mut res = vec![];
        let pos = self.pos;
        while !self.is_end() && self.is_alphanumeric(self.ch) {
            res.push(self.ch);
            self.advance();
        }

        let value: String = res.into_iter().collect();
        let label = pos..self.pos;

        match value.as_str() {
            "let" => Token::let_token(label),
            "fn" => Token::fn_token(label),
            "if" => Token::if_token(label),
            "else" => Token::else_token(label),
            "data" => Token::data(label),
            "null" => Token::null(label),
            "self" => Token::self_token(label),
            "loop" => Token::loop_token(label),
            "while" => Token::while_token(label),
            "for" => Token::for_token(label),
            "in" => Token::in_token(label),
            "true" => Token::true_token(label),
            "false" => Token::false_token(label),
            "return" => Token::return_token(label),
            "break" => Token::break_token(label),
            "continue" => Token::continue_token(label),
            "int" => Token::int(label),
            "float" => Token::float(label),
            "bool" => Token::bool(label),
            "string" => Token::string(label),
            "unit" => Token::unit(label),
            _ => Token::identifier(value, label),
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.set_checkpoint();
        match self.mode {
            ScannerMode::Default => self.scan_normal(),
            ScannerMode::String => self.scan_interpolated(),
        }
    }

    fn scan_normal(&mut self) -> Token {

        self.skip_whitespace();

        let curr_ch = self.ch;
        let pos = self.pos;

        let token = match curr_ch {
            '+' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::assign_plus(pos..self.pos);
                }
                Token::plus(pos..self.pos)
            }
            ':' => {
                let peek = self.peek();
                if peek == '=' || peek == ':' {
                    self.advance();
                    self.advance();
                    if peek == '=' {
                        return Token::assign_colon(pos..self.pos);
                    } else {
                        return Token::colon_colon(pos..self.pos);
                    }
                }
                Token::colon(pos..self.pos)
            }
            '-' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::assign_minus(pos..self.pos);
                }
                Token::minus(pos..self.pos)
            }
            '*' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::assign_star(pos..self.pos);
                }
                Token::star(pos..self.pos)
            }
            '/' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::assign_slash(pos..self.pos);
                } else if self.peek() == '/' {
                    while self.peek() != '\n' && !self.is_end() {
                        self.advance();
                    }
                    self.advance();
                    return self.next_token();
                } else if self.peek() == '*' {
                    while !self.is_end() {
                        self.advance();
                        if self.ch == '*' && self.peek() == '/' {
                            break;
                        }
                    }
                    // remove last slash when out of loop
                    self.advance();
                    self.advance();
                    return self.next_token();
                }
                Token::slash(pos..self.pos)
            }
            '&' => {
                if self.peek() == '&' {
                    self.advance();
                    self.advance();
                    return Token::and(pos..self.pos);
                }
                Token::illegal(curr_ch.to_string(), pos..self.pos)
            }
            '|' => {
                if self.peek() == '|' {
                    self.advance();
                    self.advance();
                    return Token::or(pos..self.pos);
                }
                Token::pipe(pos..self.pos)
            }
            '>' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::greater_than_equal(pos..self.pos);
                }
                Token::greater_than(pos..self.pos)
            }
            '<' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::less_than_equal(pos..self.pos);
                }
                Token::less_than(pos..self.pos)
            }
            '=' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::equal_equal(pos..self.pos);
                } else if self.peek() == '>' {
                    self.advance();
                    self.advance();
                    return Token::arrow(pos..self.pos);
                }
                Token::assign(pos..self.pos)
            }
            '!' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::not_equal(pos..self.pos);
                }
                Token::bang(pos..self.pos)
            }
            '.' => Token::dot(pos..self.pos),
            '(' => Token::left_paren(pos..self.pos),
            ')' => Token::right_paren(pos..self.pos),
            '{' => Token::left_brace(pos..self.pos),
            '}' => Token::right_brace(pos..self.pos),
            '[' => Token::left_bracket(pos..self.pos),
            ']' => Token::right_bracket(pos..self.pos),
            ',' => Token::comma(pos..self.pos),
            ';' => Token::semi_colon(pos..self.pos),
            '\0' => Token::eof(pos..self.pos),
            '"' => Token::double_quote(pos..self.pos),
            _ => {
                let token = if self.is_digit(curr_ch) {
                    self.read_digit()
                } else if self.is_alpha(curr_ch) {
                    self.read_identifier()
                } else {
                    Token::illegal(curr_ch.to_string(), pos..self.pos)
                };

                match token.token_type {
                    TokenType::Illegal => token,
                    _ => {
                        return token;
                    }
                }
            }
        };

        self.advance();
        token
    }

    fn scan_interpolated(&mut self) -> Token {
        let pos = self.pos;
        let mut token = match self.ch {
            '"' => Token::double_quote(pos..self.pos),
            '\0' => Token::eof(pos..self.pos),
            '$' => match self.peek() {
                '$' => {
                    self.advance();
                    self.read_interpolation(self.ch)
                }
                '(' => {
                    self.advance();
                    Token::left_paren(pos..self.pos)
                }
                _ => self.read_interpolation(self.ch),
            },
            '\\' => {
                self.advance();
                self.read_interpolation(self.normalize_escape(self.ch))
            }
            _ => self.read_interpolation(self.ch),
        };
        self.advance();
        token.replace_span((pos..self.pos).into());
        token
    }

    fn read_interpolation(&mut self, first_ch: char) -> Token {
        let mut out = vec![first_ch];
        let pos = self.pos;
        loop {
            match self.peek() {
                '"' => break,
                '$' => break,
                '\0' => break,
                '\\' => {
                    self.advance();
                    self.advance();
                    out.push(self.normalize_escape(self.ch));
                }
                _ => {
                    out.push(self.peek());
                    self.advance();
                }
            }
        }
        Token::string_const(out.into_iter().collect(), pos..self.pos)
    }

    fn normalize_escape(&self, ch: char) -> char {
        match ch {
            '\\' => '\\',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            _ => self.ch,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::scanner::{ScannerMode, TokenType};

    use super::Scanner;

    fn test_scan(src: &str, expected: Vec<(TokenType, Option<&str>)>) {
        let mut scanner = Scanner::new(src.to_string());

        expected.into_iter().for_each(|e| {
            let token_type = e.0;
            let value = e.1;
            let token = scanner.next_token();

            assert_eq!(token.token_type, token_type);
            if let Some(val) = value {
                assert_eq!(token.value, val.to_string());
            }
        });
    }

    #[test]
    fn scan_empty() {
        test_scan("", vec![(TokenType::EOF, None)]);
        test_scan(" ", vec![(TokenType::EOF, None)]);
    }

    #[test]
    fn scan_with_backtracking() {
        let src = "let fn hello;";
        let mut scanner = Scanner::new(src.to_string());

        let token = scanner.next_token();
        assert_eq!(token.token_type, TokenType::Let);

        let token = scanner.next_token();
        assert_eq!(token.token_type, TokenType::Fn);

        scanner.backtrack();

        let token = scanner.next_token();
        assert_eq!(token.token_type, TokenType::Fn);

        let token = scanner.next_token();
        assert_eq!(token.token_type, TokenType::Identifier);
        assert_eq!(token.value, "hello".to_string());

        let token = scanner.next_token();
        assert_eq!(token.token_type, TokenType::SemiColon);

        scanner.backtrack();

        let token = scanner.next_token();
        assert_eq!(token.token_type, TokenType::SemiColon);

        let token = scanner.next_token();
        assert_eq!(token.token_type, TokenType::EOF);
    }

    #[test]
    fn scan_string_interpolation() {
        let src = r#" "hello, $(world) hows it" "" 123"#;
        let mut scanner = Scanner::new(src.to_string());

        let tok = scanner.next_token();
        assert_eq!(tok.token_type, TokenType::DoubleQuote);

        if tok.token_type == TokenType::DoubleQuote {
            scanner.set_mode(ScannerMode::String);
        }

        let tok = scanner.next_token();
        assert_eq!(tok.token_type, TokenType::StringConst);
        assert_eq!(tok.value, String::from("hello, "));

        let tok = scanner.next_token();

        assert_eq!(tok.token_type, TokenType::LeftParen);

        scanner.set_mode(ScannerMode::Default);

        let tok = scanner.next_token();
        assert_eq!(tok.token_type, TokenType::Identifier);

        // skip the closing paren
        scanner.next_token();

        scanner.set_mode(ScannerMode::String);

        let tok = scanner.next_token();
        assert_eq!(tok.token_type, TokenType::StringConst);
        assert_eq!(tok.value, String::from(" hows it"));

        let tok = scanner.next_token();
        assert_eq!(tok.token_type, TokenType::DoubleQuote);

        if tok.token_type == TokenType::DoubleQuote {
            scanner.set_mode(ScannerMode::Default);
        }

        let tok = scanner.next_token();
        assert_eq!(tok.token_type, TokenType::DoubleQuote);

        if tok.token_type == TokenType::DoubleQuote {
            scanner.set_mode(ScannerMode::String);
        }

        let tok = scanner.next_token();
        assert_eq!(tok.token_type, TokenType::DoubleQuote);

        if tok.token_type == TokenType::DoubleQuote {
            scanner.set_mode(ScannerMode::Default);
        }

        let tok = scanner.next_token();
        assert_eq!(tok.token_type, TokenType::IntConst);
        assert_eq!(tok.value, "123");
    }

    #[test]
    fn scan_arithmetic_tokens() {
        test_scan(
            "+-*/",
            vec![
                (TokenType::Plus, None),
                (TokenType::Minus, None),
                (TokenType::Star, None),
                (TokenType::Slash, None),
                (TokenType::EOF, None),
            ],
        );
    }

    #[test]
    fn scan_eof() {
        test_scan(
            "
            
            ",
            vec![(TokenType::EOF, None)],
        );
    }

    #[test]
    fn scan_other_tokens() {
        test_scan(
            "{},;()[]! =>:.::|",
            vec![
                (TokenType::LeftBrace, None),
                (TokenType::RightBrace, None),
                (TokenType::Comma, None),
                (TokenType::SemiColon, None),
                (TokenType::LeftParen, None),
                (TokenType::RightParen, None),
                (TokenType::LeftBracket, None),
                (TokenType::RightBracket, None),
                (TokenType::Bang, None),
                (TokenType::Arrow, None),
                (TokenType::Colon, None),
                (TokenType::Dot, None),
                (TokenType::ColonColon, None),
                (TokenType::Pipe, None),
                (TokenType::EOF, None),
            ],
        );
    }

    #[test]
    fn scan_illegal_tokens() {
        test_scan(
            "$@",
            vec![
                (TokenType::Illegal, Some("$")),
                (TokenType::Illegal, Some("@")),
                (TokenType::EOF, None),
            ],
        );
    }

    #[test]
    fn scan_skip_whitespace() {
        test_scan(
            "+ - * /",
            vec![
                (TokenType::Plus, None),
                (TokenType::Minus, None),
                (TokenType::Star, None),
                (TokenType::Slash, None),
                (TokenType::EOF, None),
            ],
        );
    }

    #[test]
    fn scan_comments() {
        test_scan(
            "
        // this is a comment
        // let test = 123;
        ",
            vec![(TokenType::EOF, None)],
        );

        test_scan(
            "
        /**
         * This is a multiline comment
         */

        ",
            vec![(TokenType::EOF, None)],
        )
    }

    #[test]
    fn scan_label() {
        let src = r#"+ let fun this_is_a_identifier 1"#;
        let mut scanner = Scanner::new(src.to_string());
        let plus = scanner.next_token();
        let let_kw = scanner.next_token();
        let fun_kw = scanner.next_token();
        let ident = scanner.next_token();
        let int = scanner.next_token();

        assert_eq!(plus.span.to_range(), 0..0);
        assert_eq!(let_kw.span.to_range(), 2..5);
        assert_eq!(fun_kw.span.to_range(), 6..9);
        assert_eq!(ident.span.to_range(), 10..30);
        assert_eq!(int.span.to_range(), 31..32);
    }

    #[test]
    fn scan_digits() {
        test_scan(
            "123 23 34.2 1 + -1 1234567890",
            vec![
                (TokenType::IntConst, Some("123")),
                (TokenType::IntConst, Some("23")),
                (TokenType::FloatConst, Some("34.2")),
                (TokenType::IntConst, Some("1")),
                (TokenType::Plus, Some("+")),
                (TokenType::Minus, Some("-")),
                (TokenType::IntConst, Some("1")),
                (TokenType::IntConst, Some("1234567890")),
                (TokenType::EOF, None),
            ],
        );
    }

    #[test]
    fn scan_newline() {
        test_scan(
            "2;
            0;
            29;
            34;
            4.15;
            1;
            2.;
            0.;",
            vec![
                (TokenType::IntConst, Some("2")),
                (TokenType::SemiColon, None),
                (TokenType::IntConst, Some("0")),
                (TokenType::SemiColon, None),
                (TokenType::IntConst, Some("29")),
                (TokenType::SemiColon, None),
                (TokenType::IntConst, Some("34")),
                (TokenType::SemiColon, None),
                (TokenType::FloatConst, Some("4.15")),
                (TokenType::SemiColon, None),
                (TokenType::IntConst, Some("1")),
                (TokenType::SemiColon, None),
                (TokenType::FloatConst, Some("2.0")),
                (TokenType::SemiColon, None),
                (TokenType::FloatConst, Some("0.0")),
                (TokenType::SemiColon, None),
                (TokenType::EOF, None),
            ],
        );
    }

    #[test]
    fn scan_identifiers() {
        let alpha = "abcdefghijklmnopqrstuvwxyz";
        let capital_alpha = alpha.to_uppercase();

        test_scan(
            format!("test; test2 _test {} {}", alpha, capital_alpha).as_str(),
            vec![
                (TokenType::Identifier, Some("test")),
                (TokenType::SemiColon, None),
                (TokenType::Identifier, Some("test2")),
                (TokenType::Identifier, Some("_test")),
                (TokenType::Identifier, Some("abcdefghijklmnopqrstuvwxyz")),
                (TokenType::Identifier, Some("ABCDEFGHIJKLMNOPQRSTUVWXYZ")),
                (TokenType::EOF, None),
            ],
        );
    }

    #[test]
    fn scan_logic_tokens() {
        test_scan(
            "&& || < <= > >= == !=",
            vec![
                (TokenType::And, None),
                (TokenType::Or, None),
                (TokenType::LessThan, None),
                (TokenType::LessThanEqual, None),
                (TokenType::GreaterThan, None),
                (TokenType::GreaterThanEqual, None),
                (TokenType::EqualEqual, None),
                (TokenType::NotEqual, None),
                (TokenType::EOF, None),
            ],
        )
    }

    #[test]
    fn scan_keywords() {
        test_scan(
            "let fn true false null if else return data self loop for while break continue in",
            vec![
                (TokenType::Let, None),
                (TokenType::Fn, None),
                (TokenType::True, None),
                (TokenType::False, None),
                (TokenType::Null, None),
                (TokenType::If, None),
                (TokenType::Else, None),
                (TokenType::Return, None),
                (TokenType::Data, None),
                (TokenType::SELF, None),
                (TokenType::Loop, None),
                (TokenType::For, None),
                (TokenType::While, None),
                (TokenType::Break, None),
                (TokenType::Continue, None),
                (TokenType::In, None),
                (TokenType::EOF, None),
            ],
        );
    }

    #[test]
    fn scan_types() {
        test_scan(
            "
            int float string bool unit fn
            ",
            vec![
                (TokenType::Int, None),
                (TokenType::Float, None),
                (TokenType::String, None),
                (TokenType::Bool, None),
                (TokenType::Unit, None),
                (TokenType::Fn, None),
                (TokenType::EOF, None),
            ],
        );
    }

    #[test]
    fn scan_assignment_tokens() {
        test_scan(
            "= += -= *= /= :=",
            vec![
                (TokenType::Assign, None),
                (TokenType::AssignPlus, None),
                (TokenType::AssignMinus, None),
                (TokenType::AssignStar, None),
                (TokenType::AssignSlash, None),
                (TokenType::AssignColon, None),
                (TokenType::EOF, None),
            ],
        );
    }
}
