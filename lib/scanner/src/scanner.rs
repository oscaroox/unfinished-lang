use crate::token::Token;
use spanner::{Span, SpanMaker};

#[derive(Debug)]
pub struct Scanner<'a> {
    source: Vec<char>,
    line: usize,
    pos: usize,
    ch: char,
    span_maker: &'a mut SpanMaker<'a>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: String, span_maker: &'a mut SpanMaker<'a>) -> Scanner<'a> {
        let mut scanner = Scanner {
            source: source.chars().collect(),
            line: 0,
            pos: 0,
            ch: '\0',
            span_maker,
        };

        scanner.ch = scanner.source[0];
        scanner
    }

    fn advance(&mut self) {
        self.pos += 1;
        if self.is_end() {
            self.ch = '\0';
            return;
        }

        self.ch = self.source[self.pos];
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

    pub fn span(&mut self, left: usize, right: usize) -> Span {
        self.span_maker.span(left, right)
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

        if self.ch == '.' {
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

            return Token::float_const(res.into_iter().collect(), self.span(pos, self.pos));
        }

        Token::int_const(res.into_iter().collect(), self.span(pos, self.pos))
    }

    fn read_string(&mut self) -> Token {
        let mut res = vec![];
        let pos = self.pos;
        self.advance();

        if self.ch == '"' {
            return Token::string_const("".to_string(), self.span(pos, self.pos));
        }

        while self.peek() != '"' && !self.is_end() {
            res.push(self.ch);
            if self.peek() == '\n' {
                self.line += 1;
                self.advance();
            } else {
                self.advance();
            }
        }

        if self.is_end() {
            return Token::bad_token("Unterminated string".to_string(), self.span(pos, self.pos));
        }

        // push last char
        res.push(self.ch);
        self.advance();

        Token::string_const(res.into_iter().collect(), self.span(pos, self.pos))
    }

    fn read_identifier(&mut self) -> Token {
        let mut res = vec![];
        let pos = self.pos;
        while !self.is_end() && self.is_alphanumeric(self.ch) {
            res.push(self.ch);
            self.advance();
        }

        let value: String = res.into_iter().collect();

        let span = self.span_maker.span(pos, self.pos);

        match value.as_str() {
            "let" => Token::let_token(span),
            "fun" => Token::fun_token(span),
            "if" => Token::if_token(span),
            "else" => Token::else_token(span),
            "true" => Token::true_token(span),
            "false" => Token::false_token(span),
            "return" => Token::return_token(span),
            "data" => Token::data(span),
            "null" => Token::null(span),
            _ => Token::identifier(value, span),
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let curr_ch = self.ch;
        let pos = self.pos;

        let token = match curr_ch {
            '+' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::assign_plus(self.span(pos, self.pos));
                }
                Token::plus(self.span(pos, self.pos))
            }
            ':' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::assign_colon(self.span(pos, self.pos));
                }
                Token::illegal(curr_ch.to_string(), self.span(pos, self.pos))
            }
            '-' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::assign_minus(self.span(pos, self.pos));
                }
                Token::minus(self.span(pos, self.pos))
            }
            '*' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::assign_star(self.span(pos, self.pos));
                }
                Token::star(self.span(pos, self.pos))
            }
            '/' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::assign_slash(self.span(pos, self.pos));
                }
                Token::slash(self.span(pos, self.pos))
            }
            '&' => {
                if self.peek() == '&' {
                    self.advance();
                    self.advance();
                    return Token::and(self.span(pos, self.pos));
                }
                Token::illegal(curr_ch.to_string(), self.span(pos, self.pos))
            }
            '|' => {
                if self.peek() == '|' {
                    self.advance();
                    self.advance();
                    return Token::or(self.span(pos, self.pos));
                }
                Token::illegal(curr_ch.to_string(), self.span(pos, self.pos))
            }
            '>' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::greater_than_equal(self.span(pos, self.pos));
                }
                Token::greater_than(self.span(pos, self.pos))
            }
            '<' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::less_than_equal(self.span(pos, self.pos));
                }
                Token::less_than(self.span(pos, self.pos))
            }
            '=' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::equal_equal(self.span(pos, self.pos));
                } else if self.peek() == '>' {
                    self.advance();
                    self.advance();
                    return Token::arrow(self.span(pos, self.pos));
                }
                Token::assign(self.span(pos, self.pos))
            }
            '!' => {
                if self.peek() == '=' {
                    self.advance();
                    self.advance();
                    return Token::not_equal(self.span(pos, self.pos));
                }
                Token::bang(self.span(pos, self.pos))
            }
            '(' => Token::left_paren(self.span(pos, self.pos)),
            ')' => Token::right_paren(self.span(pos, self.pos)),
            '{' => Token::left_brace(self.span(pos, self.pos)),
            '}' => Token::right_brace(self.span(pos, self.pos)),
            '[' => Token::left_bracket(self.span(pos, self.pos)),
            ']' => Token::right_bracket(self.span(pos, self.pos)),
            ',' => Token::comma(self.span(pos, self.pos)),
            ';' => Token::semi_colon(self.span(pos, self.pos)),
            '\0' => Token::eof(self.span(pos, self.pos)),
            '"' => self.read_string(),
            _ => {
                if self.is_digit(curr_ch) {
                    return self.read_digit();
                } else if self.is_alpha(curr_ch) {
                    return self.read_identifier();
                } else {
                    Token::illegal(curr_ch.to_string(), self.span(pos, self.pos))
                }
            }
        };

        self.advance();
        token
    }
}

#[cfg(test)]
mod tests {
    use super::Scanner;
    use crate::TokenType;
    use spanner::SpanManager;

    fn test_scan(src: &str, expected: Vec<(TokenType, Option<&str>)>) {
        let mut manager = SpanManager::default();
        let mut maker = manager.add_source(src.to_string());
        let mut scanner = Scanner::new(src.to_string(), &mut maker);
        let mut spans = vec![];
        expected.into_iter().for_each(|e| {
            let token_type = e.0;
            let value = e.1;
            let token = scanner.next_token();
            spans.push(token.span);
            assert_eq!(token.token_type, token_type);
            if let Some(val) = value {
                assert_eq!(val.to_string(), token.value);
            }
        });
    }

    #[test]
    fn arithmetic_tokens() {
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
    fn eof() {
        test_scan(
            "
            
            ",
            vec![(TokenType::EOF, None)],
        );
    }

    #[test]
    fn other_tokens() {
        test_scan(
            "{},;()[]! =>",
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
                (TokenType::EOF, None),
            ],
        );
    }

    #[test]
    fn illegal_tokens() {
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
    fn skip_whitespace() {
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
    fn span() {
        let src = "+
let
fun
this_is_a_identifier";
        let mut manager = SpanManager::default();
        let mut maker = manager.add_source(src.to_string());
        let mut scanner = Scanner::new(src.to_string(), &mut maker);

        let plus = scanner.next_token();
        let let_kw = scanner.next_token();
        let fun_kw = scanner.next_token();
        let ident = scanner.next_token();

        let (_, l, r) = manager.resolve_span(plus.span);
        assert_eq!(l, 0);
        assert_eq!(r, 0);
        let (_, l, r) = manager.resolve_span(let_kw.span);
        assert_eq!(l, 2);
        assert_eq!(r, 5);
        let (_, l, r) = manager.resolve_span(fun_kw.span);
        assert_eq!(l, 6);
        assert_eq!(r, 9);
        let (_, l, r) = manager.resolve_span(ident.span);
        assert_eq!(l, 10);
        assert_eq!(r, 30);
    }

    #[test]
    fn read_digits() {
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
    fn newline() {
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
    fn read_string() {
        test_scan(
            r#""this is a string" "";"#,
            vec![
                (TokenType::StringConst, Some("this is a string")),
                (TokenType::StringConst, Some("")),
                (TokenType::SemiColon, None),
                (TokenType::EOF, None),
            ],
        );
    }

    #[test]
    fn unterminated_string() {
        test_scan(
            r#""this is astring"#,
            vec![
                (TokenType::BadToken, Some("Unterminated string")),
                (TokenType::EOF, None),
            ],
        );
    }

    #[test]
    fn read_identifier() {
        let alpha = "abcdefghijklmnopqrstuvwxyz";
        let capital_alpha = alpha.to_uppercase();

        test_scan(
            format!("test test2 _test {} {}", alpha, capital_alpha).as_str(),
            vec![
                (TokenType::Identifier, Some("test")),
                (TokenType::Identifier, Some("test2")),
                (TokenType::Identifier, Some("_test")),
                (TokenType::Identifier, Some("abcdefghijklmnopqrstuvwxyz")),
                (TokenType::Identifier, Some("ABCDEFGHIJKLMNOPQRSTUVWXYZ")),
                (TokenType::EOF, None),
            ],
        );
    }

    #[test]
    fn logic_tokens() {
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
    fn keywords() {
        test_scan(
            "let fun true false null if else return data",
            vec![
                (TokenType::Let, None),
                (TokenType::Fun, None),
                (TokenType::True, None),
                (TokenType::False, None),
                (TokenType::Null, None),
                (TokenType::If, None),
                (TokenType::Else, None),
                (TokenType::Return, None),
                (TokenType::Data, None),
                (TokenType::EOF, None),
            ],
        );
    }

    #[test]
    fn assignment() {
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
