#[derive(Debug, PartialEq, Clone)]
pub struct Position {
    col: isize,
    line: isize,
}

impl Position {
    pub fn new(col: isize, line: isize) -> Position {
        Position { col, line }
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line: {}, col: {}", self.line, self.col)
    }
}
