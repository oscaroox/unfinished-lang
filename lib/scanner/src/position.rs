#[derive(Debug, PartialEq)]
pub struct Position {
    col: isize,
    line: isize,
}

impl Position {
    pub fn new(col: isize, line: isize) -> Position {
        Position { col, line }
    }
}
