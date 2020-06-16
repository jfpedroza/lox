use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl Location {
    pub fn new(line: usize, column: usize) -> Self {
        Location { line, column }
    }

    pub fn advance(&mut self) {
        self.column += 1;
    }

    pub fn new_line(&mut self) {
        self.line += 1;
        self.column = 0;
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "line {}:{}", self.line, self.column)
    }
}
