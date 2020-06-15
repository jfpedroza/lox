use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl Location {
    pub fn new(line: usize, column: usize) -> Self {
        Location { line, column }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "line {}:{}", self.line, self.column)
    }
}
