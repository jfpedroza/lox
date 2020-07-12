use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
pub struct Loc {
    pub line: usize,
    pub column: usize,
}

impl Loc {
    pub fn new(line: usize, column: usize) -> Self {
        Loc { line, column }
    }

    pub fn advance(&mut self) {
        self.column += 1;
    }

    pub fn new_line(&mut self) {
        self.line += 1;
        self.column = 0;
    }
}

impl Display for Loc {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}:{}", self.line + 1, self.column)
    }
}

#[derive(PartialEq)]
pub struct Located<T: PartialEq> {
    pub kind: T,
    pub loc: Loc,
}

impl<T: PartialEq> Located<T> {
    pub fn new(kind: T, loc: Loc) -> Self {
        Self { kind, loc }
    }
}

impl<T: PartialEq + Debug> Debug for Located<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{:?}[{}]", self.kind, self.loc)
    }
}

impl<T: PartialEq + Default> Default for Located<T> {
    fn default() -> Self {
        Self {
            kind: Default::default(),
            loc: Default::default(),
        }
    }
}
