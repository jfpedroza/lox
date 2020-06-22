use crate::lexer::{Literal, TokenKind};
use crate::location::{Located, Location};
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

#[derive(PartialEq)]
pub enum ExprKind {
    Literal(LiteralExpr),
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Grouping(Box<Expr>),
    Comma(Box<Expr>, Box<Expr>),
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),
}

pub type Expr = Located<ExprKind>;

impl Expr {
    pub fn from_literal(literal: &Literal, loc: Location) -> Self {
        use Literal::*;
        let kind = ExprKind::Literal(match literal {
            Integer(int) => LiteralExpr::Integer(*int),
            Float(float) => LiteralExpr::Float(*float),
            Str(string) => LiteralExpr::Str(string.clone()),
        });

        Expr::new(kind, loc)
    }

    pub fn boolean(boolean: bool, loc: Location) -> Self {
        Expr::new(ExprKind::Literal(LiteralExpr::Boolean(boolean)), loc)
    }

    pub fn nil(loc: Location) -> Self {
        Expr::new(ExprKind::Literal(LiteralExpr::Nil), loc)
    }

    pub fn unary(op: UnaryOp, right: Expr, loc: Location) -> Self {
        Expr::new(ExprKind::Unary(op, Box::new(right)), loc)
    }

    pub fn binary(left: Expr, op: BinaryOp, right: Expr, loc: Location) -> Self {
        Expr::new(ExprKind::Binary(Box::new(left), op, Box::new(right)), loc)
    }

    pub fn groping(expr: Expr, loc: Location) -> Self {
        Expr::new(ExprKind::Grouping(Box::new(expr)), loc)
    }

    pub fn comma(left: Expr, right: Expr, loc: Location) -> Self {
        Expr::new(ExprKind::Comma(Box::new(left), Box::new(right)), loc)
    }

    pub fn conditional(cond: Expr, left: Expr, right: Expr, loc: Location) -> Self {
        Expr::new(
            ExprKind::Conditional(Box::new(cond), Box::new(left), Box::new(right)),
            loc,
        )
    }
}

impl Debug for Expr {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use ExprKind::*;
        let string = match &self.kind {
            Literal(literal) => literal.to_string(),
            Unary(operator, right) => parenthesize(operator.to_string(), &[right]),
            Binary(left, operator, right) => parenthesize(operator.to_string(), &[left, right]),
            Grouping(expr) => parenthesize("group", &[expr]),
            Comma(left, right) => parenthesize("comma", &[left, right]),
            Conditional(cond, left, right) => parenthesize("?:", &[cond, left, right]),
        };

        write!(f, "{}[{}]", string, self.loc)
    }
}

fn parenthesize(name: &str, exprs: &[&Expr]) -> String {
    let mut parts = vec![String::from("("), String::from(name)];

    for expr in exprs {
        parts.push(format!(" {:?}", expr));
    }

    parts.push(String::from(")"));

    parts.join("")
}

#[derive(PartialEq)]
pub enum LiteralExpr {
    Integer(i64),
    Float(f64),
    Str(String),
    Boolean(bool),
    Nil,
}

impl Display for LiteralExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use LiteralExpr::*;
        match self {
            Integer(int) => write!(f, "{}", int),
            Float(float) => write!(f, "{}", float),
            Str(string) => write!(f, "{}", string),
            Boolean(boolean) => write!(f, "{}", boolean),
            Nil => write!(f, "nil"),
        }
    }
}

#[derive(PartialEq)]
pub enum UnaryOp {
    Negate,
    Not,
}

impl UnaryOp {
    fn to_string(&self) -> &'static str {
        use UnaryOp::*;
        match self {
            Negate => "-",
            Not => "!",
        }
    }
}

impl From<TokenKind> for UnaryOp {
    fn from(kind: TokenKind) -> Self {
        use TokenKind::*;
        use UnaryOp::*;
        match kind {
            Bang => Not,
            Minus => Negate,
            kind => panic!("Token kind '{:?}' is not a unary operator", kind),
        }
    }
}

#[derive(PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl BinaryOp {
    fn to_string(&self) -> &'static str {
        use BinaryOp::*;
        match self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Rem => "%",
            Equal => "==",
            NotEqual => "!=",
            Greater => ">",
            GreaterEqual => ">=",
            Less => "<",
            LessEqual => "<=",
        }
    }
}

impl From<TokenKind> for BinaryOp {
    fn from(kind: TokenKind) -> Self {
        use TokenKind::*;
        match kind {
            Plus => BinaryOp::Add,
            Minus => BinaryOp::Sub,
            Star => BinaryOp::Mul,
            Slash => BinaryOp::Div,
            Percent => BinaryOp::Rem,
            EqualEqual => BinaryOp::Equal,
            BangEqual => BinaryOp::NotEqual,
            Greater => BinaryOp::Greater,
            GreaterEqual => BinaryOp::GreaterEqual,
            Less => BinaryOp::Less,
            LessEqual => BinaryOp::LessEqual,
            kind => panic!("Token kind '{:?}' is not a binary operator", kind),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn int_expr(int: i64, (line, col): (usize, usize)) -> Expr {
        Expr::new(
            ExprKind::Literal(LiteralExpr::Integer(int)),
            Location::new(line, col),
        )
    }

    fn binary_expr(left: Expr, op: BinaryOp, right: Expr, (line, col): (usize, usize)) -> Expr {
        Expr::binary(left, op, right, Location::new(line, col))
    }

    fn group_expr(expr: Expr, (line, col): (usize, usize)) -> Expr {
        Expr::groping(expr, Location::new(line, col))
    }

    #[test]
    fn test_debug() {
        // (1 + 2) * (4 - 3)
        let expr = binary_expr(
            group_expr(
                binary_expr(
                    int_expr(1, (0, 1)),
                    BinaryOp::Add,
                    int_expr(2, (0, 5)),
                    (0, 3),
                ),
                (0, 0),
            ),
            BinaryOp::Mult,
            group_expr(
                binary_expr(
                    int_expr(4, (0, 11)),
                    BinaryOp::Sub,
                    int_expr(3, (0, 15)),
                    (0, 13),
                ),
                (0, 10),
            ),
            (0, 8),
        );
        assert_eq!(
            format!("{:?}", expr),
            "(* (group (+ 1[1:1] 2[1:5])[1:3])[1:0] (group (- 4[1:11] 3[1:15])[1:13])[1:10])[1:8]"
        );
    }
}
