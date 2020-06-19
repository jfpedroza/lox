use crate::lexer::{Literal, TokenKind};
use std::fmt;

pub enum Expr {
    Literal(LiteralExpr),
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Grouping(Box<Expr>),
}

impl Expr {
    pub fn from_literal(literal: &Literal) -> Self {
        use Literal::*;
        Expr::Literal(match literal {
            Integer(int) => LiteralExpr::Integer(*int),
            Float(float) => LiteralExpr::Float(*float),
            Str(string) => LiteralExpr::Str(string.clone()),
        })
    }

    pub fn boolean(boolean: bool) -> Self {
        Expr::Literal(LiteralExpr::Boolean(boolean))
    }

    pub fn nil() -> Self {
        Expr::Literal(LiteralExpr::Nil)
    }

    pub fn unary(op: UnaryOp, right: Expr) -> Self {
        Expr::Unary(op, Box::new(right))
    }

    pub fn binary(left: Expr, op: BinaryOp, right: Expr) -> Self {
        Expr::Binary(Box::new(left), op, Box::new(right))
    }

    pub fn groping(expr: Expr) -> Self {
        Expr::Grouping(Box::new(expr))
    }
}

impl fmt::Debug for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Expr::*;
        let string = match self {
            Literal(literal) => literal.to_string(),
            Unary(operator, right) => parenthesize(operator.to_string(), &[right]),
            Binary(left, operator, right) => parenthesize(operator.to_string(), &[left, right]),
            Grouping(expr) => parenthesize("group", &[expr]),
        };

        write!(f, "{}", string)
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

pub enum LiteralExpr {
    Integer(i64),
    Float(f64),
    Str(String),
    Boolean(bool),
    Nil,
}

impl fmt::Display for LiteralExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
            Bang => Negate,
            Minus => Not,
            kind => panic!("Token kind '{:?}' is not a unary operator", kind),
        }
    }
}

pub enum BinaryOp {
    Add,
    Sub,
    Mult,
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
            Mult => "*",
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
            Star => BinaryOp::Mult,
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

    fn integer_expr(int: i64) -> Expr {
        Expr::Literal(LiteralExpr::Integer(int))
    }

    #[test]
    fn test_debug() {
        // (1 + 2) * (4 - 3)
        let expr = Expr::Binary(
            Box::new(Expr::Grouping(Box::new(Expr::Binary(
                Box::new(integer_expr(1)),
                BinaryOp::Add,
                Box::new(integer_expr(2)),
            )))),
            BinaryOp::Mult,
            Box::new(Expr::Grouping(Box::new(Expr::Binary(
                Box::new(integer_expr(4)),
                BinaryOp::Sub,
                Box::new(integer_expr(3)),
            )))),
        );
        assert_eq!(format!("{:?}", expr), "(* (group (+ 1 2)) (group (- 4 3)))");
    }
}
