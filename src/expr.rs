use crate::lexer::{Literal, TokenKind};
use crate::location::{Loc, Located};
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};

#[derive(PartialEq)]
pub enum ExprKind {
    Literal(LitExpr),
    Unary(UnOp, Box<Expr>),
    Binary(Box<Expr>, BinOp, Box<Expr>),
    Grouping(Box<Expr>),
    Comma(Box<Expr>, Box<Expr>),
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),
}

pub type Expr = Located<ExprKind>;

impl Expr {
    pub fn from_literal(literal: &Literal, loc: Loc) -> Self {
        use Literal::*;
        let kind = ExprKind::Literal(match literal {
            Integer(int) => LitExpr::Integer(*int),
            Float(float) => LitExpr::Float(*float),
            Str(string) => LitExpr::Str(string.clone()),
        });

        Expr::new(kind, loc)
    }

    pub fn boolean(boolean: bool, loc: Loc) -> Self {
        Expr::new(ExprKind::Literal(LitExpr::Boolean(boolean)), loc)
    }

    pub fn nil(loc: Loc) -> Self {
        Expr::new(ExprKind::Literal(LitExpr::Nil), loc)
    }

    pub fn unary(op: UnOp, right: Expr, loc: Loc) -> Self {
        Expr::new(ExprKind::Unary(op, Box::new(right)), loc)
    }

    pub fn binary(left: Expr, op: BinOp, right: Expr, loc: Loc) -> Self {
        Expr::new(ExprKind::Binary(Box::new(left), op, Box::new(right)), loc)
    }

    pub fn groping(expr: Expr, loc: Loc) -> Self {
        Expr::new(ExprKind::Grouping(Box::new(expr)), loc)
    }

    pub fn comma(left: Expr, right: Expr, loc: Loc) -> Self {
        Expr::new(ExprKind::Comma(Box::new(left), Box::new(right)), loc)
    }

    pub fn conditional(cond: Expr, left: Expr, right: Expr, loc: Loc) -> Self {
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
pub enum LitExpr {
    Integer(i64),
    Float(f64),
    Str(String),
    Boolean(bool),
    Nil,
}

impl Display for LitExpr {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        use LitExpr::*;
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
pub enum UnOp {
    Negate,
    Not,
}

impl UnOp {
    fn to_string(&self) -> &'static str {
        use UnOp::*;
        match self {
            Negate => "-",
            Not => "!",
        }
    }
}

impl From<TokenKind> for UnOp {
    fn from(kind: TokenKind) -> Self {
        use TokenKind::*;
        use UnOp::*;
        match kind {
            Bang => Not,
            Minus => Negate,
            kind => panic!("Token kind '{:?}' is not a unary operator", kind),
        }
    }
}

#[derive(PartialEq)]
pub enum BinOp {
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

impl BinOp {
    fn to_string(&self) -> &'static str {
        use BinOp::*;
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

impl From<TokenKind> for BinOp {
    fn from(kind: TokenKind) -> Self {
        use TokenKind::*;
        match kind {
            Plus => BinOp::Add,
            Minus => BinOp::Sub,
            Star => BinOp::Mul,
            Slash => BinOp::Div,
            Percent => BinOp::Rem,
            EqualEqual => BinOp::Equal,
            BangEqual => BinOp::NotEqual,
            Greater => BinOp::Greater,
            GreaterEqual => BinOp::GreaterEqual,
            Less => BinOp::Less,
            LessEqual => BinOp::LessEqual,
            kind => panic!("Token kind '{:?}' is not a binary operator", kind),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::test_utils::*;

    #[test]
    fn test_debug() {
        // (1 + 2) * (4 - 3)
        let expr = mult_expr(
            group_expr(
                add_expr(int_expr(1, (0, 1)), int_expr(2, (0, 5)), (0, 3)),
                (0, 0),
            ),
            group_expr(
                sub_expr(int_expr(4, (0, 11)), int_expr(3, (0, 15)), (0, 13)),
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
