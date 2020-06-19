use std::fmt;

enum Expr {
    Literal(LiteralExpr),
    Unary(UnaryOp, Box<Expr>),
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Grouping(Box<Expr>),
}

impl Expr {
    fn integer(int: i64) -> Self {
        Expr::Literal(LiteralExpr::Integer(int))
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

enum LiteralExpr {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Nil,
}

impl fmt::Display for LiteralExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use LiteralExpr::*;
        match self {
            Integer(int) => write!(f, "{}", int),
            Float(float) => write!(f, "{}", float),
            String(string) => write!(f, "{}", string),
            Boolean(boolean) => write!(f, "{}", boolean),
            Nil => write!(f, "nil"),
        }
    }
}

enum UnaryOp {
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

enum BinaryOp {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_debug() {
        // (1 + 2) * (4 - 3)
        let expr = Expr::Binary(
            Box::new(Expr::Grouping(Box::new(Expr::Binary(
                Box::new(Expr::integer(1)),
                BinaryOp::Add,
                Box::new(Expr::integer(2)),
            )))),
            BinaryOp::Mult,
            Box::new(Expr::Grouping(Box::new(Expr::Binary(
                Box::new(Expr::integer(4)),
                BinaryOp::Sub,
                Box::new(Expr::integer(3)),
            )))),
        );
        assert_eq!(format!("{:?}", expr), "(* (group (+ 1 2)) (group (- 4 3)))");
    }
}
