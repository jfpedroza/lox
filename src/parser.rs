use crate::expr::Expr;
use crate::lexer::{
    NumberKind, Token,
    TokenKind::{self, *},
};
use crate::location::Location;

pub struct Parser<'a> {
    input: &'a [Token<'a>],
    current: usize,
}

#[derive(Debug, PartialEq, Fail)]
pub enum ParsingError {
    #[fail(display = "[{}] Expected expression. Got {}", _0, _1)]
    ExpectedExpression(Location, String),
    #[fail(display = "[{}] Exptected ')' after expression. Got {}", _0, _1)]
    ExpectedCloseParen(Location, String),
    #[fail(
        display = "[{}] Exptected ':' for conditional expression. Got {}",
        _0, _1
    )]
    ExpectedColon(Location, String),
}

type TokenRef<'a> = &'a Token<'a>;
type OptTokenRef<'a> = Option<&'a Token<'a>>;
type TokenRefRes<'a> = Result<&'a Token<'a>, ParsingError>;
type ExprParseRes = Result<Expr, ParsingError>;

impl<'a> Parser<'a> {
    pub fn new(input: &'a [Token<'a>]) -> Self {
        Parser { input, current: 0 }
    }

    pub fn parse(&mut self) -> ExprParseRes {
        self.expression()
    }

    fn is_at_end(&self) -> bool {
        self.peek().kind == EOF
    }

    fn advance(&mut self) -> TokenRef<'a> {
        if !self.is_at_end() {
            self.current += 1;
        }

        self.previous()
    }

    fn peek(&self) -> &Token {
        &self.input[self.current]
    }

    fn previous(&self) -> TokenRef<'a> {
        &self.input[self.current - 1]
    }

    fn matches(&mut self, kinds: &[TokenKind]) -> OptTokenRef<'a> {
        for kind in kinds {
            if self.check(*kind) {
                return Some(self.advance());
            }
        }

        None
    }

    fn check(&self, kind: TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.peek().kind == kind
    }

    fn consume<F>(&mut self, kind: TokenKind, err_fn: F) -> TokenRefRes<'a>
    where
        F: FnOnce(&Parser<'a>) -> ParsingError,
    {
        self.matches(&[kind]).ok_or_else(|| err_fn(self))
    }

    fn expression(&mut self) -> ExprParseRes {
        let expr = self.conditional()?;
        if self.matches(&[Comma]).is_some() {
            let right = self.expression()?;
            Ok(Expr::comma(expr, right))
        } else {
            Ok(expr)
        }
    }

    fn conditional(&mut self) -> ExprParseRes {
        let expr = self.equality()?;
        if self.matches(&[Question]).is_some() {
            let left = self.expression()?;
            self.consume(Colon, Self::expected_colon_error)?;
            let right = self.conditional()?;
            Ok(Expr::conditional(expr, left, right))
        } else {
            Ok(expr)
        }
    }

    fn left_binary_expression<F>(&mut self, kinds: &[TokenKind], mut expr_fn: F) -> ExprParseRes
    where
        F: FnMut(&mut Parser<'a>) -> ExprParseRes,
    {
        let mut expr = expr_fn(self)?;

        while let Some(op_token) = self.matches(kinds) {
            let op = op_token.kind.into();
            let right = expr_fn(self)?;

            expr = Expr::binary(expr, op, right);
        }

        Ok(expr)
    }

    fn equality(&mut self) -> ExprParseRes {
        self.left_binary_expression(&[BangEqual, EqualEqual], Self::comparison)
    }

    fn comparison(&mut self) -> ExprParseRes {
        self.left_binary_expression(&[Greater, GreaterEqual, Less, LessEqual], Self::addition)
    }

    fn addition(&mut self) -> ExprParseRes {
        self.left_binary_expression(&[Minus, Plus], Self::multiplication)
    }

    fn multiplication(&mut self) -> ExprParseRes {
        self.left_binary_expression(&[Star, Slash, Percent], Self::unary)
    }

    fn unary(&mut self) -> ExprParseRes {
        if let Some(op_token) = self.matches(&[Bang, Minus]) {
            let op = op_token.kind.into();
            let right = self.unary()?;
            return Ok(Expr::unary(op, right));
        }

        self.primary()
    }

    fn primary(&mut self) -> ExprParseRes {
        let int_kind = Number(NumberKind::Integer);
        let float_kind = Number(NumberKind::Float);

        if self.matches(&[False]).is_some() {
            Ok(Expr::boolean(false))
        } else if self.matches(&[True]).is_some() {
            Ok(Expr::boolean(true))
        } else if self.matches(&[Nil]).is_some() {
            Ok(Expr::nil())
        } else if let Some(token) = self.matches(&[int_kind, float_kind, Str]) {
            Ok(Expr::from_literal(token.literal.as_ref().unwrap()))
        } else if self.matches(&[LeftParen]).is_some() {
            let expr = self.expression()?;
            self.consume(RightParen, Self::expected_close_paren_error)?;
            Ok(Expr::groping(expr))
        } else {
            Err(self.expected_expression_error())
        }
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().kind == Semicolon {
                return;
            }

            if [Class, Fun, Var, For, If, While, Print, Return].contains(&self.peek().kind) {
                return;
            }

            self.advance();
        }
    }

    fn expected_close_paren_error(&self) -> ParsingError {
        let token = self.peek();
        ParsingError::ExpectedCloseParen(token.location, token.lexeme.to_string())
    }

    fn expected_expression_error(&self) -> ParsingError {
        let token = self.peek();
        ParsingError::ExpectedExpression(token.location, token.lexeme.to_string())
    }

    fn expected_colon_error(&self) -> ParsingError {
        let token = self.peek();
        ParsingError::ExpectedColon(token.location, token.lexeme.to_string())
    }
}
