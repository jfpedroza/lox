#[cfg(test)]
mod tests;

use crate::expr::Expr;
use crate::lexer::{
    NumberKind, Token,
    TokenKind::{self, *},
};
use crate::location::Loc;

pub struct Parser<'a> {
    input: &'a [Token<'a>],
    current: usize,
}

#[derive(Debug, PartialEq, Fail)]
pub enum ParsingError {
    #[fail(display = "[{}] Expected expression. Got {}", _0, _1)]
    ExpectedExpression(Loc, String),
    #[fail(display = "[{}] Exptected ')' after expression. Got {}", _0, _1)]
    ExpectedCloseParen(Loc, String),
    #[fail(
        display = "[{}] Exptected ':' for conditional expression. Got {}",
        _0, _1
    )]
    ExpectedColon(Loc, String),
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

    pub fn expression(&mut self) -> ExprParseRes {
        let expr = self.conditional()?;
        if let Some(token) = self.matches(&[Comma]) {
            let right = self.expression()?;
            Ok(Expr::comma(expr, right, token.loc))
        } else {
            Ok(expr)
        }
    }

    fn conditional(&mut self) -> ExprParseRes {
        let expr = self.equality()?;
        if let Some(op_token) = self.matches(&[Question]) {
            let left = self.expression()?;
            self.consume(Colon, Self::expected_colon_error)?;
            let right = self.conditional()?;
            Ok(Expr::conditional(expr, left, right, op_token.loc))
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

            expr = Expr::binary(expr, op, right, op_token.loc);
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
            return Ok(Expr::unary(op, right, op_token.loc));
        }

        self.primary()
    }

    fn primary(&mut self) -> ExprParseRes {
        let int_kind = Number(NumberKind::Integer);
        let float_kind = Number(NumberKind::Float);

        if let Some(token) = self.matches(&[False]) {
            Ok(Expr::boolean(false, token.loc))
        } else if let Some(token) = self.matches(&[True]) {
            Ok(Expr::boolean(true, token.loc))
        } else if let Some(token) = self.matches(&[Nil]) {
            Ok(Expr::nil(token.loc))
        } else if let Some(token) = self.matches(&[int_kind, float_kind, Str]) {
            let literal = token.literal.as_ref().unwrap();
            Ok(Expr::from_literal(literal, token.loc))
        } else if let Some(token) = self.matches(&[LeftParen]) {
            let expr = self.expression()?;
            self.consume(RightParen, Self::expected_close_paren_error)?;
            Ok(Expr::groping(expr, token.loc))
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
        ParsingError::ExpectedCloseParen(token.loc, token.lexeme.to_string())
    }

    fn expected_expression_error(&self) -> ParsingError {
        let token = self.peek();
        ParsingError::ExpectedExpression(token.loc, token.lexeme.to_string())
    }

    fn expected_colon_error(&self) -> ParsingError {
        let token = self.peek();
        ParsingError::ExpectedColon(token.loc, token.lexeme.to_string())
    }
}
