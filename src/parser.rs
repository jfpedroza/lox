#[cfg(test)]
mod tests;

use crate::expr::{Expr, ExprKind};
use crate::lexer::{
    Token,
    TokenKind::{self, *},
};
use crate::location::Loc;
use crate::stmt::Stmt;

pub struct Parser<'a> {
    input: &'a [Token<'a>],
    current: usize,
    errors: Vec<ParsingError>,
    pub allow_expression: bool,
    found_expression: bool,
}

#[derive(Debug, PartialEq, Fail)]
pub enum ParsingError {
    ExpectedExpression(Loc, String),
    ExpectedCloseParen(Loc, String),
    ExpectedCloseBrace(Loc, String),
    ExpectedColon(Loc, String),
    ExpectedSemicolon(Loc, String, String),
    ExpectedVarName(Loc, String),
    InvalidAssignmentTarget(Loc),
    Multiple(Vec<ParsingError>),
}

type TokenRef<'a> = &'a Token<'a>;
type OptTokenRef<'a> = Option<&'a Token<'a>>;
type TokenRefRes<'a> = Result<&'a Token<'a>, ParsingError>;
type ExprParseRes = Result<Expr, ParsingError>;
type StmtParseRes = Result<Stmt, ParsingError>;

impl<'a> Parser<'a> {
    pub fn new(input: &'a [Token<'a>]) -> Self {
        Parser {
            input,
            current: 0,
            errors: vec![],
            allow_expression: false,
            found_expression: false,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParsingError> {
        let mut stmts = Vec::new();

        while !self.is_at_end() {
            stmts.push(self.declaration()?);
            if self.found_expression {
                break;
            }

            self.allow_expression = false;
        }

        match self.errors.len() {
            0 => Ok(stmts),
            1 => Err(self.errors.pop().unwrap()),
            len => {
                let mut errors = Vec::with_capacity(len);
                errors.append(&mut self.errors);
                Err(ParsingError::Multiple(errors))
            }
        }
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

    fn declaration(&mut self) -> StmtParseRes {
        let res = if self.matches(&[Var]).is_some() {
            self.var_declaration()
        } else {
            self.statement()
        };

        res.or_else(|err| {
            self.errors.push(err);
            self.synchronize();
            // Returns a dummy nil;
            Ok(Stmt::expression(Expr::nil(Loc::new(0, 0)), Loc::new(0, 0)))
        })
    }

    fn statement(&mut self) -> StmtParseRes {
        if self.matches(&[Print]).is_some() {
            return self.print_statement();
        } else if let Some(token) = self.matches(&[LeftBrace]) {
            return Ok(Stmt::block(self.block()?, token.loc));
        }

        self.expression_statement()
    }

    fn print_statement(&mut self) -> StmtParseRes {
        let Token { loc, .. } = self.previous();
        let expr = self.expression()?;
        self.consume(Semicolon, |p| p.expected_semicolon_error("value"))?;
        Ok(Stmt::print(expr, *loc))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParsingError> {
        let mut stmts = Vec::new();

        while !self.check(RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }

        self.consume(RightBrace, Self::expected_close_brace_error)?;

        Ok(stmts)
    }

    fn var_declaration(&mut self) -> StmtParseRes {
        let Token { loc, .. } = self.previous();
        let name = self.consume(Identifier, Self::expected_var_name_error)?;
        let init = if self.matches(&[Equal]).is_some() {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(Semicolon, |p| {
            p.expected_semicolon_error("variable declaration")
        })?;

        Ok(Stmt::var(name.lexeme, init, *loc))
    }

    fn expression_statement(&mut self) -> StmtParseRes {
        let expr = self.expression()?;
        if self.allow_expression && self.is_at_end() {
            self.found_expression = true;
        } else {
            self.consume(Semicolon, |p| p.expected_semicolon_error("expression"))?;
        }

        let loc = expr.loc;
        Ok(Stmt::expression(expr, loc))
    }

    pub fn expression(&mut self) -> ExprParseRes {
        let expr = self.assignment()?;
        if let Some(token) = self.matches(&[Comma]) {
            let right = self.expression()?;
            Ok(Expr::comma(expr, right, token.loc))
        } else {
            Ok(expr)
        }
    }

    fn assignment(&mut self) -> ExprParseRes {
        let expr = self.conditional()?;

        if let Some(token) = self.matches(&[Equal]) {
            let value = self.expression()?;

            if let ExprKind::Variable(name) = expr.kind {
                return Ok(Expr::assign(name, value, expr.loc));
            } else {
                self.errors
                    .push(ParsingError::InvalidAssignmentTarget(token.loc));
            }
        }

        Ok(expr)
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
        let primary_tokens = [False, True, Nil, Integer, Float, Str, LeftParen, Identifier];
        let token = self
            .matches(&primary_tokens)
            .ok_or_else(|| self.expected_expression_error())?;

        Ok(match token.kind {
            False => Expr::boolean(false, token.loc),
            True => Expr::boolean(true, token.loc),
            Nil => Expr::nil(token.loc),
            Integer | Float | Str => {
                let literal = token.literal.as_ref().unwrap();
                Expr::from_literal(literal, token.loc)
            }
            LeftParen => {
                let expr = self.expression()?;
                self.consume(RightParen, Self::expected_close_paren_error)?;
                Expr::groping(expr, token.loc)
            }
            Identifier => Expr::variable(token.lexeme, token.loc),
            kind => panic!("Shouldn't have executed this. Kind: {:?}", kind),
        })
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

    fn expected_close_brace_error(&self) -> ParsingError {
        let token = self.peek();
        ParsingError::ExpectedCloseBrace(token.loc, token.lexeme.to_string())
    }

    fn expected_expression_error(&self) -> ParsingError {
        let token = self.peek();
        ParsingError::ExpectedExpression(token.loc, token.lexeme.to_string())
    }

    fn expected_colon_error(&self) -> ParsingError {
        let token = self.peek();
        ParsingError::ExpectedColon(token.loc, token.lexeme.to_string())
    }

    fn expected_semicolon_error(&self, after: &str) -> ParsingError {
        let token = self.peek();
        ParsingError::ExpectedSemicolon(token.loc, String::from(after), token.lexeme.to_string())
    }

    fn expected_var_name_error(&self) -> ParsingError {
        let token = self.peek();
        ParsingError::ExpectedVarName(token.loc, token.lexeme.to_string())
    }
}
