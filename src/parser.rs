#[cfg(test)]
mod tests;

use crate::expr::{BinOp, Expr, ExprKind, Param};
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
    pub(super) allow_expression: bool,
    found_expression: bool,
}

#[derive(Debug, PartialEq, Fail)]
pub enum ParsingError {
    ExpectedExpression(Loc, String),
    ExpectedOpenParen(Loc, String, String),
    ExpectedCloseParen(Loc, String, String),
    ExpectedOpenBrace(Loc, String, String),
    ExpectedCloseBrace(Loc, String, String),
    ExpectedColon(Loc, String),
    ExpectedSemicolon(Loc, String, String),
    ExpectedName(Loc, String, String),
    InvalidAssignmentTarget(Loc),
    MaximumArgumentsExceeded(Loc, String),
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
            _ => {
                let mut errors = Vec::new();
                std::mem::swap(&mut self.errors, &mut errors);
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

    fn check_next(&self, kind: TokenKind) -> bool {
        if self.is_at_end() {
            return false;
        }

        self.input[self.current + 1].kind == kind
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
        } else if self.check(Fun) && self.check_next(Identifier) {
            self.advance();
            self.function("function")
        } else if self.matches(&[Class]).is_some() {
            self.class_declaration()
        } else {
            self.statement()
        };

        res.or_else(|err| {
            self.errors.push(err);
            self.synchronize();
            // Returns a dummy stmt
            Ok(Stmt::default())
        })
    }

    fn statement(&mut self) -> StmtParseRes {
        if self.matches(&[If]).is_some() {
            self.if_statement()
        } else if self.matches(&[Print]).is_some() {
            self.print_statement()
        } else if self.matches(&[While]).is_some() {
            self.while_statement()
        } else if self.matches(&[For]).is_some() {
            self.for_statement()
        } else if self.matches(&[Return]).is_some() {
            self.return_statement()
        } else if let Some(token) = self.matches(&[LeftBrace]) {
            Ok(Stmt::block(self.block()?, token.loc))
        } else if let Some(token) = self.matches(&[Break]) {
            self.consume(Semicolon, |p| p.expected_semicolon_error("'break'"))?;
            Ok(Stmt::break_stmt(token.loc))
        } else {
            self.expression_statement()
        }
    }

    fn if_statement(&mut self) -> StmtParseRes {
        let Token { loc, .. } = self.previous();
        self.consume(LeftParen, |p| p.expected_open_paren_error("'if'"))?;
        let cond = self.expression()?;
        self.consume(RightParen, |p| p.expected_close_paren_error("if condition"))?;

        let then_branch = self.statement()?;
        let else_branch = self
            .matches(&[Else])
            .map(|_| self.statement())
            .transpose()?;

        Ok(Stmt::if_stmt(cond, then_branch, else_branch, *loc))
    }

    fn print_statement(&mut self) -> StmtParseRes {
        let Token { loc, .. } = self.previous();
        let expr = self.expression()?;
        self.consume(Semicolon, |p| p.expected_semicolon_error("value"))?;
        Ok(Stmt::print(expr, *loc))
    }

    fn while_statement(&mut self) -> StmtParseRes {
        let Token { loc, .. } = self.previous();
        self.consume(LeftParen, |p| p.expected_open_paren_error("'while'"))?;
        let cond = self.expression()?;
        self.consume(RightParen, |p| {
            p.expected_close_paren_error("while condition")
        })?;

        let body = self.statement()?;

        Ok(Stmt::while_stmt(cond, body, *loc))
    }

    fn for_statement(&mut self) -> StmtParseRes {
        let Token { loc, .. } = self.previous();
        self.consume(LeftParen, |p| p.expected_open_paren_error("'for'"))?;

        let init = if self.matches(&[Semicolon]).is_some() {
            None
        } else if self.matches(&[Var]).is_some() {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let cond = if let Some(token) = self.matches(&[Semicolon]) {
            Expr::boolean(true, token.loc)
        } else {
            let cond = self.expression()?;
            self.consume(Semicolon, |p| p.expected_semicolon_error("condition"))?;
            cond
        };

        let increment = if self.check(RightParen) {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(RightParen, |p| p.expected_close_paren_error("for clauses"))?;

        let mut body = self.statement()?;

        if let Some(inc_expr) = increment {
            let body_loc = body.loc;
            body = Stmt::block(vec![body, inc_expr.into()], body_loc);
        }

        let while_stmt = Stmt::while_stmt(cond, body, *loc);
        Ok(if let Some(init_stmt) = init {
            let init_loc = init_stmt.loc;
            Stmt::block(vec![init_stmt, while_stmt], init_loc)
        } else {
            while_stmt
        })
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParsingError> {
        let mut stmts = Vec::new();

        while !self.check(RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }

        self.consume(RightBrace, |p| p.expected_close_brace_error("block"))?;

        Ok(stmts)
    }

    fn return_statement(&mut self) -> StmtParseRes {
        let Token { loc, .. } = self.previous();
        let ret = if !self.check(Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(Semicolon, |p| p.expected_semicolon_error("return value"))?;

        Ok(Stmt::return_stmt(ret, *loc))
    }

    fn var_declaration(&mut self) -> StmtParseRes {
        let Token { loc, .. } = self.previous();
        let name = self.consume(Identifier, |p| p.expected_name_error("variable"))?;
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

    fn function(&mut self, kind: &str) -> StmtParseRes {
        let name = self.consume(Identifier, |p| p.expected_name_error(kind))?;
        let params = self.function_params(&format!("{} name", kind))?;

        self.consume(LeftBrace, |p| {
            p.expected_open_brace_error(&format!("{} body", kind))
        })?;
        let body = self.block()?;

        Ok(Stmt::function(name.lexeme, params, body, name.loc))
    }

    fn function_params(&mut self, open_paren_after: &str) -> Result<Vec<Param>, ParsingError> {
        self.consume(LeftParen, |p| p.expected_open_paren_error(open_paren_after))?;

        let mut params = Vec::new();
        if !self.check(RightParen) {
            params.push(self.consume(Identifier, |p| p.expected_name_error("parameter"))?);
            while self.matches(&[Comma]).is_some() {
                params.push(self.consume(Identifier, |p| p.expected_name_error("parameter"))?);
            }

            if params.len() > 255 {
                let loc = params[256].loc;
                self.errors
                    .push(ParsingError::max_args_exceeded(loc, "parameters"));
            }
        }

        self.consume(RightParen, |p| p.expected_close_paren_error("parameters"))?;

        let params = params
            .iter()
            .map(|param| Param::new(String::from(param.lexeme), param.loc))
            .collect();
        Ok(params)
    }

    fn class_declaration(&mut self) -> StmtParseRes {
        let Token { loc, .. } = self.previous();
        let name = self.consume(Identifier, |p| p.expected_name_error("class"))?;

        self.consume(LeftBrace, |p| p.expected_open_brace_error("class body"))?;
        let mut methods = Vec::new();
        while !self.check(RightBrace) && !self.is_at_end() {
            methods.push(self.function("method")?);
        }

        self.consume(RightBrace, |p| p.expected_close_brace_error("class body"))?;

        Ok(Stmt::class(name.lexeme, methods, *loc))
    }

    fn expression_statement(&mut self) -> StmtParseRes {
        let expr = self.expression()?;
        if self.allow_expression && self.is_at_end() {
            self.found_expression = true;
        } else {
            self.consume(Semicolon, |p| p.expected_semicolon_error("expression"))?;
        }

        Ok(Stmt::from(expr))
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

        if let Some(token) = self.matches(&[
            Equal,
            PlusEqual,
            MinusEqual,
            StarEqual,
            SlashEqual,
            PercentEqual,
        ]) {
            let value = self.expression()?;
            return self.make_assign_expr(expr, token, value);
        }

        Ok(expr)
    }

    fn make_assign_expr(&mut self, expr: Expr, op_token: &Token, value: Expr) -> ExprParseRes {
        Ok(if let ExprKind::Variable(name) = expr.kind {
            let assign_value = match op_token.kind {
                Equal => value,
                PlusEqual | PlusPlus | MinusEqual | MinusMinus | StarEqual | SlashEqual
                | PercentEqual => {
                    let op = op_token.kind.into();
                    Expr::binary(Expr::variable(&name, expr.loc), op, value, op_token.loc)
                }
                _ => unreachable!(),
            };

            Expr::assign(name, assign_value, expr.loc)
        } else {
            self.errors
                .push(ParsingError::InvalidAssignmentTarget(expr.loc));
            value
        })
    }

    fn conditional(&mut self) -> ExprParseRes {
        let expr = self.or()?;
        if let Some(op_token) = self.matches(&[Question]) {
            let left = self.expression()?;
            self.consume(Colon, Self::expected_colon_error)?;
            let right = self.conditional()?;
            Ok(Expr::conditional(expr, left, right, op_token.loc))
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> ExprParseRes {
        let mut expr = self.and()?;

        while let Some(op_token) = self.matches(&[Or]) {
            let op = op_token.kind.into();
            let right = self.and()?;

            expr = Expr::logical(expr, op, right, op_token.loc);
        }

        Ok(expr)
    }

    fn and(&mut self) -> ExprParseRes {
        let mut expr = self.equality()?;

        while let Some(op_token) = self.matches(&[And]) {
            let op = op_token.kind.into();
            let right = self.equality()?;

            expr = Expr::logical(expr, op, right, op_token.loc);
        }

        Ok(expr)
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
            Ok(Expr::unary(op, right, op_token.loc))
        } else if let Some(op_token) = self.matches(&[PlusPlus, MinusMinus]) {
            let right = self.unary()?;
            // ++i generates i = i + 1
            self.make_assign_expr(right, op_token, Expr::integer(1, op_token.loc))
        } else {
            self.postfix()
        }
    }

    fn postfix(&mut self) -> ExprParseRes {
        let left = self.primary()?;

        if let Some(op_token) = self.matches(&[PlusPlus, MinusMinus]) {
            // i++ generates i = i + 1, i - 1
            let one = Expr::integer(1, op_token.loc);
            let left = self.make_assign_expr(left, op_token, one)?;
            let name = match left.kind {
                ExprKind::Assign(ref name, _) => name,
                _ => unreachable!(),
            };

            let op = match op_token.kind {
                PlusPlus => BinOp::Sub,
                MinusMinus => BinOp::Add,
                _ => unreachable!(),
            };

            let one = Expr::integer(1, op_token.loc);
            let right = Expr::binary(Expr::variable(name, left.loc), op, one, op_token.loc);

            let left_loc = left.loc;
            Ok(Expr::comma(left, right, left_loc))
        } else {
            let mut expr = left;
            loop {
                expr = if self.matches(&[LeftParen]).is_some() {
                    self.finish_call(expr)?
                } else if self.matches(&[Dot]).is_some() {
                    let name = self.consume(Identifier, |p| p.expected_name_error("property"))?;
                    Expr::get(expr, name.lexeme, name.loc)
                } else {
                    break;
                }
            }

            Ok(expr)
        }
    }

    fn finish_call(&mut self, callee: Expr) -> ExprParseRes {
        let mut args = Vec::new();

        if !self.check(RightParen) {
            args.push(self.assignment()?);
            while self.matches(&[Comma]).is_some() {
                args.push(self.assignment()?);
            }

            if args.len() > 255 {
                let loc = args[256].loc;
                self.errors
                    .push(ParsingError::max_args_exceeded(loc, "arguments"));
            }
        }

        let token = self.consume(RightParen, |p| p.expected_close_paren_error("arguments"))?;

        Ok(Expr::call(callee, args, token.loc))
    }

    fn primary(&mut self) -> ExprParseRes {
        let primary_tokens = [
            False, True, Nil, Integer, Float, Str, LeftParen, Identifier, Fun,
        ];
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
                self.consume(RightParen, |p| p.expected_close_paren_error("expression"))?;
                Expr::grouping(expr, token.loc)
            }
            Identifier => Expr::variable(token.lexeme, token.loc),
            Fun => self.anon_function()?,
            kind => panic!("Shouldn't have executed this. Kind: {:?}", kind),
        })
    }

    fn anon_function(&mut self) -> ExprParseRes {
        let Token { loc, .. } = self.previous();

        let params = self.function_params("'fun'")?;

        self.consume(LeftBrace, |p| {
            p.expected_open_brace_error("anonymous function body")
        })?;
        let body = self.block()?;

        Ok(Expr::function(params, body, *loc))
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

    fn expected_open_paren_error(&self, after: &str) -> ParsingError {
        let token = self.peek();
        ParsingError::ExpectedOpenParen(token.loc, String::from(after), token.lexeme.to_string())
    }

    fn expected_close_paren_error(&self, after: &str) -> ParsingError {
        let token = self.peek();
        ParsingError::ExpectedCloseParen(token.loc, String::from(after), token.lexeme.to_string())
    }

    fn expected_open_brace_error(&self, before: &str) -> ParsingError {
        let token = self.peek();
        ParsingError::ExpectedOpenBrace(token.loc, String::from(before), token.lexeme.to_string())
    }

    fn expected_close_brace_error(&self, after: &str) -> ParsingError {
        let token = self.peek();
        ParsingError::ExpectedCloseBrace(token.loc, String::from(after), token.lexeme.to_string())
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

    fn expected_name_error(&self, kind: &str) -> ParsingError {
        let token = self.peek();
        ParsingError::ExpectedName(token.loc, String::from(kind), token.lexeme.to_string())
    }
}

impl ParsingError {
    fn max_args_exceeded(loc: Loc, kind: &str) -> Self {
        ParsingError::MaximumArgumentsExceeded(loc, String::from(kind))
    }
}
