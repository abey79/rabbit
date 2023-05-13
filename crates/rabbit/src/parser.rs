//! Parser for the Wabbit language
//!
//! The parser can be tested using the following commands:
//! ```shell
//! cargo run --bin wabbit -- parser -c 'print 1 + 2;'
//! cargo run --bin wabbit -- parser input.wb
//! ```

use crate::errors::{ParserError, SyntaxError};
use crate::input::{ErrorContext, Input};
use crate::location::{Loc, Span};
use crate::model::{
    BinOpKind, Block, Comp, CompOpKind, Expr, FuncName, Function, NameModel, Param, Stmt, TypeName,
    VarName,
};
use crate::tokenizer::TokenKind::*;
use crate::tokenizer::{Token, TokenKind, Tokenizer};

pub struct Parser<'a> {
    input: &'a Input<'a>,
    tokens: Vec<Token>,
    pos: usize,
    block: Block,
}

pub type Result<T> = std::result::Result<T, ParserError>;

/// Returns the next token if it matches and advances the parser, or returns an error
macro_rules! expect {
    ($self:ident, $kind:pat) => {{
        let token = $self
            .tokens
            .get($self.pos)
            .ok_or_else(|| $self.end_of_file_err())?;

        if matches!(token.kind, $kind) {
            Ok($self.next().unwrap())
        } else {
            Err(ParserError::SyntaxError(
                SyntaxError::UnexpectedToken(token.clone()),
                ErrorContext::new($self.input, token.span),
            ))
        }
    }};
}

/// Returns an `Option<Token>` if the token matches, in which case the parser advances
macro_rules! accept {
    ($self:ident, $kind:pat) => {{
        let token = $self.tokens.get($self.pos);
        if let Some(token) = token {
            if matches!(token.kind, $kind) {
                $self.pos += 1;
                Some(token)
            } else {
                None
            }
        } else {
            None
        }
    }};
}

/// utility function to create a span from two locations if possible
fn sp(start: Option<Loc>, end: Option<Loc>) -> Span {
    match (start, end) {
        (Some(start), Some(end)) => Span { start, end },
        _ => Span::default(),
    }
}

impl<'a> Parser<'a> {
    pub fn parse(input: &'a Input) -> Result<Block> {
        let mut parser = Self::new(input)?;
        parser.run()?;
        Ok(parser.block)
    }
    fn new(input: &'a Input) -> Result<Self> {
        Ok(Self {
            input,
            tokens: Tokenizer::tokenize(input)?,
            pos: 0,
            block: Block::default(),
        })
    }

    fn run(&mut self) -> Result<()> {
        // note: not using parse_block() here due to the lack of braces
        let start = self.start();
        while self.pos < self.tokens.len() {
            let stmt = self.parse_stmt()?;
            self.block.stmts.push(stmt);
        }
        self.block.span = sp(start, self.end());
        Ok(())
    }

    fn peek(&self) -> Option<&TokenKind> {
        self.tokens.get(self.pos).map(|t| &t.kind)
    }

    fn next(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.pos);
        self.pos += 1;
        token
    }

    fn start(&self) -> Option<Loc> {
        self.tokens.get(self.pos).map(|t| t.span.start)
    }

    fn end(&self) -> Option<Loc> {
        self.tokens.get(self.pos - 1).map(|t| t.span.end)
    }

    /// Create an EOF error.
    ///
    /// For this, we create a span based on the end location of the last token.
    fn end_of_file_err(&self) -> ParserError {
        let span = if let Some(token) = self.tokens.last() {
            Span::new(token.span.end, token.span.end)
        } else {
            Span::default()
        };
        ParserError::SyntaxError(
            SyntaxError::UnexpectedEndOfFile,
            ErrorContext::new(self.input, span),
        )
    }

    /// Create an unexpected token error.
    fn unexpected_token_err(&self, token: Token) -> ParserError {
        let span = token.span;
        ParserError::SyntaxError(
            SyntaxError::UnexpectedToken(token),
            ErrorContext::new(self.input, span),
        )
    }

    fn parse_block(&mut self) -> Result<Block> {
        let mut block = Block::default();

        let start = self.start();
        expect!(self, LBrace)?;
        while accept!(self, RBrace).is_none() {
            let stmt = self.parse_stmt()?;
            block.stmts.push(stmt);
        }
        Ok(block.span(sp(start, self.end())))
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        let start_pos = self.pos;
        match self.peek() {
            Some(Print) => self.parse_print_stmt(),
            Some(Break) => self.parse_break_stmt(),
            Some(Continue) => self.parse_continue_stmt(),
            Some(Const) => self.parse_const_stmt(),
            Some(Var) => self.parse_var_stmt(),
            Some(Name(_)) => {
                // here an expr stmt could be confused with an assignment stmt
                let res = self.parse_assign_stmt();
                if res.is_ok() {
                    res
                } else {
                    self.pos = start_pos;
                    self.parse_expr_stmt()
                }
            }
            Some(If) => self.parse_if_stmt(),
            Some(While) => self.parse_while_stmt(),
            Some(Func) => self.parse_func_stmt(),
            Some(Return) => self.parse_return_stmt(),
            Some(_) => self.parse_expr_stmt(),
            None => Err(self.end_of_file_err()),
        }
    }

    fn parse_break_stmt(&mut self) -> Result<Stmt> {
        let start = self.start();
        expect!(self, Break)?;
        expect!(self, Semi)?;
        Ok(Stmt::break_().span(sp(start, self.end())))
    }

    fn parse_continue_stmt(&mut self) -> Result<Stmt> {
        let start = self.start();
        expect!(self, Continue)?;
        expect!(self, Semi)?;
        Ok(Stmt::continue_().span(sp(start, self.end())))
    }

    fn parse_print_stmt(&mut self) -> Result<Stmt> {
        let start = self.start();
        expect!(self, Print)?;
        let expr = self.parse_expr()?;
        expect!(self, Semi)?;
        Ok(Stmt::print(expr).span(sp(start, self.end())))
    }

    fn parse_const_stmt(&mut self) -> Result<Stmt> {
        let start = self.start();
        expect!(self, Const)?;
        let name = self.parse_var_name()?;

        let type_ = if let Some(Name(_)) = self.peek() {
            Some(self.parse_type_name()?)
        } else {
            None
        };

        expect!(self, Assign)?;
        let expr = self.parse_expr()?;
        expect!(self, Semi)?;
        Ok(Stmt::const_def(name, type_, expr).span(sp(start, self.end())))
    }

    fn parse_var_stmt(&mut self) -> Result<Stmt> {
        let start = self.start();
        expect!(self, Var)?;
        let name = self.parse_var_name()?;

        let type_ = if let Some(Name(_)) = self.peek() {
            Some(self.parse_type_name()?)
        } else {
            None
        };

        let expr = if accept!(self, Assign).is_some() {
            Some(self.parse_expr()?)
        } else {
            None
        };

        expect!(self, Semi)?;
        Ok(Stmt::var_def(name, type_, expr).span(sp(start, self.end())))
    }

    fn parse_assign_stmt(&mut self) -> Result<Stmt> {
        let start = self.start();
        let name = self.parse_var_name()?;
        expect!(self, Assign)?;
        let expr = self.parse_expr()?;
        expect!(self, Semi)?;
        Ok(Stmt::assign(name, expr).span(sp(start, self.end())))
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt> {
        let start = self.start();
        expect!(self, If)?;
        let cond = self.parse_expr()?;
        let then_block = self.parse_block()?;

        let else_block = if accept!(self, Else).is_some() {
            let blk = self.parse_block()?;
            Some(blk)
        } else {
            None
        };
        Ok(Stmt::if_(cond, then_block, else_block).span(sp(start, self.end())))
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt> {
        let start = self.start();
        expect!(self, While)?;
        let cond = self.parse_expr()?;
        let block = self.parse_block()?;
        Ok(Stmt::while_(cond, block).span(sp(start, self.end())))
    }

    fn parse_func_stmt(&mut self) -> Result<Stmt> {
        let start = self.start();
        expect!(self, Func)?;
        let name = self.parse_func_name()?;
        expect!(self, LParen)?;
        let mut params = Vec::new();
        let mut param_start = self.start();
        if accept!(self, RParen).is_none() {
            loop {
                let param_name = self.parse_var_name()?;
                let param_type = self.parse_type_name()?;
                params.push(Param::new(param_name, param_type).span(sp(param_start, self.end())));
                param_start = self.start();

                if let Some(token) = self.next() {
                    match token.kind {
                        Comma => continue,
                        RParen => break,
                        _ => {
                            let t = token.clone();
                            return Err(self.unexpected_token_err(t));
                        }
                    }
                } else {
                    return Err(self.end_of_file_err());
                }
            }
        }

        let ret_type = self.parse_type_name()?;
        let block = self.parse_block()?;

        Ok(Stmt::func_def(
            name,
            Function::new(params, ret_type, block).span(sp(start, self.end())),
        )
        .span(sp(start, self.end())))
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt> {
        let start = self.start();
        expect!(self, Return)?;
        let expr = self.parse_expr()?;
        expect!(self, Semi)?;
        Ok(Stmt::return_(expr).span(sp(start, self.end())))
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt> {
        let start = self.start();
        let expr = self.parse_expr()?;
        expect!(self, Semi)?;
        Ok(Stmt::expr(expr).span(sp(start, self.end())))
    }

    fn parse_expr(&mut self) -> Result<Expr> {
        self.parse_or_term()
    }

    fn parse_or_term(&mut self) -> Result<Expr> {
        let mut start = self.start();
        let mut lhs = self.parse_and_term()?;
        while accept!(self, Or).is_some() {
            let rhs = self.parse_and_term()?;
            lhs = Expr::bin_op(BinOpKind::Or, lhs, rhs).span(sp(start, self.end()));
            start = self.start();
        }
        Ok(lhs)
    }

    fn parse_and_term(&mut self) -> Result<Expr> {
        let mut start = self.start();
        let mut lhs = self.parse_rel_term()?;
        while accept!(self, And).is_some() {
            let rhs = self.parse_rel_term()?;
            lhs = Expr::bin_op(BinOpKind::And, lhs, rhs).span(sp(start, self.end()));
            start = self.start();
        }
        Ok(lhs)
    }

    fn parse_rel_term(&mut self) -> Result<Expr> {
        let start = self.start();
        let lhs = self.parse_add_term()?;
        let mut comps = Vec::new();
        let mut comp_start = self.start();
        while let Some(op_token) = accept!(
            self,
            Less | LessEqual | Greater | GreaterEqual | Equal | NotEqual
        ) {
            let op: CompOpKind = op_token.kind.clone().into();
            let right = self.parse_add_term()?;
            comps.push(Comp::new(op, right).span(sp(comp_start, self.end())));
            comp_start = self.start();
        }

        // no spurious "empty" comparisons
        if comps.is_empty() {
            return Ok(lhs);
        }

        Ok(Expr::comp_op(lhs, comps).span(sp(start, self.end())))
    }

    fn parse_add_term(&mut self) -> Result<Expr> {
        let mut start = self.start();
        let mut lhs = self.parse_mul_term()?;
        while let Some(op_token) = accept!(self, Plus | Minus) {
            let op: BinOpKind = op_token.kind.clone().into();
            let rhs = self.parse_mul_term()?;
            lhs = Expr::bin_op(op, lhs, rhs).span(sp(start, self.end()));
            start = self.start();
        }
        Ok(lhs)
    }

    fn parse_mul_term(&mut self) -> Result<Expr> {
        let mut start = self.start();
        let mut lhs = self.parse_factor()?;
        while let Some(op_token) = accept!(self, Star | Slash) {
            let op: BinOpKind = op_token.kind.clone().into();
            let rhs = self.parse_factor()?;
            lhs = Expr::bin_op(op, lhs, rhs).span(sp(start, self.end()));
            start = self.start();
        }
        Ok(lhs)
    }

    fn parse_factor(&mut self) -> Result<Expr> {
        let start_pos = self.pos;
        match self.peek() {
            Some(Int(_)) => self.parse_integer(),
            Some(Float(_)) => self.parse_float(),
            Some(Char(_)) => self.parse_char(),
            Some(Bool(_)) => self.parse_bool(),
            Some(Name(_)) => {
                // this can be either a variable or a function call
                let res = self.parse_func_call();
                if res.is_err() {
                    self.pos = start_pos;
                    self.parse_variable()
                } else {
                    res
                }
            }
            Some(Minus) | Some(Plus) | Some(Not) => self.parse_unary_factor(),
            Some(LParen) => self.parse_grouping(),
            Some(_) => unimplemented!(), //TODO: error handling
            None => Err(self.end_of_file_err()),
        }
    }

    fn parse_func_call(&mut self) -> Result<Expr> {
        let start = self.start();
        let name = self.parse_func_name()?;
        expect!(self, LParen)?;
        let mut args = Vec::new();
        if accept!(self, RParen).is_none() {
            loop {
                let arg = self.parse_expr()?;
                args.push(arg);

                if let Some(token) = self.next() {
                    match token.kind {
                        Comma => continue,
                        RParen => break,
                        _ => {
                            let t = token.clone();
                            return Err(self.unexpected_token_err(t));
                        }
                    }
                } else {
                    return Err(self.end_of_file_err());
                }
            }
        }

        Ok(Expr::func_call(name, args).span(sp(start, self.end())))
    }

    fn parse_grouping(&mut self) -> Result<Expr> {
        expect!(self, LParen)?;
        let expr = self.parse_expr()?;
        expect!(self, RParen)?;
        Ok(expr)
    }

    fn parse_unary_factor(&mut self) -> Result<Expr> {
        let start = self.start();
        let op_token = expect!(self, Plus | Minus | Not)?;
        let op_kind = op_token.kind.clone().into();
        let expr = self.parse_factor()?;
        Ok(Expr::unary_op(op_kind, expr).span(sp(start, self.end())))
    }

    fn parse_integer(&mut self) -> Result<Expr> {
        let start = self.start();
        let tok = expect!(self, Int(_))?;
        if let Int(n) = tok.kind {
            Ok(Expr::integer(n).span(sp(start, self.end())))
        } else {
            unreachable!()
        }
    }

    fn parse_float(&mut self) -> Result<Expr> {
        let start = self.start();
        let tok = expect!(self, Float(_))?;
        if let Float(n) = tok.kind {
            Ok(Expr::float(n).span(sp(start, self.end())))
        } else {
            unreachable!()
        }
    }

    fn parse_char(&mut self) -> Result<Expr> {
        let start = self.start();
        let tok = expect!(self, Char(_))?;
        if let Char(n) = tok.kind {
            Ok(Expr::char(n).span(sp(start, self.end())))
        } else {
            unreachable!()
        }
    }

    fn parse_bool(&mut self) -> Result<Expr> {
        let start = self.start();
        let tok = expect!(self, Bool(_))?;
        if let Bool(n) = tok.kind {
            Ok(Expr::bool(n).span(sp(start, self.end())))
        } else {
            unreachable!()
        }
    }

    fn parse_variable(&mut self) -> Result<Expr> {
        let start = self.start();
        let name = self.parse_var_name()?;
        Ok(Expr::variable(name).span(sp(start, self.end())))
    }

    fn parse_name<T: NameModel>(&mut self) -> Result<T> {
        let start = self.start();
        let tok = expect!(self, Name(_))?;
        if let Name(ref n) = tok.kind {
            Ok(T::new(n.clone()).span(sp(start, self.end())))
        } else {
            unreachable!()
        }
    }

    fn parse_var_name(&mut self) -> Result<VarName> {
        self.parse_name()
    }

    fn parse_type_name(&mut self) -> Result<TypeName> {
        self.parse_name()
    }

    fn parse_func_name(&mut self) -> Result<FuncName> {
        self.parse_name()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::input::ErrorContext;
    use crate::model::StmtKind;

    macro_rules! parse_file {
        ($input:literal) => {{
            Parser::parse(&Input::new(include_str!($input))).unwrap();
        }};
    }

    #[test]
    fn run_parser_tests() {
        parse_file!("../../../tests/Parser/01_break.wb");
        parse_file!("../../../tests/Parser/02_continue.wb");
        parse_file!("../../../tests/Parser/03_print.wb");
        parse_file!("../../../tests/Parser/04_multiple.wb");
        parse_file!("../../../tests/Parser/05_const.wb");
        parse_file!("../../../tests/Parser/06_var.wb");
        parse_file!("../../../tests/Parser/07_if.wb");
        parse_file!("../../../tests/Parser/08_while.wb");
        parse_file!("../../../tests/Parser/09_exprstatement.wb");
        parse_file!("../../../tests/Parser/10_factor.wb");
        parse_file!("../../../tests/Parser/11_term.wb");
        parse_file!("../../../tests/Parser/12_unary.wb");
        parse_file!("../../../tests/Parser/13_sumterm.wb");
        parse_file!("../../../tests/Parser/14_relterm.wb");
        parse_file!("../../../tests/Parser/15_andterm.wb");
        parse_file!("../../../tests/Parser/16_orterm.wb");
        parse_file!("../../../tests/Parser/17_grouping.wb");
    }

    #[test]
    fn test_parse_assign_vs_expr_stmt() {
        // this should properly parse as an expr stmt--not an assign stmt!
        let ast = Parser::parse(&Input::new("a < 1;")).unwrap();
        assert_eq!(
            ast,
            Block::new(vec![Stmt::expr(Expr::comp_op(
                Expr::variable(VarName::new("a".to_string())),
                [Comp::new(CompOpKind::Lt, Expr::integer(1))]
            ))])
        );
    }

    #[test]
    fn test_chained_comparison() {
        let ast = Parser::parse(&Input::new("1 < 2 <= 3;")).unwrap();
        assert_eq!(
            ast,
            Block::new(vec![Stmt::expr(Expr::comp_op(
                Expr::integer(1),
                [
                    Comp::new(CompOpKind::Lt, Expr::integer(2)),
                    Comp::new(CompOpKind::Le, Expr::integer(3)),
                ]
            ))])
        );
    }

    #[test]
    fn test_break_span() {
        let input = Input::new("    break     ;  ");
        let ast = Parser::parse(&input).unwrap();
        assert_eq!(
            ast.stmts[0].span,
            Span::new(Loc::new(1, 5), Loc::new(1, 15))
        );

        let ctx = ErrorContext::new(&input, ast.stmts[0].span);
        println!("{}", ctx);

        let input = Input::new("func hello(arg int, b float) bool { print arg; }");
        let ast = Parser::parse(&input).unwrap();
        if let Stmt {
            kind: StmtKind::FuncDef { ref func, ref name },
            ..
        } = ast.stmts[0]
        {
            println!("{}", ErrorContext::new(&input, name.span));
            println!("{}", ErrorContext::new(&input, func.params[0].type_.span));
            println!("{}", ErrorContext::new(&input, func.params[1].name.span));
            println!("{}", ErrorContext::new(&input, func.return_type.span));
        } else {
            unreachable!()
        }
    }
}
