use crate::errors::{InterpError, SyntaxError};
use crate::input::{ErrorContext, Input};
use crate::location::Span;
use crate::model::{
    BinOpKind, Block, Comp, CompOpKind, Expr, ExprKind, FuncName, Function, Stmt, StmtKind,
    TypeName, UnaryOpKind, VarName,
};
use crate::parser::Parser;
use crate::value::Value;
use std::collections::HashMap;
use std::io::{stdout, Stdout, Write};

// everything in this file uses [`InterpError`]  as error type
type Result<T> = std::result::Result<T, InterpError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Variable {
    Const(Value),
    Var(TypeName, Option<Value>),
}

impl Variable {
    pub fn type_(&self) -> TypeName {
        match self {
            Self::Const(v) => v.type_(),
            Self::Var(t, _) => t.clone(),
        }
    }

    pub fn is_type(&self, t: &TypeName) -> bool {
        match self {
            Self::Const(v) => v.is_type(t),
            Self::Var(vt, _) => vt == t,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct StmtOptions {
    allow_break: bool,
    allow_continue: bool,
    allow_return: bool,
    allow_func_def: bool,
}

impl Default for StmtOptions {
    fn default() -> Self {
        Self {
            allow_break: false,
            allow_continue: false,
            allow_return: false,
            allow_func_def: true,
        }
    }
}

impl StmtOptions {
    pub fn in_loop(self) -> Self {
        Self {
            allow_break: true,
            allow_continue: true,
            allow_func_def: false,
            ..self
        }
    }

    pub fn in_func(self) -> Self {
        Self {
            allow_return: true,
            allow_func_def: false,
            ..self
        }
    }

    pub fn in_if(self) -> Self {
        Self {
            allow_func_def: false,
            ..self
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtResult {
    None,
    Continue,
    Break,
    Return(Value),
}

pub struct Interp<'a, W: Write = Stdout> {
    input: &'a Input<'a>,
    globals: HashMap<String, Variable>,
    functions: HashMap<String, &'a Function>,
    locals: Vec<HashMap<String, Variable>>,
    writer: W,
}

const BLANK: Input = Input::new("");

// this is for "normal" use, with output to stdout
impl<'a> Interp<'a, Stdout> {
    pub fn interp(input: &'a Input) -> Result<()> {
        let block = Parser::parse(input)?;
        let mut interp = Self::new(input);
        interp.run(&block)
    }

    pub fn interp_block(block: &Block) -> Result<()> {
        let mut interp = Self::new(&BLANK);
        interp.run(block)
    }

    fn new(input: &'a Input<'a>) -> Self {
        Self {
            input,
            globals: HashMap::new(),
            functions: HashMap::new(),
            locals: Vec::new(),
            writer: stdout(),
        }
    }
}

// this is for testing, with output to a `String`
impl<'a> Interp<'a, Vec<u8>> {
    pub fn interp_to_string(input: &'a Input) -> Result<String> {
        let block = Parser::parse(input)?;
        let mut interp = Self::with_writer(input, Vec::<u8>::new());
        interp.run(&block)?;
        Ok(String::from_utf8(interp.writer).unwrap())
    }

    fn with_writer(input: &'a Input, writer: Vec<u8>) -> Self {
        Self {
            input,
            globals: HashMap::new(),
            functions: HashMap::new(),
            locals: Vec::new(),
            writer,
        }
    }
}

impl<'a, W: Write> Interp<'a, W> {
    pub fn run(&mut self, block: &'a Block) -> Result<()> {
        self.visit_block(block, StmtOptions::default())?;
        Ok(())
    }

    /// Build an [`InterpError`] from a [`SyntaxError`] and a [`Span`].
    fn make_err(&self, err: SyntaxError, span: Span) -> InterpError {
        InterpError::SyntaxError(err, ErrorContext::new(self.input, span))
    }

    /// Build an [`InterpError`] from a [`SyntaxError`] and a [`Span`] and return it as a
    /// [`Result`].
    ///
    /// This function is intended as a shorthand for returning an error that will be displayed with
    /// suitable context of the user.
    fn err<T>(&self, err: SyntaxError, span: Span) -> std::result::Result<T, InterpError> {
        Err(self.make_err(err, span))
    }

    /// Define a variable in the current scope.
    fn define(&mut self, name: &'a VarName, variable: Variable) -> Result<()> {
        if let Some(locals) = self.locals.last_mut() {
            locals.insert(name.name.clone(), variable);
        } else {
            self.globals.insert(name.name.clone(), variable);
        }
        Ok(())
    }

    /// Assign a value to a variable in the current scope (or global scope).
    fn assign(&mut self, name: &'a VarName, value: Value, value_span: Span) -> Result<()> {
        let scope = if let Some(locals) = self.locals.last_mut() {
            if locals.contains_key(&name.name) {
                locals
            } else {
                &mut self.globals
            }
        } else {
            &mut self.globals
        };

        if scope.contains_key(&name.name) {
            let entry = scope.get(&name.name).unwrap();
            match entry {
                Variable::Const(_) => {
                    return self.err(SyntaxError::AssignToConst(name.clone()), name.span)
                }
                Variable::Var(t, _) => {
                    if !value.is_type(t) {
                        let t = t.clone();
                        return self.err(
                            SyntaxError::InconsistentType(name.clone(), t, value.type_()),
                            value_span,
                        );
                    } else {
                        scope.insert(name.name.clone(), Variable::Var(t.clone(), Some(value)));
                    }
                }
            }

            Ok(())
        } else {
            self.err(
                SyntaxError::AssignToUndeclaredVariable(name.clone()),
                name.span,
            )
        }
    }

    fn visit_variable(&mut self, name: &'a VarName) -> Result<Value> {
        let scope = if let Some(locals) = self.locals.last() {
            if locals.contains_key(&name.name) {
                locals
            } else {
                &self.globals
            }
        } else {
            &self.globals
        };

        if let Some(var) = scope.get(&name.name) {
            match var {
                Variable::Const(v) => Ok(v.clone()),
                Variable::Var(_, Some(v)) => Ok(v.clone()),
                Variable::Var(_, None) => {
                    self.err(SyntaxError::UnsetVariable(name.clone()), name.span)
                }
            }
        } else {
            self.err(SyntaxError::UnknownVariable(name.clone()), name.span)
        }
    }

    fn visit_expr(&mut self, expr: &'a Expr) -> Result<Value> {
        match &expr.kind {
            ExprKind::Variable(ident) => self.visit_variable(ident),
            ExprKind::BinOp { op, left, right } => {
                let left = self.visit_expr(left)?;
                let right = self.visit_expr(right)?;
                let result = match op {
                    BinOpKind::Add => left.add(&right),
                    BinOpKind::Sub => left.sub(&right),
                    BinOpKind::Mul => left.mul(&right),
                    BinOpKind::Div => left.div(&right),
                    BinOpKind::Or => left.or(&right),
                    BinOpKind::And => left.and(&right),
                };
                result.ok_or(self.make_err(SyntaxError::BinOpError(*op, left, right), expr.span))
            }
            ExprKind::UnaryOp { op, operand } => {
                let value = self.visit_expr(operand)?;
                let result = match op {
                    UnaryOpKind::Pos => value.pos(),
                    UnaryOpKind::Neg => value.neg(),
                    UnaryOpKind::Not => value.not(),
                };
                result.ok_or(self.make_err(SyntaxError::UnaryOpError(*op, value), expr.span))
            }
            ExprKind::CompOp { left, comps } => {
                let mut left = self.visit_expr(left)?;
                for Comp { op, right, span } in comps {
                    let right = self.visit_expr(right)?;
                    let left_prev = left.clone();
                    left = match op {
                        CompOpKind::Lt => left.lt(&right),
                        CompOpKind::Le => left.le(&right),
                        CompOpKind::Gt => left.gt(&right),
                        CompOpKind::Ge => left.ge(&right),
                        CompOpKind::Eq => left.eq(&right),
                        CompOpKind::Ne => left.ne(&right),
                    }
                    .ok_or(self.make_err(SyntaxError::CompOpError(*op, left_prev, right), *span))?;
                }
                Ok(left)
            }
            ExprKind::FuncCall { name, args } => self.visit_func_call(name, args),
            ExprKind::Integer(val) => Ok(Value::Int(*val)),
            ExprKind::Float(val) => Ok(Value::Float(*val)),
            ExprKind::Char(val) => Ok(Value::Char(*val)),
            ExprKind::Bool(val) => Ok(Value::Bool(*val)),
        }
    }

    fn visit_func_call(&mut self, name: &'a FuncName, args: &'a Vec<Expr>) -> Result<Value> {
        // here I'm running into borrow checker issues, as I concurrently need to act on the
        // interpreter AND the "context" (function list, stack, etc.). Addressing this properly
        // probably requires a better design (split interpreter and state objects).
        // Anyway, this is why is start by evaluating all arguments...

        // evaluate all arguments
        let mut values = Vec::new();
        for arg in args {
            values.push(self.visit_expr(arg)?);
        }

        // find function
        let func = self
            .functions
            .get(&name.name)
            .ok_or_else(|| self.make_err(SyntaxError::UnknownFunction(name.clone()), name.span))?;

        // check argument number
        if func.params.len() != args.len() {
            return self.err(
                SyntaxError::WrongNumberOfArguments(name.clone(), func.params.len(), args.len()),
                name.span,
            );
        }

        // check argument types, create scope and populate it
        let mut scope = HashMap::new();
        for (i, (param, value)) in func.params.iter().zip(values.iter()).enumerate() {
            if !value.is_type(&param.type_) {
                return self.err(
                    SyntaxError::InconsistentArgumentType(
                        param.name.clone(),
                        param.type_.clone(),
                        value.type_(),
                    ),
                    args[i].span,
                );
            }

            scope.insert(
                param.name.name.clone(),
                Variable::Var(param.type_.clone(), Some(value.clone())),
            );
        }

        // moved here to keep borrow checker happy
        let end_func_pos = func.block.span.end;

        // add scope to stack and evaluate function body
        self.locals.push(scope);
        let res = self.visit_block(&func.block, StmtOptions::default().in_func());
        self.locals.pop();

        if let Ok(res) = res {
            match res {
                StmtResult::Return(value) => Ok(value),
                _ => self.err(
                    SyntaxError::MissingReturnStatement(name.clone()),
                    Span::new(end_func_pos, end_func_pos),
                ),
            }
        } else {
            Err(res.err().unwrap())
        }
    }

    fn visit_stmt(&mut self, stmt: &'a Stmt, opt: StmtOptions) -> Result<StmtResult> {
        match &stmt.kind {
            StmtKind::ConstDef {
                name,
                type_,
                value: expr,
            } => {
                let value = self.visit_expr(expr)?;
                if let Some(type_) = type_ {
                    if !value.is_type(type_) {
                        return self.err(
                            SyntaxError::InconsistentType(
                                name.clone(),
                                type_.clone(),
                                value.type_(),
                            ),
                            expr.span,
                        );
                    }
                }

                self.define(name, Variable::Const(value))?;
                Ok(StmtResult::None)
            }
            StmtKind::VarDef {
                name,
                type_,
                value: expr,
            } => {
                let value = if let Some(value) = expr {
                    Some(self.visit_expr(value)?)
                } else {
                    None
                };

                let variable = match (type_, value) {
                    (Some(type_), Some(value)) => {
                        if !value.is_type(type_) {
                            return self.err(
                                SyntaxError::InconsistentType(
                                    name.clone(),
                                    type_.clone(),
                                    value.type_(),
                                ),
                                expr.as_ref().expect("exists since value was computed").span,
                            );
                        }

                        Variable::Var(type_.clone(), Some(value))
                    }
                    (Some(type_), None) => Variable::Var(type_.clone(), None),
                    (None, Some(value)) => Variable::Var(value.type_(), Some(value)),
                    (None, None) => {
                        return self.err(SyntaxError::NoTypeOrValue(name.clone()), name.span);
                    }
                };

                self.define(name, variable)?;
                Ok(StmtResult::None)
            }
            StmtKind::Assign { name, value } => {
                let val = self.visit_expr(value)?;
                self.assign(name, val, value.span)?;
                Ok(StmtResult::None)
            }
            StmtKind::Print { expr } => {
                let value = self.visit_expr(expr)?;

                // custom writing code for proper handling of line breaks (not for char!)
                match value {
                    Value::Int(i) => writeln!(&mut self.writer, "{}", i).unwrap(),
                    Value::Float(fl) => writeln!(&mut self.writer, "{:?}", fl).unwrap(),
                    Value::Char(c) => write!(&mut self.writer, "{}", c).unwrap(),
                    Value::Bool(b) => writeln!(&mut self.writer, "{}", b).unwrap(),
                }
                Ok(StmtResult::None)
            }
            StmtKind::If {
                condition,
                then_block,
                else_block,
            } => {
                let value = self.visit_expr(condition)?;

                if let Value::Bool(v) = value {
                    if v {
                        self.visit_block(then_block, opt.in_if())
                    } else if let Some(else_block) = else_block {
                        self.visit_block(else_block, opt.in_if())
                    } else {
                        Ok(StmtResult::None)
                    }
                } else {
                    self.err(SyntaxError::IfConditionNotBool(value), condition.span)
                }
            }
            StmtKind::While { condition, block } => loop {
                let value = self.visit_expr(condition)?;

                if let Value::Bool(v) = value {
                    if v {
                        let res = self.visit_block(block, opt.in_loop())?;
                        match res {
                            StmtResult::Break => return Ok(StmtResult::None),
                            StmtResult::Return(value) => return Ok(StmtResult::Return(value)),
                            _ => {}
                        }
                    } else {
                        return Ok(StmtResult::None);
                    }
                } else {
                    return self.err(SyntaxError::WhileConditionNotBool(value), condition.span);
                }
            },
            StmtKind::Break => {
                if opt.allow_break {
                    Ok(StmtResult::Break)
                } else {
                    self.err(SyntaxError::UnexpectedBreak, stmt.span)
                }
            }
            StmtKind::Continue => {
                if opt.allow_continue {
                    Ok(StmtResult::Continue)
                } else {
                    self.err(SyntaxError::UnexpectedContinue, stmt.span)
                }
            }
            StmtKind::Expr { expr } => {
                // evaluate the expression and throw away the result
                self.visit_expr(expr)?;
                Ok(StmtResult::None)
            }
            StmtKind::FuncDef { name, func } => {
                if opt.allow_func_def {
                    self.functions.insert(name.name.clone(), func);
                    Ok(StmtResult::None)
                } else {
                    self.err(SyntaxError::UnexpectedFuncDef, name.span)
                }
            }
            StmtKind::Return { expr } => {
                if opt.allow_return {
                    let value = self.visit_expr(expr)?;
                    Ok(StmtResult::Return(value))
                } else {
                    self.err(SyntaxError::UnexpectedReturn, stmt.span)
                }
            }
        }
    }

    fn visit_block(&mut self, block: &'a Block, opt: StmtOptions) -> Result<StmtResult> {
        for stmt in &block.stmts {
            let res = self.visit_stmt(stmt, opt)?;
            match res {
                StmtResult::None => {}
                _ => return Ok(res),
            }
        }

        Ok(StmtResult::None)
    }

    // This version of visit_block doesn't stop after a failed statement, so it shows as many errors
    // as possible

    // fn visit_block(
    //     &mut self,
    //     block: &'a Block,
    //     opt: StmtOptions,
    // ) -> Result<StmtResult, InterpError> {
    //     for stmt in &block.stmts {
    //         let res = self.visit_stmt(stmt, opt);
    //
    //         if let Err(e) = res {
    //             println!("Error: {}\n", e);
    //         } else {
    //             match res {
    //                 Ok(StmtResult::None) => {}
    //                 Ok(_) => return res,
    //                 _ => return Ok(StmtResult::None),
    //             }
    //         }
    //     }
    //
    //     Ok(StmtResult::None)
    // }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_var_def_valid() {
        let block = Block::new(vec![
            Stmt::var_def("x", None, Some(Expr::integer(42))),
            Stmt::var_def("y", Some("int".into()), Some(Expr::integer(43))),
            Stmt::var_def("z", Some("float".into()), None),
            Stmt::const_def("w", None, Expr::integer(44)),
            Stmt::const_def("w", Some("float".into()), Expr::float(4.4)),
        ]);

        Interp::interp_block(&block).unwrap();
    }

    fn assert_syntax_error(ierr: InterpError, err: SyntaxError) {
        assert!(matches!(ierr, InterpError::SyntaxError(_, _)));

        if let InterpError::SyntaxError(e, _) = ierr {
            assert_eq!(e, err);
        }
    }

    #[test]
    fn test_var_def_conflict() {
        let block = Block::new(vec![Stmt::var_def(
            "x",
            Some("float".into()),
            Some(Expr::integer(42)),
        )]);

        assert_syntax_error(
            Interp::interp_block(&block).unwrap_err(),
            SyntaxError::InconsistentType("x".into(), "float".into(), "int".into()),
        );
    }

    #[test]
    fn test_const_def_conflict() {
        let block = Block::new(vec![Stmt::const_def(
            "x",
            Some("float".into()),
            Expr::integer(42),
        )]);

        assert_syntax_error(
            Interp::interp_block(&block).unwrap_err(),
            SyntaxError::InconsistentType("x".into(), "float".into(), "int".into()),
        );
    }

    #[test]
    fn test_var_def_no_type_nor_value() {
        let block = Block::new(vec![Stmt::var_def("x", None, None)]);

        assert_syntax_error(
            Interp::interp_block(&block).unwrap_err(),
            SyntaxError::NoTypeOrValue("x".into()),
        );
    }

    #[test]
    fn test_var_type_ok() {
        let block = Block::new(vec![
            Stmt::var_def("x", Some("int".into()), None),
            Stmt::assign("x", Expr::integer(42)),
            Stmt::var_def("y", None, Some(Expr::float(3.2))),
            Stmt::assign("y", Expr::float(3.2)),
            Stmt::var_def("z", Some("char".into()), Some(Expr::char('a'))),
            Stmt::assign("z", Expr::char('b')),
        ]);

        Interp::interp_block(&block).unwrap();
    }

    #[test]
    fn test_var_type_conflict_uninit() {
        let block = Block::new(vec![
            Stmt::var_def("x", Some("int".into()), None),
            Stmt::assign("x", Expr::float(3.2)),
        ]);

        assert_syntax_error(
            Interp::interp_block(&block).unwrap_err(),
            SyntaxError::InconsistentType("x".into(), "int".into(), "float".into()),
        );
    }

    #[test]
    fn test_var_type_conflict_init() {
        let block = Block::new(vec![
            Stmt::var_def("x", None, Some(Expr::integer(42))),
            Stmt::assign("x", Expr::float(3.2)),
        ]);

        assert_syntax_error(
            Interp::interp_block(&block).unwrap_err(),
            SyntaxError::InconsistentType("x".into(), "int".into(), "float".into()),
        );
    }

    #[test]
    fn test_const_assignment() {
        let block = Block::new(vec![
            Stmt::const_def("x", None, Expr::integer(42)),
            Stmt::assign("x", Expr::integer(43)),
        ]);

        assert_syntax_error(
            Interp::interp_block(&block).unwrap_err(),
            SyntaxError::AssignToConst("x".into()),
        );
    }

    #[test]
    fn test_interp_to_string() {
        let s = Interp::interp_to_string(&Input::new("print 42;")).unwrap();
        assert_eq!(s, "42\n");
    }

    //macro_rules! files

    #[test]
    fn test_continue() {
        let prog = Input::new(
            "
            var n int = 0;
            while true {
                n = n + 1;
                if n == 1 {
                    continue;
                }
                print n;
                
                if n == 3 {
                    break;
                }
            }
        ",
        );

        Interp::interp(&prog).unwrap();
    }

    #[test]
    fn test_scopes() {
        let prog = Input::new(
            "
            const x = 6;
            var y = 7;
            func a() int { var x = 3; return x; }
            func b() int { return x * a(); }
            func c() int { var x = 5; return x * b(); }
            func d() int { return y * c(); }
            print d();
        ",
        );

        assert_eq!(Interp::interp_to_string(&prog).unwrap(), "630\n");
    }

    #[test]
    #[should_panic] //TODO: implement proper block scoping
    fn test_block_scope() {
        let prog =
            Input::new("func f() int { var x = 1; if true { var x = 2; }return x;} print f();");
        assert_eq!(Interp::interp_to_string(&prog).unwrap(), "1\n");
    }

    #[test]
    fn test_programs() {
        macro_rules! test_program {
            ($name:literal) => {
                assert_eq!(
                    Interp::interp_to_string(&Input::new(include_str!(concat!(
                        "../../../tests/Programs/",
                        $name,
                        ".wb"
                    ))))
                    .unwrap(),
                    include_str!(concat!(
                        "../tests/fixtures/programs/",
                        $name,
                        ".wb.expected"
                    ))
                );
            };
        }

        test_program!("00_intliteral");
        test_program!("01_intbinop");
        test_program!("02_intunaryop");
        test_program!("03_intvar");
        test_program!("04_floatliteral");
        test_program!("05_floatbinop");
        test_program!("06_floatunaryop");
        test_program!("07_floatvar");
        test_program!("08_intrel");
        test_program!("09_floatrel");
        test_program!("10_bool");
        test_program!("11_cond");
        test_program!("12_loop");
        test_program!("13_charliteral");
        test_program!("14_charrel");
        test_program!("16_brk");
        // test_program!("17_shortcircuit");
        test_program!("20_square");
        test_program!("21_sqrt");
        // test_program!("24_conversions");
        //TODO: fix these

        let slow = false;
        if slow {
            test_program!("15_mandel");
            test_program!("22_fib");
            test_program!("23_mandel");
        }
    }
}
