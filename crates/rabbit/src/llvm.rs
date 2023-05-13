use crate::errors::{LlvmCodegenError, SyntaxError};
use crate::input::{ErrorContext, Input};
use crate::location::Span;
use crate::model::{
    BinOpKind, Block, Comp, CompOpKind, Expr, ExprKind, Stmt, StmtKind, TypeName, UnaryOpKind,
    VarName,
};
use std::collections::HashMap;
use std::fmt::Write;
use std::{fmt, result};

pub type Result<T> = result::Result<T, LlvmCodegenError>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LType {
    I32,
    I8,
    I1,
    Double,
}

// TODO: remove all panics or implement type checker

impl fmt::Display for LType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LType::I32 => write!(f, "i32"),
            LType::I8 => write!(f, "i8"),
            LType::I1 => write!(f, "i1"),
            LType::Double => write!(f, "double"),
        }
    }
}

impl From<&TypeName> for LType {
    fn from(type_name: &TypeName) -> Self {
        match type_name.name.as_str() {
            "int" => LType::I32,
            "bool" => LType::I1,
            "char" => LType::I8,
            "float" => LType::Double,
            _ => panic!("unknown type"),
        }
    }
}

impl LType {
    pub fn init_val(&self) -> &'static str {
        match self {
            LType::Double => "0.0",
            _ => "0",
        }
    }
}

#[derive(Debug, Clone)]
pub struct LValue {
    pub value: String,
    pub type_: TypeName,
}

impl LValue {
    pub fn new(value: impl Into<String>, type_: impl Into<TypeName>) -> Self {
        Self {
            value: value.into(),
            type_: type_.into(),
        }
    }

    pub fn ltype(&self) -> Option<LType> {
        match self.type_.name.as_str() {
            "int" => Some(LType::I32),
            "bool" => Some(LType::I1),
            "char" => Some(LType::I8),
            "float" => Some(LType::Double),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct LlvmCodegen<'a> {
    input: &'a Input<'a>,
    globals: HashMap<String, LValue>,
    code: Vec<String>,
    id: usize,
}

impl<'a> LlvmCodegen<'a> {
    pub fn new(input: &'a Input<'a>) -> Self {
        Self {
            input,
            globals: HashMap::new(),
            code: Vec::new(),
            id: 0,
        }
    }

    fn out<S: Into<String>>(&mut self, s: S) {
        self.code.push(s.into());
    }

    fn err(&self, err: SyntaxError, span: Span) -> LlvmCodegenError {
        LlvmCodegenError::SyntaxError(err, ErrorContext::new(self.input, span))
    }

    fn next_id(&mut self) -> usize {
        let id = self.id;
        self.id += 1;
        id
    }

    fn new_local(&mut self) -> String {
        format!("%r{}", self.next_id())
    }

    fn new_label(&mut self) -> String {
        format!("L{}", self.next_id())
    }

    pub fn run(&mut self, _block: &Block) -> Result<()> {
        self.visit_block(_block)?;
        Ok(())
    }

    pub fn visit_block(&mut self, block: &Block) -> Result<()> {
        for stmt in &block.stmts {
            self.visit_stmt(stmt)?;
        }
        Ok(())
    }

    pub fn generate(&self) -> result::Result<String, fmt::Error> {
        let mut writer = String::new();

        writeln!(writer, "declare void @_print_int(i32 %x)")?;
        writeln!(writer, "declare void @_print_float(double %x)")?;
        writeln!(writer, "declare void @_print_char(i8 %x)")?;
        writeln!(writer, "declare void @_print_bool(i1 %x)")?;

        // declare global variables
        for (name, lvalue) in &self.globals {
            let ltype = lvalue.ltype().unwrap();
            writeln!(writer, "@{} = global {} {}", name, ltype, ltype.init_val())?;
        }

        writeln!(writer, "define void @wabbit_main() {{")?;

        for line in &self.code {
            writeln!(writer, "{line}")?;
        }

        writeln!(writer, "ret void")?;
        writeln!(writer, "}}")?;

        Ok(writer)
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        use StmtKind::*;
        match &stmt.kind {
            VarDef { name, type_, value } => self.visit_var_def_stmt(name, type_, value)?,
            ConstDef { name, type_, value } => self.visit_const_def_stmt(name, type_, value)?,
            Assign { name, value } => self.visit_assign_stmt(name, value)?,
            Print { expr } => self.visit_print_stmt(expr)?,
            If {
                condition,
                then_block,
                else_block,
            } => self.visit_if_stmt(condition, then_block, else_block)?,
            While { condition, block } => self.visit_while_stmt(condition, block)?,
            Expr { expr } => self.visit_expr(expr).map(|_| ())?,
            _ => unimplemented!(),
        }

        Ok(())
    }

    fn visit_var_def_stmt(
        &mut self,
        name: &VarName,
        type_: &Option<TypeName>,
        value: &Option<Expr>,
    ) -> Result<()> {
        let lvalue = if let Some(value) = value {
            let lvalue = self.visit_expr(value)?;
            if let Some(type_) = type_ {
                if lvalue.type_ != *type_ {
                    panic!("type mismatch");
                }
            }
            lvalue
        } else if let Some(type_) = type_ {
            LValue::new(LType::from(type_).init_val(), type_.clone())
        } else {
            panic!("type and value cannot be both None")
        };

        //TODO: figure out scope

        if let Some(_) = self.globals.get(&name.name) {
            panic!("variable already defined");
        }

        let ltype = lvalue.ltype().unwrap();

        self.out(format!(
            "store {} {}, {}* @{}",
            ltype, &lvalue.value, ltype, name.name
        ));

        self.globals.insert(
            name.name.clone(),
            LValue::new(name.name.clone(), lvalue.type_),
        );

        Ok(())
    }

    fn visit_const_def_stmt(
        &mut self,
        name: &VarName,
        type_: &Option<TypeName>,
        value: &Expr,
    ) -> Result<()> {
        let lvalue = self.visit_expr(value)?;
        if let Some(type_) = type_ {
            if lvalue.type_ != *type_ {
                panic!("type mismatch");
            }
        }

        //TODO: figure out scope

        if let Some(_) = self.globals.get(&name.name) {
            panic!("constant already defined");
        }

        let ltype = lvalue.ltype().unwrap();

        self.out(format!(
            "store {} {}, {}* @{}",
            ltype, &lvalue.value, ltype, name.name
        ));

        self.globals.insert(
            name.name.clone(),
            LValue::new(name.name.clone(), lvalue.type_),
        );

        Ok(())
    }

    fn visit_assign_stmt(&mut self, name: &VarName, value: &Expr) -> Result<()> {
        let lvalue = self.visit_expr(value)?;
        let ltype = lvalue.ltype().unwrap();

        // TODO: look in local scope
        if let Some(global_var) = self.globals.get(&name.name) {
            if global_var.type_.name != lvalue.type_.name {
                panic!("type mismatch");
            }
            self.out(format!(
                "store {} {}, {}* @{}",
                ltype, &lvalue.value, ltype, name.name
            ));
        } else {
            panic!("variable not defined");
        }

        Ok(())
    }

    fn visit_print_stmt(&mut self, expr: &Expr) -> Result<()> {
        let value = self.visit_expr(expr)?;
        let func = match value.type_.name.as_str() {
            "int" => Ok("_print_int"),
            "bool" => Ok("_print_bool"),
            "char" => Ok("_print_char"),
            "float" => Ok("_print_float"),
            _ => Err(LlvmCodegenError::SyntaxError(
                SyntaxError::UnknownType(value.type_.clone()),
                ErrorContext::new(self.input, expr.span),
            )),
        }?;
        let ltype = value
            .ltype()
            .ok_or(self.err(SyntaxError::UnknownType(value.type_.clone()), expr.span))?;

        self.out(format!("call void @{}({} {})", func, ltype, value.value));
        Ok(())
    }

    fn visit_if_stmt(
        &mut self,
        cond: &Expr,
        then_block: &Block,
        else_block: &Option<Block>,
    ) -> Result<()> {
        let cond = self.visit_expr(cond)?;
        if cond.type_.name != "bool" {
            panic!("condition must be a boolean");
        }

        let then_label = self.new_label();
        let else_label = self.new_label();
        let end_label = self.new_label();

        self.out(format!(
            "br i1 {}, label %{}, label %{}",
            cond.value, then_label, else_label
        ));

        self.out(format!("{}:", then_label));
        self.visit_block(then_block)?;
        self.out(format!("br label %{}", end_label));

        self.out(format!("{}:", else_label));
        if let Some(else_block) = else_block {
            self.visit_block(else_block)?;
        }
        self.out(format!("br label %{}", end_label));

        self.out(format!("{}:", end_label));

        Ok(())
    }

    fn visit_while_stmt(&mut self, cond: &Expr, block: &Block) -> Result<()> {
        let cond_label = self.new_label();
        let body_label = self.new_label();
        let end_label = self.new_label();

        self.out(format!("br label %{}", cond_label));
        self.out(format!("{}:", cond_label));
        let cond = self.visit_expr(cond)?;
        if cond.type_.name != "bool" {
            panic!("condition must be a boolean");
        }
        self.out(format!(
            "br i1 {}, label %{}, label %{}",
            cond.value, body_label, end_label
        ));

        self.out(format!("{}:", body_label));
        self.visit_block(block)?;
        self.out(format!("br label %{}", cond_label));

        self.out(format!("{}:", end_label));

        Ok(())
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<LValue> {
        use ExprKind::*;
        match &expr.kind {
            Variable(name) => self.visit_variable_expr(name),
            BinOp { op, left, right } => self.visit_bin_op_expr(op, left, right),
            UnaryOp { op, operand: expr } => self.visit_unary_op_expr(op, expr),
            CompOp { left, comps } => self.visit_comp_op_expr(left, comps),
            Integer(value) => Ok(LValue::new(format!("{}", value), "int")),
            Float(value) => Ok(LValue::new(format!("{:?}", value), "float")),
            Char(value) => Ok(LValue::new(
                format!("{}", Into::<u32>::into(*value)),
                "char",
            )),
            Bool(value) => Ok(LValue::new(
                format!("{}", Into::<u32>::into(*value)),
                "bool",
            )),
            _ => unimplemented!(),
        }
    }

    fn visit_variable_expr(&mut self, name: &VarName) -> Result<LValue> {
        //TODO: look in various scopes
        let lvalue = self
            .globals
            .get(&name.name)
            .ok_or_else(|| self.err(SyntaxError::UnknownVariable(name.clone()), name.span))?
            .clone();

        let ltype = lvalue.ltype().unwrap();
        let out_val = self.new_local();

        self.out(format!(
            "{0} = load {1}, {1}* @{2}",
            out_val, ltype, lvalue.value
        ));
        Ok(LValue::new(out_val, lvalue.type_))
    }

    fn visit_bin_op_expr(&mut self, op: &BinOpKind, left: &Expr, right: &Expr) -> Result<LValue> {
        let left_val = self.visit_expr(left)?;
        let right_val = self.visit_expr(right)?;

        let ltype = left_val
            .ltype()
            .ok_or(self.err(SyntaxError::UnknownType(left_val.type_.clone()), left.span))?;

        let rtype = right_val.ltype().ok_or(self.err(
            SyntaxError::UnknownType(right_val.type_.clone()),
            right.span,
        ))?;

        if ltype != rtype {
            return Err(self.err(
                SyntaxError::BinOpTypeError(*op, left_val.type_.clone(), right_val.type_.clone()),
                Span::new(left.span.start, right.span.end),
            ));
        }

        let ltype = left_val
            .ltype()
            .ok_or(self.err(SyntaxError::UnknownType(left_val.type_.clone()), left.span))?;

        let op = match ltype {
            LType::I32 | LType::I8 => match op {
                BinOpKind::Add => "add",
                BinOpKind::Sub => "sub",
                BinOpKind::Mul => "mul",
                BinOpKind::Div => "sdiv",
                _ => panic!("unimplemented"), //TODO: error handling
            },
            LType::Double => match op {
                BinOpKind::Add => "fadd",
                BinOpKind::Sub => "fsub",
                BinOpKind::Mul => "fmul",
                BinOpKind::Div => "fdiv",
                _ => panic!("unimplemented"), //TODO: error handling
            },
            LType::I1 => match op {
                BinOpKind::And => "and",
                BinOpKind::Or => "or",
                _ => panic!("unimplemented"), //TODO: error handling
            },
        };

        let var = self.new_local();
        self.out(format!(
            "{} = {} {} {}, {}",
            var, op, ltype, left_val.value, right_val.value
        ));
        Ok(LValue::new(var, left_val.type_.clone()))
    }

    fn visit_unary_op_expr(&mut self, op: &UnaryOpKind, expr: &Expr) -> Result<LValue> {
        let val = self.visit_expr(expr)?;
        let ltype = val
            .ltype()
            .ok_or(self.err(SyntaxError::UnknownType(val.type_.clone()), expr.span))?;

        let (op, cst) = match ltype {
            LType::I32 | LType::I8 => match op {
                UnaryOpKind::Pos => return Ok(val),
                UnaryOpKind::Neg => ("sub", "0"),
                UnaryOpKind::Not => panic!("unimplemented"), //TODO: error handling
            },
            LType::Double => match op {
                UnaryOpKind::Pos => return Ok(val),
                UnaryOpKind::Neg => ("fsub", "0.0"),
                UnaryOpKind::Not => panic!("unimplemented"), //TODO: error handling
            },
            LType::I1 => match op {
                UnaryOpKind::Not => ("xor", "1"),
                UnaryOpKind::Pos | UnaryOpKind::Neg => panic!("unimplemented"), //TODO: error handling
            },
        };

        let var = self.new_local();
        self.out(format!("{} = {} {} {}, {}", var, op, ltype, cst, val.value));
        Ok(LValue::new(var, val.type_.clone()))
    }

    fn visit_comp_op_expr(&mut self, left: &Expr, comps: &Vec<Comp>) -> Result<LValue> {
        let left_val = self.visit_expr(left)?;

        //TODO: implement chain of comparisons
        if comps.len() != 1 {
            panic!("comparison chaining unimplemented");
        }

        let comp = &comps[0];

        let right_val = self.visit_expr(&comp.right)?;

        if left_val.type_ != right_val.type_ {
            panic!("comparison between different types");
        }

        let ltype = left_val
            .ltype()
            .ok_or(self.err(SyntaxError::UnknownType(left_val.type_.clone()), left.span))?;

        let op = match ltype {
            LType::I32 | LType::I8 | LType::I1 => match comp.op {
                CompOpKind::Eq => "icmp eq",
                CompOpKind::Lt => "icmp slt",
                CompOpKind::Le => "icmp sle",
                CompOpKind::Gt => "icmp sgt",
                CompOpKind::Ge => "icmp sge",
                CompOpKind::Ne => "icmp ne",
            },
            LType::Double => match comp.op {
                CompOpKind::Eq => "fcmp oeq",
                CompOpKind::Lt => "fcmp olt",
                CompOpKind::Le => "fcmp ole",
                CompOpKind::Gt => "fcmp ogt",
                CompOpKind::Ge => "fcmp oge",
                CompOpKind::Ne => "fcmp one",
            },
        };

        let var = self.new_local();
        self.out(format!(
            "{} = {} {} {}, {}",
            var, op, ltype, left_val.value, right_val.value
        ));
        Ok(LValue::new(var, "bool"))
    }
}
