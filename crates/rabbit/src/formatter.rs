//! Formatter for the Wabbit language.
//!
//! The formatter can be tested using the following commands:
//! ```shell
//! cargo run --bin wabbit -- format -c 'print 1 + 2;'
//! cargo run --bin wabbit -- format input.wb
//! ```

// inspiration: https://rust-unofficial.github.io/patterns/patterns/behavioural/visitor.html

use crate::model::{
    Block, Comp, Expr, ExprKind, FuncName, Param, Stmt, StmtKind, TypeName, VarName,
};
use std::fmt;

struct Formatter<'a, 'b: 'a> {
    f: &'a mut fmt::Formatter<'b>,
    indent: usize,
}

impl Formatter<'_, '_> {
    pub fn visit_expr_paren(&mut self, expr: &Expr) -> fmt::Result {
        write!(self.f, "(")?;
        self.visit_expr(expr)?;
        write!(self.f, ")")
    }

    fn indent(&mut self) -> fmt::Result {
        for _ in 0..self.indent {
            write!(self.f, "    ")?;
        }
        Ok(())
    }

    fn visit_var_name(&mut self, name: &VarName) -> fmt::Result {
        self.f.write_str(&name.name)
    }

    fn visit_type_name(&mut self, name: &TypeName) -> fmt::Result {
        self.f.write_str(&name.name)
    }

    fn visit_func_name(&mut self, name: &FuncName) -> fmt::Result {
        self.f.write_str(&name.name)
    }

    fn visit_expr(&mut self, expr: &Expr) -> fmt::Result {
        match &expr.kind {
            ExprKind::Variable(ident) => self.visit_var_name(ident),
            ExprKind::UnaryOp { op, operand } => {
                let op_precedence = op.precedence();
                let operand_precedence = operand.kind.precedence();
                write!(self.f, "{}", op)?;

                if operand_precedence < op_precedence {
                    self.visit_expr_paren(operand)
                } else {
                    self.visit_expr(operand)
                }
            }
            ExprKind::BinOp { op, left, right } => {
                let left_precedence = left.kind.precedence();
                let right_precedence = right.kind.precedence();
                let op_precedence = op.precedence();
                if left_precedence < op_precedence {
                    self.visit_expr_paren(left)?;
                } else {
                    self.visit_expr(left)?;
                }
                write!(self.f, " {} ", op)?;
                if right_precedence < op_precedence {
                    self.visit_expr_paren(right)
                } else {
                    self.visit_expr(right)
                }
            }
            ExprKind::CompOp { left, comps } => {
                let left_precedence = left.kind.precedence();
                let comp_precedence = expr.kind.precedence();
                if left_precedence < comp_precedence {
                    self.visit_expr_paren(left)?;
                } else {
                    self.visit_expr(left)?;
                }
                for Comp { op, right, .. } in comps {
                    let right_precedence = right.kind.precedence();

                    write!(self.f, " {} ", op)?;
                    if right_precedence < comp_precedence {
                        self.visit_expr_paren(right)?;
                    } else {
                        self.visit_expr(right)?;
                    }
                }
                Ok(())
            }
            ExprKind::FuncCall { name, args } => {
                self.visit_func_name(name)?;
                write!(self.f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(self.f, ", ")?;
                    }
                    self.visit_expr(arg)?;
                }
                write!(self.f, ")")
            }
            ExprKind::Integer(n) => write!(self.f, "{}", n),
            ExprKind::Float(n) => write!(self.f, "{:?}", n),
            ExprKind::Char(c) => write!(self.f, "'{}'", c),
            ExprKind::Bool(b) => write!(self.f, "{}", b),
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> fmt::Result {
        match &stmt.kind {
            StmtKind::ConstDef { name, type_, value } => {
                self.indent()?;
                write!(self.f, "const ")?;
                self.visit_var_name(name)?;

                if let Some(type_) = type_ {
                    write!(self.f, " ")?;
                    self.visit_type_name(type_)?;
                }
                write!(self.f, " = ")?;
                self.visit_expr(value)?;
                writeln!(self.f, ";")
            }
            StmtKind::VarDef { name, type_, value } => {
                self.indent()?;
                write!(self.f, "var ")?;
                self.visit_var_name(name)?;

                if let Some(type_) = type_ {
                    write!(self.f, " ")?;
                    self.visit_type_name(type_)?;
                }
                if let Some(value) = value {
                    write!(self.f, " = ")?;
                    self.visit_expr(value)?;
                }
                writeln!(self.f, ";")
            }
            StmtKind::Assign { name, value } => {
                self.indent()?;
                self.visit_var_name(name)?;
                write!(self.f, " = ")?;
                self.visit_expr(value)?;
                writeln!(self.f, ";")
            }
            StmtKind::Print { expr } => {
                self.indent()?;
                write!(self.f, "print ")?;
                self.visit_expr(expr)?;
                writeln!(self.f, ";")
            }
            StmtKind::If {
                condition,
                then_block,
                else_block,
            } => {
                self.indent()?;
                write!(self.f, "if ")?;
                self.visit_expr(condition)?;
                writeln!(self.f, " {{")?;
                self.indent += 1;
                self.visit_block(then_block)?;
                self.indent -= 1;
                write!(self.f, "}}")?;
                if let Some(else_block) = else_block {
                    writeln!(self.f, " else {{")?;
                    self.indent += 1;
                    self.visit_block(else_block)?;
                    self.indent -= 1;
                    write!(self.f, "}}")?;
                }
                writeln!(self.f)
            }
            StmtKind::While { condition, block } => {
                self.indent()?;
                write!(self.f, "while ")?;
                self.visit_expr(condition)?;
                writeln!(self.f, " {{")?;
                self.indent += 1;
                self.visit_block(block)?;
                self.indent -= 1;
                writeln!(self.f, "}}")
            }
            StmtKind::Break => {
                self.indent()?;
                writeln!(self.f, "break;")
            }
            StmtKind::Continue => {
                self.indent()?;
                writeln!(self.f, "continue;")
            }
            StmtKind::Expr { expr } => {
                self.indent()?;
                self.visit_expr(expr)?;
                writeln!(self.f, ";")
            }
            StmtKind::FuncDef { name, func } => {
                self.indent()?;
                write!(self.f, "func ")?;
                self.visit_func_name(name)?;
                write!(self.f, "(")?;
                for (i, Param { name, type_, .. }) in func.params.iter().enumerate() {
                    if i > 0 {
                        write!(self.f, ", ")?;
                    }
                    self.visit_var_name(name)?;
                    write!(self.f, " ")?;
                    self.visit_type_name(type_)?;
                }
                write!(self.f, ") ")?;
                self.visit_type_name(&func.return_type)?;
                writeln!(self.f, " {{")?;
                self.indent += 1;
                self.visit_block(&func.block)?;
                self.indent -= 1;
                writeln!(self.f, "}}\n")
            }
            StmtKind::Return { expr } => {
                self.indent()?;
                write!(self.f, "return ")?;
                self.visit_expr(expr)?;
                writeln!(self.f, ";")
            }
        }
    }

    fn visit_block(&mut self, block: &Block) -> fmt::Result {
        for statement in &block.stmts {
            self.visit_stmt(statement)?;
        }
        Ok(())
    }
}

// hook the formatter to Block's display
impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut formatter = Formatter { f, indent: 0 };
        formatter.visit_block(self)
    }
}
