//! Error types for the Wabbit compiler
//!
//! The strategy is to decouple errors into two categories:
//! - Semantic errors: these are mainly syntax errors ([`SyntaxError`]) and contains actual nature
//!   of the error
//! - Processing errors: these errors are related to a specific pipeline step (e.g.
//!   [`InterpError`], etc.). These typically include a semantic error as well as contextual information
//!   about the error (e.g. [`ErrorContext`]).
//!
//! Note: the `transparent` feature of `thiserror` is critical here. This means that a tokenizer
//! error can seamlessly bubble up through, e.g., the parser and the interpreter and be properly
//! displayed. This is really beautiful! 🤩

use crate::input::ErrorContext;
use crate::model::{BinOpKind, CompOpKind, FuncName, TypeName, UnaryOpKind, VarName};
use crate::tokenizer::Token;
use crate::value::Value;
use thiserror::Error;

/// Syntax errors
#[derive(Error, Debug, PartialEq)]
pub enum SyntaxError {
    #[error("unexpected character '{0}'")]
    UnexpectedCharacter(char),

    #[error("unexpected token: {0}")]
    UnexpectedToken(Token),

    #[error("unexpected end of file")]
    UnexpectedEndOfFile,

    #[error("unknown variable {0}")]
    UnknownVariable(VarName),

    #[error("cannot assign to undeclared variable {0}")]
    AssignToUndeclaredVariable(VarName),

    #[error("cannot assign to const variable {0}")]
    AssignToConst(VarName),

    #[error("unknown type {0}")]
    UnknownType(TypeName),

    #[error("cannot assign to variable {0} of type {1} with value of type {2}")]
    InconsistentType(VarName, TypeName, TypeName),

    #[error("cannot use unset variable {0}")]
    UnsetVariable(VarName),

    #[error("cannot declare variable {0} without type nor value")]
    NoTypeOrValue(VarName),

    #[error("cannot apply unary operator {0} to value {1}")]
    UnaryOpError(UnaryOpKind, Value),

    #[error("cannot apply binary operator {0} to values {1} and {2}")]
    BinOpError(BinOpKind, Value, Value),

    #[error("incompatible types {1} and {2} for operator {0}")]
    BinOpTypeError(BinOpKind, TypeName, TypeName),

    #[error("cannot apply comparison operator {0} to values {1} and {2}")]
    CompOpError(CompOpKind, Value, Value),

    #[error("cannot divide by zero")]
    DivideByZero,

    #[error("cannot use if condition {0} as bool")]
    IfConditionNotBool(Value),

    #[error("cannot use while condition {0} as bool")]
    WhileConditionNotBool(Value),

    #[error("cannot use break outside of loop")]
    UnexpectedBreak,

    #[error("cannot use continue outside of loop")]
    UnexpectedContinue,

    #[error("cannot use return outside of function")]
    UnexpectedReturn,

    #[error("cannot define nested function")]
    UnexpectedFuncDef,

    #[error("unknown function {0}")]
    UnknownFunction(FuncName),

    #[error("wrong number of arguments for function {0}: expected {1}, got {2}")]
    WrongNumberOfArguments(FuncName, usize, usize),

    #[error("cannot pass argument {0} of type {1} with value of type {2}")]
    InconsistentArgumentType(VarName, TypeName, TypeName),

    #[error("reached end of function {0} without return statement")]
    MissingReturnStatement(FuncName),
}

/// Errors generated by the tokenizer
#[derive(Error, Debug, PartialEq)]
pub enum TokenizerError {
    #[error("{1}Syntax error: {0}")]
    SyntaxError(SyntaxError, ErrorContext),
}

/// Errors generated by the parser
#[derive(Error, Debug, PartialEq)]
pub enum ParserError {
    #[error("{1}Syntax error: {0}")]
    SyntaxError(SyntaxError, ErrorContext),

    #[error(transparent)]
    TokenizerError(#[from] TokenizerError),
}

/// Errors generated by the interpreter
#[derive(Error, Debug, PartialEq)]
pub enum InterpError {
    #[error("{1}Syntax error: {0}")]
    SyntaxError(SyntaxError, ErrorContext),

    #[error(transparent)]
    ParserError(#[from] ParserError),
}

/// Errors generated by the LLVM code generator
#[derive(Error, Debug, PartialEq)]
pub enum LlvmCodegenError {
    #[error("{1}Syntax error: {0}")]
    SyntaxError(SyntaxError, ErrorContext),
}