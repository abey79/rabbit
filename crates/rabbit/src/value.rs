use crate::model::{NameModel, TypeName};
use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i32),
    Float(f64),
    Char(char),
    Bool(bool),
}

// this is used for error display
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::Float(fl) => write!(f, "{:?}", fl),
            Self::Char(c) => write!(f, "'{}'", c), //TODO: escape chars
            Self::Bool(b) => write!(f, "{}", b),
        }
    }
}

impl Value {
    //============================
    // type checking

    pub fn type_(&self) -> TypeName {
        match self {
            Self::Int(_) => TypeName::new("int".to_string()),
            Self::Float(_) => TypeName::new("float".to_string()),
            Self::Char(_) => TypeName::new("char".to_string()),
            Self::Bool(_) => TypeName::new("bool".to_string()),
        }
    }

    pub fn is_type(&self, ty: &TypeName) -> bool {
        match self {
            Self::Int(_) => ty.name == "int",
            Self::Float(_) => ty.name == "float",
            Self::Char(_) => ty.name == "char",
            Self::Bool(_) => ty.name == "bool",
        }
    }

    //============================
    // unary ops

    pub fn pos(&self) -> Option<Self> {
        match self {
            Self::Int(_) | Self::Float(_) => Some(self.clone()),
            _ => None,
        }
    }

    pub fn neg(&self) -> Option<Self> {
        match *self {
            Self::Int(i) => Some(Self::Int(-i)),
            Self::Float(f) => Some(Self::Float(-f)),
            _ => None,
        }
    }

    pub fn not(&self) -> Option<Self> {
        match *self {
            Self::Bool(b) => Some(Self::Bool(!b)),
            _ => None,
        }
    }

    //============================
    // binary ops

    pub fn add(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Some(Self::Int(a + b)),
            (Self::Float(a), Self::Float(b)) => Some(Self::Float(a + b)),
            _ => None,
        }
    }

    pub fn sub(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Some(Self::Int(a - b)),
            (Self::Float(a), Self::Float(b)) => Some(Self::Float(a - b)),
            _ => None,
        }
    }

    pub fn mul(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Some(Self::Int(a * b)),
            (Self::Float(a), Self::Float(b)) => Some(Self::Float(a * b)),
            _ => None,
        }
    }

    pub fn div(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) if *b != 0 => Some(Self::Int(a / b)),
            (Self::Float(a), Self::Float(b)) if *b != 0.0 => Some(Self::Float(a / b)),
            _ => None,
        }
    }

    pub fn lt(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Some(Self::Bool(a < b)),
            (Self::Float(a), Self::Float(b)) => Some(Self::Bool(a < b)),
            (Self::Char(a), Self::Char(b)) => Some(Self::Bool(a < b)),
            _ => None,
        }
    }

    pub fn le(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Some(Self::Bool(a <= b)),
            (Self::Float(a), Self::Float(b)) => Some(Self::Bool(a <= b)),
            (Self::Char(a), Self::Char(b)) => Some(Self::Bool(a <= b)),
            _ => None,
        }
    }

    pub fn gt(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Some(Self::Bool(a > b)),
            (Self::Float(a), Self::Float(b)) => Some(Self::Bool(a > b)),
            (Self::Char(a), Self::Char(b)) => Some(Self::Bool(a > b)),
            _ => None,
        }
    }

    pub fn ge(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Some(Self::Bool(a >= b)),
            (Self::Float(a), Self::Float(b)) => Some(Self::Bool(a >= b)),
            (Self::Char(a), Self::Char(b)) => Some(Self::Bool(a >= b)),
            _ => None,
        }
    }

    pub fn eq(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Some(Self::Bool(a == b)),
            (Self::Float(a), Self::Float(b)) => Some(Self::Bool(a == b)),
            (Self::Char(a), Self::Char(b)) => Some(Self::Bool(a == b)),
            (Self::Bool(a), Self::Bool(b)) => Some(Self::Bool(a == b)),
            _ => None,
        }
    }

    pub fn ne(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => Some(Self::Bool(a != b)),
            (Self::Float(a), Self::Float(b)) => Some(Self::Bool(a != b)),
            (Self::Char(a), Self::Char(b)) => Some(Self::Bool(a != b)),
            (Self::Bool(a), Self::Bool(b)) => Some(Self::Bool(a != b)),
            _ => None,
        }
    }

    pub fn or(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Bool(a), Self::Bool(b)) => Some(Self::Bool(*a || *b)),
            _ => None,
        }
    }

    pub fn and(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Bool(a), Self::Bool(b)) => Some(Self::Bool(*a && *b)),
            _ => None,
        }
    }
}
