use std::fmt::{self, Display};

use crate::gc::*;
use crate::object::*;

#[derive(Debug, Copy, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Number(f64),
    Native(NativeFunction),
    Obj(GcRef),
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Number(v) => write!(f, "{}", v),
            Value::Obj(ref_idx) => write!(f, "<Obj ref:{}>", ref_idx.0),
            Value::Native(_n) => write!(f, "<native>"),
        }
    }
}

pub fn values_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
        (Value::Nil, Value::Nil) => true,
        (Value::Number(n1), Value::Number(n2)) => n1 == n2,
        (Value::Obj(ref1), Value::Obj(ref2)) => ref1 == ref2,
        _ => false,
    }
}
