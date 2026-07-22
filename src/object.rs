use std::collections::HashMap;

use crate::chunk::Chunk;
use crate::gc::GcRef;
use crate::value::Value;
use crate::vm::Vm;

pub type Table = HashMap<String, Value>;

#[derive(Debug, Clone)]
pub enum Obj {
    String(String),
    Function(Function),
    Closure(Closure),
    UpValue(UpValue),
    Class(Class),
    Instance(Instance),
    BoundMethod(BoundMethod),
    Array(Vec<Value>),
}

impl Obj {
    pub fn get_size(&self, vm: &Vm) -> usize {
        use std::mem::size_of;

        let base_size = size_of::<Self>();

        match self {
            Obj::String(s) => base_size + s.capacity(),
            Obj::Function(f) => {
                base_size
                    + f.chunk.code.capacity()
                    + f.chunk.constants.capacity() * size_of::<Value>()
            }
            Obj::Closure(c) => base_size + c.upvalues.len() * size_of::<usize>(),

            Obj::UpValue(_) => base_size + size_of::<Value>(),

            Obj::Class(c) => {
                let name_size = vm.gc.deref(c.name).get_size(vm);
                base_size
                    + name_size
                    + c.methods.capacity() * (size_of::<Value>() + size_of::<usize>())
            }

            Obj::Instance(i) => {
                base_size + i.fields.capacity() * (size_of::<Value>() + size_of::<usize>())
            }

            Obj::BoundMethod(_) => base_size,

            Obj::Array(arr) => base_size + arr.capacity() * size_of::<Value>(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct GcObject {
    pub is_marked: bool,
    pub obj: Obj,
}

impl GcObject {
    pub fn new(obj: Obj) -> Self {
        Self {
            is_marked: false,
            obj,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: GcRef,
    pub upvalues: Vec<FnUpValue>,
}

impl Function {
    pub fn new(name: GcRef) -> Self {
        Self {
            arity: 0,
            name,
            chunk: Chunk::new(),
            upvalues: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct NativeFunction(pub fn(&Vm, &[Value]) -> Value);

#[derive(Debug, Clone)]
pub struct Closure {
    pub function: GcRef,
    pub upvalues: Vec<GcRef>,
}

impl Closure {
    pub fn new(function: GcRef) -> Self {
        Self {
            function,
            upvalues: Vec::new(),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct FnUpValue {
    pub index: u8,
    pub is_local: bool,
}

impl FnUpValue {
    pub fn new(index: u8, is_local: bool) -> Self {
        Self { index, is_local }
    }
}

#[derive(Debug, Clone)]
pub struct UpValue {
    pub location: usize,
    pub closed: Option<Value>,
}

impl UpValue {
    pub fn new(location: usize) -> Self {
        Self {
            location,
            closed: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: GcRef,
    pub methods: Table,
}

impl Class {
    pub fn new(name: GcRef) -> Self {
        Self {
            name,
            methods: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub class: GcRef,
    pub fields: Table,
}

impl Instance {
    pub fn new(class: GcRef) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BoundMethod {
    pub receiver: Value,
    pub method: GcRef,
}

impl BoundMethod {
    pub fn new(receiver: Value, method: GcRef) -> Self {
        Self { receiver, method }
    }
}
