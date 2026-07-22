#![allow(dead_code)]
use crate::value::*;

#[derive(Debug, Copy, Clone)]
pub enum OpCode {
    Print,
    Return,
    Negate,
    Constant(u8),
    Nil,
    True,
    False,
    Pop,
    GetLocal(u8),
    SetLocal(u8),
    SetGlobal(u8),
    GetGlobal(u8),
    SetUpValue(u8),
    GetUpValue(u8),
    DefineGlobal(u8),
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
    Not,
    JumpIfFalse(u16),
    Jump(u16),
    Loop(u16),
    Call(u8),
    Closure(u8),
    CloseUpValue,
    Class(u8),
    GetProperty(u8),
    SetProperty(u8),
    Method(u8),
    Invoke((u8, u8)),
    SuperInvoke((u8, u8)),
    Inherit,
    GetSuper(u8),
    Array(u8),
    SetIndex,
    GetIndex,
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub code: Vec<OpCode>,
    pub lines: Vec<usize>,
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            lines: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub fn write(&mut self, op_code: OpCode, line: usize) -> usize {
        self.code.push(op_code);
        self.lines.push(line);
        self.code.len() - 1
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }
}
