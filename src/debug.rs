#![allow(dead_code)]

use crate::chunk::*;
use crate::gc::*;
use crate::object::*;
use crate::value::*;

pub struct Disassembler;

impl Disassembler {
    pub fn new() -> Self {
        Self
    }

    pub fn dasm_chunk(&self, name: &str, chunk: &Chunk, gc: &Gc) {
        println!("== {} ==", name);
        for (offset, code) in chunk.code.iter().enumerate() {
            self.dasm_instruction(chunk, offset, code, gc)
        }
    }

    pub fn dasm_instruction(&self, chunk: &Chunk, offset: usize, code: &OpCode, gc: &Gc) {
        print!("{:04}    ", offset);

        if offset > 0 && chunk.lines[offset] == chunk.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", chunk.lines[offset]);
        }

        match code {
            OpCode::Constant(c) => self.const_instruction(chunk, "OP_CONSTANT", *c),
            OpCode::Nil => println!("OP_NIL"),
            OpCode::True => println!("OP_TRUE"),
            OpCode::False => println!("OP_FALSE"),
            OpCode::Pop => println!("OP_POP"),
            OpCode::SetLocal(c) => self.byte_instruction("OP_SET_LOCAL", *c),
            OpCode::GetLocal(c) => self.byte_instruction("OP_GET_LOCAL", *c),
            OpCode::SetGlobal(c) => self.const_instruction(chunk, "OP_SET_GLOBAL", *c),
            OpCode::GetGlobal(c) => self.const_instruction(chunk, "OP_GET_GLOBAL", *c),
            OpCode::SetUpValue(c) => self.byte_instruction("OP_SET_UPVALUE", *c),
            OpCode::GetUpValue(c) => self.byte_instruction("OP_GET_UPVALUE", *c),

            OpCode::DefineGlobal(c) => self.const_instruction(chunk, "OP_DEFINE_GLOBAL", *c),
            OpCode::Equal => println!("OP_EQUAL"),
            OpCode::Greater => println!("OP_GREATER"),
            OpCode::Less => println!("OP_LESS"),
            OpCode::Negate => println!("OP_NEGATE"),
            OpCode::Print => println!("OP_PRINT"),
            OpCode::Return => println!("OP_RETURN"),
            OpCode::Add => println!("OP_ADD"),
            OpCode::Subtract => println!("OP_SUBTRACT"),
            OpCode::Multiply => println!("OP_MULTIPLY"),
            OpCode::Divide => println!("OP_DIVIDE"),
            OpCode::JumpIfFalse(o) => self.jump_instruction("OP_JUMP_IF_FALSE", 1, *o, offset),
            OpCode::Jump(o) => self.jump_instruction("OP_JUMP", 1, *o, offset),
            OpCode::Loop(o) => self.jump_instruction("OP_LOOP", -1, *o, offset),
            OpCode::Call(arg_count) => self.byte_instruction("OP_CALL", *arg_count),
            OpCode::Closure(c) => {
                self.const_instruction(chunk, "OP_CLOSURE", *c);
                match &chunk.constants[*c as usize] {
                    Value::Obj(gc_ref) => {
                        if let Obj::Function(f) = gc.deref(*gc_ref) {
                            for upvalue in f.upvalues.iter() {
                                let is_local = if upvalue.is_local { "local" } else { "upvalue" };
                                let idx = upvalue.index;
                                println!("{:04}    |               {} {}", offset, is_local, idx);
                            }
                        }
                    }
                    _ => (),
                }
            }
            OpCode::CloseUpValue => println!("OP_CLOSE_UPVALUE"),
            OpCode::Class(c) => self.const_instruction(chunk, "OP_CLASS", *c),
            OpCode::GetProperty(c) => self.const_instruction(chunk, "OP_GET_PROPERTY", *c),
            OpCode::SetProperty(c) => self.const_instruction(chunk, "OP_SET_PROPERTY", *c),
            OpCode::Method(c) => self.const_instruction(chunk, "OP_METHOD", *c),
            OpCode::Invoke(t) => self.invoke_instruction("OP_INVOKE", *t),
            OpCode::SuperInvoke(t) => self.invoke_instruction("OP_SUPER_INVOKE", *t),
            OpCode::Inherit => println!("OP_INHERIT"),
            OpCode::GetSuper(c) => self.const_instruction(chunk, "OP_INHERIT", *c),
            OpCode::Array(c) => self.const_instruction(chunk, "OP_ARRAY", *c),
            OpCode::SetIndex => println!("OP_SET_INDEX"),
            OpCode::GetIndex => println!("OP_GET_INDEX"),
            _ => println!("Unknown opcode: {:?}", code),
        }
    }

    pub fn const_instruction(&self, chunk: &Chunk, name: &str, offset: u8) {
        println!(
            "{:<16} {:4} {}",
            name, offset, chunk.constants[offset as usize]
        );
    }

    pub fn byte_instruction(&self, name: &str, offset: u8) {
        println!("{:<16} {:4}", name, offset);
    }

    pub fn jump_instruction(&self, name: &str, sign: i16, offset: u16, pos: usize) {
        let jump_to = if sign == 1 {
            pos + 1 + offset as usize
        } else {
            pos + 1 - offset as usize
        };
        println!("{:<16} {:4} -> {}", name, pos, jump_to);
    }

    pub fn invoke_instruction(&self, name: &str, info: (u8, u8)) {
        println!("{:<16} ({} args) {:4}", name, info.1, info.0);
    }
}
