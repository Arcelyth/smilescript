use crate::chunk::*;
use crate::gc::*;
use crate::object::*;
use crate::scanner::*;

pub struct LoopState {
    pub start_index: usize,
    pub break_jumps: Vec<usize>,
}

impl LoopState {
    pub fn new(start_index: usize) -> Self {
        Self {
            start_index,
            break_jumps: Vec::new(),
        }
    }
}

pub struct Compiler<'c> {
    pub enclosing: Option<Box<Compiler<'c>>>,
    pub function: GcRef,
    pub fn_ty: FunctionType,
    pub locals: Vec<Local<'c>>,
    pub scope_depth: i32,
    pub loop_stack: Vec<LoopState>,
}

impl<'c> Compiler<'c> {
    pub const LOCAL_COUNT: usize = std::u8::MAX as usize + 1;
    pub fn new(func_name: &str, fn_ty: FunctionType, gc: &mut Gc) -> Self {
        let f_name_ref = gc.alloc(Obj::String(func_name.to_string()));
        let new_f = Function::new(f_name_ref);
        let f_ref = gc.alloc(Obj::Function(new_f));
        let mut n = Self {
            enclosing: None,
            function: f_ref,
            fn_ty: fn_ty,
            locals: Vec::with_capacity(Self::LOCAL_COUNT),
            scope_depth: 0,
            loop_stack: Vec::new(),
        };

        let token = match fn_ty {
            FunctionType::Method | FunctionType::Initializer => {
                Token::new(TokenType::Error, "this", 0)
            }
            _ => Token::new(TokenType::Error, "", 0),
        };
        n.locals.push(Local::new(token, 0));
        n
    }

    pub fn current_function<'gc>(&mut self, gc: &'gc mut Gc) -> &'gc mut Function {
        match gc.deref_mut(self.function) {
            Obj::Function(f) => f,
            _ => unreachable!(),
        }
    }

    pub fn current_chunk_mut<'gc>(&mut self, gc: &'gc mut Gc) -> &'gc mut Chunk {
        match gc.deref_mut(self.function) {
            Obj::Function(f) => &mut f.chunk,
            _ => unreachable!(),
        }
    }

    pub fn current_chunk<'gc>(&mut self, gc: &'gc Gc) -> &'gc Chunk {
        match gc.deref(self.function) {
            Obj::Function(f) => &f.chunk,
            _ => unreachable!(),
        }
    }

    pub fn resolve_local(&mut self, name: Token) -> Result<Option<u8>, String> {
        for i in (0..self.locals.len()).rev() {
            let local = &self.locals[i];
            if local.name.lexeme == name.lexeme {
                if local.depth == -1 {
                    return Err("Can't read local variable in its own initializer.".to_string());
                }
                return Ok(Some(i as u8));
            }
        }
        Ok(None)
    }

    pub fn resolve_upvalue(&mut self, name: Token, gc: &mut Gc) -> Result<Option<u8>, String> {
        if let Some(enclosing) = self.enclosing.as_mut() {
            if let Some(index) = enclosing.resolve_local(name)? {
                enclosing.locals[index as usize].is_captured = true;
                return Ok(Some(self.add_upvalue(index, true, gc)?));
            }
            if let Some(index) = enclosing.resolve_upvalue(name, gc)? {
                return Ok(Some(self.add_upvalue(index, false, gc)?));
            }
        }
        Ok(None)
    }

    fn add_upvalue(&mut self, idx: u8, is_local: bool, gc: &mut Gc) -> Result<u8, &str> {
        let cur_function = self.current_function(gc);
        for (i, upv) in cur_function.upvalues.iter().enumerate() {
            if upv.index == idx && upv.is_local == is_local {
                return Ok(i as u8);
            }
        }

        match u8::try_from(cur_function.upvalues.len()) {
            Ok(_index) => (),
            Err(_) => {
                return Err("Too many upvalues in one chunk.");
            }
        }

        cur_function.upvalues.push(FnUpValue::new(idx, is_local));
        Ok(cur_function.upvalues.len() as u8 - 1)
    }
}

pub struct Local<'src> {
    pub name: Token<'src>,
    pub depth: i32,
    pub is_captured: bool,
}

impl<'src> Local<'src> {
    pub fn new(name: Token<'src>, depth: i32) -> Self {
        Self {
            name,
            depth,
            is_captured: false,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum FunctionType {
    Function,
    Method,
    Initializer,
    Script,
}
