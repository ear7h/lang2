#![allow(dead_code)]
use std::fmt;

#[derive(Debug)]
pub enum Ir<T> {
    Label(LabelId),
    Jump(LabelId),
    CondJump{
        cond: ExprId,
        if_target: LabelId,
        else_target: Option<LabelId>,
    },

    // arg number
    Arg(usize, T), // from stack

    // it does not make sense to subject global data to SSA rules. So, instead,
    // this should represent a symbol which behaves like a compile-time constant
    // pointer.
    Global(String, T),

    // re-declare
    Identity(ExprId, T),
    Phi(Vec<ExprId>, T),
    Call{
        func : usize,
        args : Vec<ExprId>,
        ty : T,
    },
}

impl<T: fmt::Display> fmt::Display for Block<T> {
    fn fmt(&self, f : &mut fmt::Formatter<'_>) -> fmt::Result {
        for (id, expr) in self.ops.iter().enumerate() {
            match expr {
                Ir::Label(id) => writeln!(f, "L{}:", id),
                Ir::Jump(id) => writeln!(f, "\tjump L{}", id),
                Ir::CondJump{cond, if_target, else_target} => {
                    let it = if_target;
                    let et_opt = else_target;

                    if let Some(et) = et_opt {
                        writeln!(f, "\tif {} jump L{} else L{}", cond, it,  et)
                    } else {
                        writeln!(f, "\tif {} jump L{}", cond, it)
                    }
                },
                Ir::Arg(n, _) => writeln!(f, "\tv{} = arg{}", id, n),
                Ir::Global(s, _) => writeln!(f, "\tv{} = {}", id, s),
                Ir::Identity(eid, _) => writeln!(f, "\tv{} = v{}", id, eid),
                Ir::Phi(vars, _) => {
                    writeln!(f, "\tv{} = phi({:?})", id, vars)
                },
                Ir::Call{func, args, ..} => {
                    writeln!(f, "\tv{} = v{}({:?})", id, func, args)
                }
            }?
        }
        Ok(())
    }
}

type LabelId = usize;
type ExprId = usize;

/// T is the parent language's type system
#[derive(Default, Debug)]
pub struct Block<T> {
    pub nlabels : usize,
    pub ops : Vec<Ir<T>>,
}


impl<T : Clone> Block<T> {
    pub fn new_label(&mut self) -> LabelId {
        let ret = self.nlabels;
        self.nlabels += 1;
        ret
    }

    pub fn push(&mut self, expr : Ir<T>) -> ExprId {
        let ret = self.ops.len();
        self.ops.push(expr);
        return ret
    }

    pub fn arg(&mut self, n : usize, ty : T) -> ExprId {
        self.push(Ir::Arg(n, ty))
    }

    pub fn global(&mut self, n : usize, ty : T) -> ExprId {
        self.push(Ir::Arg(n, ty))
    }

    pub fn identity(&mut self, src : usize, ty : T) -> ExprId {
        self.push(Ir::Identity(src, ty))
    }

    pub fn phi(&mut self, exprs : Vec<ExprId>, ty : T) -> ExprId {
        self.push(Ir::Phi(exprs, ty))
    }

    pub fn call(&mut self,
            func : usize,
            args: Vec<usize>,
            ty : T) -> usize
    {
        self.push(Ir::Call{func, args, ty})
    }


    pub fn label(&mut self, id : LabelId) {
        self.push(Ir::Label(id));
    }

    pub fn jump(&mut self, target : LabelId) {
        self.push(Ir::Jump(target));
    }

    pub fn cond_jump(&mut self, cond : ExprId,
                 if_target : LabelId,
                 else_target : Option<LabelId>) {
        self.push(Ir::CondJump{cond, if_target, else_target});
    }
}
