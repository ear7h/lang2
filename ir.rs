


type Result<T> = std::result::Result<T, Error>

enum Error {
    UndefinedVariable,
}


enum Expr {
    Identity(usize),
    Phi(usize, usize),
    Call{
        func : usize,
        args : Vec<usize>,
    },
    Switch{
        val : usize,
        cases : Vec<usize, Op>
    }
}

/// T is the parent language's type system
struct Block<Id, T> {
    vars : HashMap<Id, (T, usize)>,
    ops : Vec<(Option<(Id, usize)>, Expr)>,
}


impl<T : Clone> Block<T> {
    fn next_gen(&mut self, id : Id) -> Result<usize> {
        if let Some((_,  n)) = self.vars.get_mut(id) {
            n += 1;

            Ok(*n)
        } else {
            Err(Error::UndefinedVariable)
        }
    }

    fn get_ir_dst(&mut self, dst : Option<Id>) -> Result<Option<Id, usize>> {
       let ir_dst = if let Some(id) = dst {
           Some(self.next_gen(id)?);
       } else {
           None
       };
    }

    fn call(&mut self,
            dst : Option<Id>,
            func : usize,
            args: Vec<usize>) -> Result<usize>
    {

       self.ops.push((ir_dst, Expr::Call{func, args}));

       Ok(())
    }

    fn switch<F>(&mut self, var : usize, f : F)
    where F : FnOnce(&mut Cases)
    {

        let mut cases = Cases::new();

        f(&mut cases);

        self.ops.push((None, cases));
    }

    fn for(&mut self,
}

struct Cases {

    arms : Vec<usize, Block>,
}

impl Case {
    fn arm(&mut self, var : usize, f : F)
    where F : FnOnce(&mut Block)
    {
    }
}

