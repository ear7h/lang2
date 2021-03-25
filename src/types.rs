#![allow(dead_code)]

/* references
 *
 * https://web.archive.org/web/20190803133115/https://pages.cs.wisc.edu/~horwitz/CS704-NOTES/1.LAMBDA-CALCULUS.html#NOR
 * https://stackoverflow.com/questions/4039494/how-would-you-implement-a-beta-reduction-function-in-f
 * https://en.wikipedia.org/wiki/Beta_normal_form
 * my (private) implementation for this homework assignment https://github.com/cse130-assignments/04-nano
 * I'll probably need to read https://people.csail.mit.edu/dnj/teaching/6898/papers/wadler88.pdf
 */
/*

    types:
    _
    MyType
    struct {
        a MyType
    }
    MyPoly MyType

    type constructors:
    type MyPoly T U V = struct {
        t T
        u U
        v V
    }
    type MyPoly T U = T U
*/


use std::fmt::{self, Debug};
use std::collections::HashMap;
use std::hash::Hash;

pub struct TraitName<Ident> {
    name : Ident,
    args : Vec<Type<Ident>>,
}

#[derive(Clone)]
pub enum Type<Ident> {
    Hole,
    Var(Ident),
    Prim(Ident),
    Enum{
        name : Ident,
        fields : Vec<(Ident, Self)>,
    },
    Struct{
        name : Option<Ident>,
        fields : Vec<(Ident, Self)>,
    },
    Func {
        sig : Vec<Self>,
    },
    Apply {
        f : Box<Self>,
        arg : Box<Self>,
    },
    Abstract {
        formal : Ident,
        // constraints : Vec<TraitName>, // trait constraints
        body : Box<Self>,
    },
}

impl<Ident : Debug> Debug for Type<Ident> {
    fn fmt(&self, f : &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;

        match self {
            Hole => write!(f, "_"),
            Prim(s) => write!(f, "{:?}", s),
            Enum{fields, name} => {
                write!(f, "{:?} (", name)?;

                let mut it = fields.iter();

                if let Some((name, ty)) = it.next() {
                    write!(f, "{:?} : {:?}", name, ty )?
                }

                for (name, ty) in it {
                    write!(f, " + {:?} : {:?}", name, ty)?
                }

                write!(f, ")")?;

                Ok(())
            },
            Struct{name, fields} => {
                match name {
                    Some(s) => write!(f, "{:?} (", s)?,
                    None => write!(f, "(")?,
                }
                let mut it = fields.iter();

                if let Some((name, ty)) = it.next() {
                    write!(f, "{:?} : {:?}", name, ty )?
                }

                for (name, ty) in it {
                    write!(f, " * {:?} : {:?}", name, ty)?
                }

                write!(f, ")")?;

                Ok(())
            },
            Func{sig} => {
                write!(f, "(")?;

                let mut it = sig.iter();
                if let Some(v) = it.next() {
                    write!(f, "{:?}", v)?
                }
                for ty in it {
                    write!(f, " -> {:?}", ty)?
                }

                write!(f, ")")?;

                Ok(())
            }
            Apply{f:ff, arg} => write!(f, "({:?}) ({:?})", ff, arg),
            Abstract{formal, body} => write!(f, "\\{:?} -> {:?}", formal, body),
            Var(s) => write!(f, "{:?}", s),
        }
    }
}

impl<Ident : Clone + Debug + Eq> Type<Ident> {

    pub fn app(f : Self, args : &[Self]) -> Self {
        match args {
            [arg] => Self::Apply{
                f : box f,
                arg : box arg.clone(),
            },
            [tail @ .., arg] => Self::Apply{
                f : box Self::app(f, tail),
                arg : box arg.clone(),
            },
            _ => panic!("cannot have empty args"),
        }
    }

    pub fn abs(formals : &[Ident], body : Self) -> Self {
        match formals {
            [formal] => Self::Abstract{
                formal : formal.clone(),
                body : box body,
            },
            [formal, tail @ ..] => Self::Abstract{
                formal : formal.clone(),
                body: box Self::abs(tail, body),
            },
            _ => panic!("cannot have empty formals"),
        }
    }

    // TODO: this is an alternative to using the ConsEnv, I'm leaving it
    // here to benchmark in the future
    #[allow(dead_code)]
    fn rewrite(&mut self, name : &Ident, ty : &Self) {
        use Type::*;

        match self {
            Enum{fields, ..} => {
                for (_, fty) in fields {
                    fty.rewrite(name, ty)
                }
            },
            Struct{fields, ..} => {
                for (_, fty) in fields {
                    fty.rewrite(name, ty)
                }
            },
            Func{sig} => {
                for fty in sig {
                    fty.rewrite(name, ty)
                }
            }
            Var(s) if s == name => {
                std::mem::swap(self, &mut ty.clone());
            },
            Apply{f, arg} => {
                f.rewrite(name, ty);
                arg.rewrite(name, ty);
            },
            Abstract{formal, body} if name != formal => {
                body.rewrite(name, ty)
            },
            _ => {},
        }
    }

    pub fn eval<E : Env<Ident> + ?Sized>(&self, env : &E)
    -> Result<Type<Ident>, String> {
        self.eval_rec(&ConsEnv::from(env))
    }

    fn eval_rec<E : Env<Ident> + ?Sized>(&self,
                             env : &ConsEnv<'_, '_, E, Ident>,
                             ) -> Result<Type<Ident>, String> {
        use Type::*;

        match self {
            Hole => Ok(Hole),
            Prim(s) => Ok(Prim(s.clone())),
            Var(s) => {
                env.get(s)
                    .map(Clone::clone)
                    .ok_or(format!("undefined type {:?}", s))?
                    .eval_rec(env)
            },
            Enum{name, fields} => {
                let mut ret = Vec::with_capacity(fields.len());
                for (name, ty) in fields {
                    ret.push((name.clone(), ty.eval_rec(env)?));
                }

                Ok(Enum{name : name.clone(), fields : ret})
            },
            Struct{name, fields} => {
                let mut ret = Vec::with_capacity(fields.len());
                for (name, ty) in fields {
                    ret.push((name.clone(), ty.eval_rec(env)?));
                }

                Ok(Struct{name : name.clone(), fields : ret})
            },
            Func{sig} => {
                let mut ret = Vec::with_capacity(sig.len());
                for ty in sig {
                    ret.push(ty.eval_rec(env)?);
                }

                Ok(Func{sig : ret})
            },
            Apply{f, arg} => {
                // TODO: use a loop to get an abstraction out of f?
                // we can't always avoid recursion because f might be
                // an application
                let ty = f.eval_rec(env)?;
                match ty {
                    Abstract{formal, body} => {
                        let env1 = ConsEnv::Cons{
                            head: (formal, &arg),
                            tail: &env,
                        };
                        //body.rewrite(&formal, arg);
                        body.eval_rec(&env1)
                    },
                    _ => Err(format!("cannot apply to {:?}", ty)),
                }
            },
            Abstract{..} => Ok(self.clone()),
        }
    }
}

/// an extendable immutable env, works like a haskell list
pub enum ConsEnv<'e, 'a, E : ?Sized, Ident> {
    Root(&'e E),
    Cons{
        head: (Ident, &'a Type<Ident>),
        tail: &'a Self,
    }
}

impl<'e, E, Ident> From<&'e E> for ConsEnv<'e, '_, E, Ident>
where
    E : Env<Ident> + ?Sized,
{
    fn from(env : &'e E) -> Self {
        ConsEnv::Root(env)
    }
}

impl<E, Ident> Env<Ident> for ConsEnv<'_, '_, E, Ident>
where
    Ident : Eq,
    E : Env<Ident> + ?Sized,
{
    fn get(&self, key : &Ident) -> Option<&Type<Ident>> {
        match self {
            ConsEnv::Root(env) => env.get(key),
            ConsEnv::Cons{head, tail} => {
                if &head.0 == key {
                    Some(&head.1)
                } else {
                    tail.get(key)
                }
            }
        }
    }
}



pub trait Env<Ident> {
    fn get(&self, key : &Ident) -> Option<&Type<Ident>>;
}

impl<Ident> Env<Ident> for HashMap<Ident, Type<Ident>>
where
    Ident : Eq + Hash
{
    fn get(&self, key : &Ident) -> Option<&Type<Ident>> {
        HashMap::get(self, key)
    }
}


impl<Ident> Env<Ident> for [(Ident, Type<Ident>)]
where
    Ident : Eq,
{
    fn get(&self, key : &Ident) -> Option<&Type<Ident>> {
        self.iter()
            .rev()
            .find_map(|(k, v)| (k == key).then(|| v))
    }
}
