#![allow(dead_code)]

/* references
 *
 * https://web.archive.org/web/20190803133115/https://pages.cs.wisc.edu/~horwitz/CS704-NOTES/1.LAMBDA-CALCULUS.html#NOR
 * https://stackoverflow.com/questions/4039494/how-would-you-implement-a-beta-reduction-function-in-f
 * https://en.wikipedia.org/wiki/Beta_normal_form
 * my (private) implementation for this homework assignment https://github.com/cse130-assignments/04-nano
 * I'll probably need to read https://people.csail.mit.edu/dnj/teaching/6898/papers/wadler88.pdf
 * https://www.reddit.com/r/haskell/comments/9z6v51/whats_the_difference_between_head_normal_formhnf/
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


use std::fmt::{self, Debug, Display};
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Clone)]
pub enum Error<Ident> {
    Undefined(Ident),
    CannotApplyTo(Type<Ident>),
    CannotInferHigherKinds(Type<Ident>),
}


impl <Ident : Display> Display for Error<Ident> {
    fn fmt(&self, f : &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;

        match self {
            Undefined(id) => write!(f, "undefined type {}", id),
            CannotApplyTo(ty) => write!(f, "cannot apply to {}", ty),
            CannotInferHigherKinds(ty) => write!(f, "cannot infer from type abstraction {}", ty),
        }
    }
}

impl <Ident : Debug> Debug for Error<Ident> {
    fn fmt(&self, f : &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;

        match self {
            Undefined(id) => write!(f, "undefined type {:?}", id),
            CannotApplyTo(ty) => write!(f, "cannot apply to {:?}", ty),
            CannotInferHigherKinds(ty) => write!(f, "cannot infer from type abstraction {:?}", ty),
        }
    }
}



pub struct TraitName<Ident> {
    name : Ident,
    args : Vec<Type<Ident>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type<Ident> {
    Hole,
    Var(Ident),
    Prim(Ident),
    Enum{
        fields : Vec<(Ident, Self)>,
    },
    Struct{
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

impl<Ident : Display> Display for Type<Ident> {
    fn fmt(&self, f : &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;

        match self {
            Hole => write!(f, "_"),
            Prim(s) => write!(f, "{}", s),
            Var(s) => write!(f, "{}", s),
            Enum{fields} => {
                let mut it = fields.iter();

                if let Some((name, ty)) = it.next() {
                    write!(f, "{} : {}", name, ty )?
                }

                for (name, ty) in it {
                    write!(f, " + {} : {}", name, ty)?
                }


                Ok(())
            },
            Struct{fields} => {
                let mut it = fields.iter();

                if let Some((name, ty)) = it.next() {
                    write!(f, "{} : {}", name, ty )?
                }

                for (name, ty) in it {
                    write!(f, " * {} : {}", name, ty)?
                }


                Ok(())
            },
            Func{sig} => {
                write!(f, "fn (")?;

                match sig.len() {
                    0 => write!(f, ") -> ()")?,
                    1 => write!(f, ") -> ({})", sig[0])?,
                    2 => write!(f, "{}) -> ({})", sig[0], sig[1])?,
                    n => {
                        for ty in sig[..(n-2)].iter() {
                            write!(f, "{}, ", ty)?
                        }

                        write!(f, "{}) -> ({})", sig[n-2], sig[n-1])?
                    }
                };

                Ok(())
            }
            Apply{f:ff, arg} => write!(f, "({}) ({})", ff, arg),
            Abstract{formal, body} => write!(f, "\\{} -> {}", formal, body),
        }
    }
}

mod dsl {
    pub use super::Type::{self, *};

    pub fn apply<Ident>(e1 : Type<Ident>, e2 : Type<Ident>) -> Type<Ident> {
        Apply{
            f : box e1,
            arg : box e2,
        }
    }

    pub fn abs<Ident>(formal : Ident, body : Type<Ident>) -> Type<Ident> {
        Abstract{
            formal,
            body: box body
        }
    }

    pub fn apply_n<It, Ident>(f : Type<Ident>, args : It) -> Type<Ident>
    where
        It : IntoIterator<Item = Type<Ident>>
    {
        let mut args = args.into_iter();

        match args.next() {
            None => f,
            Some(arg) => {
                apply_n(Type::Apply{
                    f : box f,
                    arg : box arg,
                }, args)
            },
        }
    }

    pub fn abs_n<It, Ident>(formals : It, body : Type<Ident>) -> Type<Ident>
    where
        It : IntoIterator<Item = Ident>
    {
        let mut formals = formals.into_iter();

        match formals.next() {
            None => body,
            Some(formal) => {
                Type::Abstract{
                    formal: formal,
                    body : box abs_n(
                        formals,
                        body,
                    ),
                }
            }
        }
    }
}


impl<Ident : Clone + Debug + Eq> Type<Ident> {


    pub fn apply_n<It>(f : Self, args : It) -> Self
    where
        It : IntoIterator<Item = Self>
    {
        let mut args = args.into_iter();

        match args.next() {
            None => f,
            Some(arg) => {
                Self::apply_n(Self::Apply{
                    f : box f,
                    arg : box arg,
                }, args)
            },
        }
    }

    pub fn abstract_n<It>(formals : It, body : Self) -> Self
    where
        It : IntoIterator<Item = Ident>
    {
        let mut formals = formals.into_iter();

        match formals.next() {
            None => body,
            Some(formal) => {
                Self::Abstract{
                    formal: formal,
                    body : box Self::abstract_n(
                        formals,
                        body,
                    ),
                }
            }
        }
    }

    fn rewrite(&mut self, name : &Ident, ty : &Self) {
        use Type::*;

        match self {
            Enum{fields} => {
                for (_, fty) in fields {
                    fty.rewrite(name, ty)
                }
            },
            Struct{fields} => {
                for (_, fty) in fields {
                    fty.rewrite(name, ty)
                }
            },
            Func{sig} => {
                for fty in sig {
                    fty.rewrite(name, ty)
                }
            }
            Apply{f, arg} => {
                f.rewrite(name, ty);
                arg.rewrite(name, ty);
            },
            Abstract{formal, body} if name != formal => {
                body.rewrite(name, ty)
            },
            Var(s) if s == name => {
                std::mem::swap(self, &mut ty.clone());
            },
            Hole | Var(_) | Prim(_) | Abstract{..} => {},
        }
    }


    // TODO: add a recursion limit argument to eval funcs

    pub fn eval<E : Env<Ident> + ?Sized>(&self, env : &E)
    -> Result<Type<Ident>, Error<Ident>> {
        self.clone().eval_whnf1(env)
    }

    pub fn eval_lazy<E : Env<Ident> + ?Sized>(&self, env : &E)
    -> Result<Type<Ident>, Error<Ident>> {
        self.clone().eval_whnf1(env)
    }

    pub fn eval_strict<E : Env<Ident> + ?Sized>(&self, env : &E)
    -> Result<Type<Ident>, Error<Ident>> {
        self.clone().eval_nf(env)
    }

    /// Call by name reduction to weak head normal form
    ///
    /// A lambda expression is in head normal form when it is neither
    /// a redex nor a lambda abstraction with a reducible body
    ///
    /// A lambda expression is in weak head normal form when it is not
    /// a redex
    fn eval_whnf1<E>(
        self,
        env : &E
    ) -> Result<Type<Ident>, Error<Ident>>
        where
            E : Env<Ident> + ?Sized
    {
        use Type::*;

        match self {
            Apply{f, arg} => {
                match f.eval_whnf1(env)? {
                    Abstract{formal, mut body} => {
                        body.rewrite(&formal, &arg);
                        body.eval_whnf1(env)
                    },
                    ff @ Prim(_) => {
                        Ok(Apply{f: box ff, arg})
                    }
                    x => {
                        Err(Error::CannotApplyTo(x))
                    }
                }
            },
            Var(s) => {
                env.get(&s)
                    .map(Clone::clone)
                    .ok_or(Error::Undefined(s.clone()))?
                    .eval_whnf1(env)
            },
            _ => Ok(self),
        }
    }

    /// another whnf algorithm
    fn eval_whnf2<E>(
        mut self,
        env : &E
    ) -> Result<Type<Ident>, Error<Ident>>
        where
            E : Env<Ident> + ?Sized
    {
        use Type::*;

        while let Apply{box f, arg} = self {
            self = match f {
                Apply{..} => {
                    Apply{
                        f : box f.eval_whnf2(env)?,
                        arg
                    }
                },
                Abstract{formal, box mut body} => {
                    body.rewrite(&formal, &arg);
                    body
                },
                Var(s) => {
                    let ff = env.get(&s)
                        .map(Clone::clone)
                        .ok_or(Error::Undefined(s.clone()))?;

                    Apply{
                        f : box ff,
                        arg
                    }
                },
                Prim(_) => {
                    return Ok(Apply{f: box f, arg})
                }
                x => {
                    return Err(Error::CannotApplyTo(x))
                }
            }
        }

        Ok(self)
    }

    /// Normal order reduction to normal form
    ///
    /// If the expression reduces to a normal form, this algorithm
    /// will find it
    fn eval_nf<E>(
        self,
        env : &E
    ) -> Result<Type<Ident>, Error<Ident>>
        where
            E : Env<Ident> + ?Sized
    {
        use Type::*;

        match self {
            Var(_) => Ok(self),
            Abstract{..} => Ok(self),
            Apply{f, arg} => {
                match f.eval_lazy(env)? {
                    Abstract{formal, mut body} => {
                        body.rewrite(&formal, &arg);
                        body.eval_nf(env)
                    },
                    x => {
                        Ok(Apply{
                            f : box x.eval_nf(env)?,
                            arg: box arg.eval_nf(env)?
                        })
                    }
                }
            }
            x => Ok(x)
        }

    }
}

/// an extendable immutable env, works like a haskell list
pub enum ConsEnv<'e, 'a, E : ?Sized, Ident> {
    Root(&'e E),
    Cons{
        head: (&'a Ident, &'a Type<Ident>),
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
                if head.0 == key {
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn apply_n() {
        use Type::*;

        let tests = vec![
            (
                Type::apply_n(
                    Var("a"),
                    vec![Var("b"), Var("c")]
                ),
                Apply{
                    f: box Apply{
                        f : box Var("a"),
                        arg: box Var("b")
                    },
                    arg: box Var("c")
                }
            )
        ];

        for (tc, exp) in tests {
            assert_eq!(tc, exp)
        }
    }

    #[test]
    fn abstract_n() {
        use Type::*;

        let tests = vec![
            (
                Type::abstract_n(
                    vec!["a", "b"],
                    Var("c"),
                ),
                Abstract{
                    formal: "a",
                    body: box Abstract{
                        formal : "b",
                        body: box Var("c")
                    },
                }
            )
        ];

        for (tc, exp) in tests {
            assert_eq!(tc, exp)
        }
    }

    #[test]
    fn eval_whnf() {
        use super::dsl::*;

        let env = vec![
            ("hello", Prim("hello")),
            ("If", abs_n(vec!["P", "T", "F"],
                apply_n(Var("P"), vec![Var("T"), Var("F")]))),
            ("True", abs_n(vec!["A", "B"], Var("A"))),
            ("False", abs_n(vec!["A", "B"], Var("B"))),

            ("0", abs_n(vec!["f", "x"], Var("x"))),

            ("1", abs_n(vec!["f", "x"],
                apply(Var("f"), Var("x")))),

            ("2", abs_n(vec!["f", "x"],
                apply(Var("f"), apply(Var("f"), Var("x"))))),

            ("3", abs_n(vec!["f", "x"],
                apply(Var("f"), apply(Var("f"), apply(Var("f"), Var("x")))))),

            ("+", abs_n(vec!["m", "n", "f", "x"],
                apply(
                    apply(Var("m"), Var("f")),
                        apply(apply(Var("n"), Var("f")), Var("x"))))),
            ("Box", abs("t", Struct{fields : vec![("inner", Var("t"))]})),
            //f f f x
        ];

        let env1 = env.as_slice();

        let tests = vec![
            (
                apply_n(Var("If"), vec![Var("True"), Prim("a"), Prim("b")]),
                Prim("a"),
            ),
            (
                apply_n(Var("+"), vec![Var("1"), Var("2")]),
                abs_n(vec!["f", "x"],
                    apply(
                        apply(Var("1"), Var("f")),
                            apply_n(
                                Var("2"),
                                vec![Var("f"), Var("x")])))
            ),
            (
                apply_n(
                    Var("+"),
                    vec![Var("1"), Var("2"), Prim("f'"), Prim("x'")]),
                apply(Prim("f'"),
                    apply(
                        apply(Var("2"), Prim("f'")), Prim("x'"))),
            ),
            (
                apply_n(
                    Var("+"),
                    vec![Var("1"), Var("2"), Var("Box"), Prim("x'")]),
                Struct{fields : vec![("inner", apply(apply(Var("2"), Var("Box")), Prim("x'")))]}
            )
        ];

        for (left, right) in tests {
            let left1 = left.clone().eval_whnf1(env1).unwrap();
            assert_eq!(
                left1,
                right,
                "\nleft: {}\nright {}\n", left1, right);

            let left2 = left.clone().eval_whnf2(env1).unwrap();
            assert_eq!(
                left2,
                right,
                "\nleft: {}\nright {}\n", left1, right);
        }

    }

    #[test]
    fn eval_nf() {
        use super::dsl::*;

        let env = vec![
            ("hello", Prim("hello")),
            ("If", abs_n(vec!["P", "T", "F"],
                apply_n(Var("P"), vec![Var("T"), Var("F")]))),
            ("True", abs_n(vec!["A", "B"], Var("A"))),
            ("False", abs_n(vec!["A", "B"], Var("B"))),

            ("0", abs_n(vec!["f", "x"], Var("x"))),

            ("1", abs_n(vec!["f", "x"],
                apply(Var("f"), Var("x")))),

            ("2", abs_n(vec!["f", "x"],
                apply(Var("f"), apply(Var("f"), Var("x"))))),

            ("3", abs_n(vec!["f", "x"],
                apply(Var("f"), apply(Var("f"), apply(Var("f"), Var("x")))))),

            ("+", abs_n(vec!["m", "n", "f", "x"],
                apply(
                    apply(Var("m"), Var("f")),
                        apply(apply(Var("n"), Var("f")), Var("x"))))),

            ("Box", abs("t", Struct{fields : vec![("inner", Var("t"))]})),
        ];

        let env1 = env.as_slice();

        let tests = vec![
            (
                apply_n(Var("If"), vec![Var("True"), Prim("a"), Prim("b")]),
                Prim("a"),
            ),
            (
                apply_n(Var("If"), vec![Var("True"), Prim("a"), Var("FAIL")]),
                Prim("a"),
            ),
            (
                apply_n(Var("If"), vec![Var("False"), Var("FAIL"), Prim("a")]),
                Prim("a"),
            ),
            (
                apply_n(Var("+"), vec![Var("1"), Var("2")]),
                abs_n(vec!["f", "x"],
                    apply(
                        apply(Var("1"), Var("f")),
                            apply_n(
                                Var("2"),
                                vec![Var("f"), Var("x")])))
            ),
            (
                apply_n(
                    Var("+"),
                    vec![Var("1"), Var("2"), Prim("f'"), Prim("x'")]),
                apply(Prim("f'"),
                    apply(Prim("f'"),
                        apply(Prim("f'"), Prim("x'"))))
            ),
            (
                apply_n(
                    Var("+"),
                    vec![Var("1"), Var("2"), Var("Box"), Prim("x'")]),
                Struct{fields : vec![("inner", apply(apply(Var("2"), Var("Box")), Prim("x'")))]}
            )
        ];

        for (left, right) in tests {
            let left1 = left.clone().eval_nf(env1).unwrap();
            assert_eq!(
                left1,
                right,
                "\nleft: {}\nright {}\n", left1, right);
        }
    }
}
