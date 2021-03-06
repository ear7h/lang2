/* references
 *
 * https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=174ca95a8b938168764846e97d5e9a2c
 */

#![allow(dead_code)]
use std::collections::hash_map::*;
use std::collections::hash_set::*;
use std::hash::Hash;

use std::fmt;
use std::rc::Rc;

use crate::types::{Type, Env, Error};


/// TypeInfo holds type information about variables
/// TODO: add Option<Ident> to the Ref type? since names aren't on the structs anymore
/// limiting the depth of reconstruction isn't possible.
#[derive(Clone, Debug)]
pub enum TypeInfo<Ident> {
    /// a type that has not been inferred
    Unknown,
    /// trait bounds on another type
    /// TODO: matching trait impls to types (in particular for blanket impls)
    /// requires unifying the the proper type to the implementation signature
    /// and constructing the resulting type to make sure it's valid
    Constrained(HashSet<TraitId>, VarId),
    /// a primitive type
    Prim(Ident),
    /// this variable's type is dependent on another varible's type
    Ref(VarId),
    Enum{
        fields : Vec<(Ident, VarId)>,
    },
    Struct{
        fields : Vec<(Ident, VarId)>,
    },
    Func{
        sig : Vec<VarId>,
    }
}



type VarId = usize;
type TraitId = usize;


#[derive(Default)]
pub struct Engine<Ident> {

    /// globally defined types, this is
    global_types : Rc<HashMap<Ident, Type<Ident>>>,
    /// locally defined types, can be added in the middle of
    local_types : HashMap<Ident, Type<Ident>>,


    /// vars is a list of variables and their types
    vars : Vec<TypeInfo<Ident>>,
}

impl<Ident : Clone + fmt::Debug + Eq + Hash> Env<Ident> for Engine<Ident> {
    fn get(&self, name : &Ident) -> Option<&Type<Ident>> {
        self.local_types.get(name)
            .or_else(|| self.global_types.get(name))
    }
}

impl<Ident : Clone + fmt::Debug + Eq + Hash> Engine<Ident> {

    /*
    pub fn get_type(&self, name : &Ident) -> Option<&Type<Ident>> {
        self.local_types.get(name)
            .or_else(|| self.global_types.get(name))
    }
    */

    pub fn insert_local_type(&mut self, name : Ident, ty : Type<Ident>) {
        self.local_types.insert(name, ty);
    }

    pub fn insert_info(&mut self, info : TypeInfo<Ident>) -> VarId {
        let next_id = self.vars.len();
        self.vars.push(info);
        next_id
    }

    pub fn insert_info_for_type(&mut self, ty : &Type<Ident>) -> Result<VarId, Error<Ident>> {
        match ty {
            Type::Hole => Ok(self.insert_info(TypeInfo::Unknown)),
            Type::Var(name) => {
                let ty = self.get(name)
                    .ok_or(Error::Undefined(name.clone()))?
                    .clone();
                self.insert_info_for_type(&ty)
            },
            Type::Prim(name) => Ok(self.insert_info(TypeInfo::Prim(name.clone()))),
            Type::Enum{fields} => {
                let mut ret = Vec::new();
                for (name, ty) in fields {
                    ret.push((name.clone(), self.insert_info_for_type(ty)?));
                }

                Ok(self.insert_info(TypeInfo::Enum{
                    fields : ret,
                }))
            },
            Type::Struct{fields} => {
                let mut ret = Vec::new();
                for (name, ty) in fields {
                    ret.push((name.clone(), self.insert_info_for_type(ty)?));
                }

                Ok(self.insert_info(TypeInfo::Struct{
                    fields : ret,
                }))
            },
            Type::Func{sig} => {
                let mut ret = Vec::new();
                for ty in sig {
                    ret.push(self.insert_info_for_type(ty)?);
                }

                Ok(self.insert_info(TypeInfo::Func{
                    sig : ret
                }))
            },
            Type::Abstract{..} => {
                Err(Error::CannotInferHigherKinds(ty.clone()))
            },
            Type::Apply{..} => {
                self.insert_info_for_type(&ty.eval(self)?)
            }
        }
    }

    pub fn unify(&mut self, a: VarId, b : VarId) -> Result<(), String> {
        use TypeInfo::*;

        let m = (
            (a, self.vars[a].clone()),
            (b, self.vars[b].clone()),
        );

        match m {
            ((_, Ref(a)), (b, _)) |
            ((b, _), (_, Ref(a))) => self.unify(a, b),

            (
                (a, Constrained(mut a_traits, aa)),
                (b, Constrained(b_traits, bb)),
            ) => {
                self.unify(aa, bb)?;

                // unifiying two polymorphic types
                // we add the trait constraints from a to b
                // and modify b to refer to a

                for x in b_traits {
                    a_traits.insert(x);
                }

                self.vars[a] = Constrained(a_traits, aa);
                self.vars[b] = Ref(a);
                Ok(())
            },
            ((a, Constrained(_, aa)), (b, _)) |
            ((b, _), (a, Constrained(_, aa))) => {
                self.unify(aa, b)?;
                self.vars[b] = Ref(a);
                Ok(())
            },
            ((a, Unknown), (b, _)) |
            ((b, _), (a, Unknown)) => {
                self.vars[a] = Ref(b);
                Ok(())
            },
            // matching primitives
            ((_, Prim(a_name)), (_, Prim(b_name)))
            if a_name == b_name => {
                Ok(())
            },
            (
                (_, Enum{fields : a_fields}),
                (_, Enum{fields : b_fields}),
            )  => {
                for (af, bf) in a_fields.iter().zip(b_fields.iter()) {
                    if af.0 != bf.0 {
                        return Err(format!("struct field name mismatch {:?} {:?}", af, bf))
                    }

                    self.unify(af.1, bf.1)?;
                }

                Ok(())
            },
            (
                (_, Struct{fields : a_fields}),
                (_, Struct{fields : b_fields}),
            )  => {

                for (af, bf) in a_fields.iter().zip(b_fields.iter()) {
                    if af.0 != bf.0 {
                        return Err(format!("struct field name mismatch {:?} {:?}", af, bf))
                    }

                    self.unify(af.1, bf.1)?;
                }

                Ok(())
            },
            (
                (_, Func{sig : a_sig}),
                (_, Func{sig : b_sig}),
            ) => {
                for (aa, bb) in a_sig.iter().zip(b_sig.iter()) {
                    self.unify(*aa, *bb)?;
                }
                Ok(())
            },
            (a, b) => Err(format!("Conflict between {:?} and {:?}", a, b)),
        }
    }
    /// if unknwon_as_hole is false, errors are returned when Unknown type infos are found
    /// depth is the maximum number of named types to follow
    fn reconstruct(&self,
        id : VarId,
        unknown_as_hole : bool,
        depth : Option<usize>,
    ) -> Result<Type<Ident>, VarId> {

        use TypeInfo::*;

        match &self.vars[id] {
            Unknown if unknown_as_hole => Ok(Type::Hole),
            Unknown => Err(id),
            Constrained(_trs, _id) => {
                todo!()
            },
            Ref(id) => self.reconstruct(*id, unknown_as_hole, depth),
            Prim(name) => Ok(Type::Prim(name.clone())),
            Enum{fields} => {
                let depth = match depth {
                    None => None,
                    Some(n) => Some(n-1),
                };

                let mut ret = Vec::with_capacity(fields.len());

                for (name, vid) in fields {
                    ret.push((
                            name.clone(),
                            self.reconstruct(*vid, unknown_as_hole, depth)?
                    ));
                }

                Ok(Type::Enum{
                    fields : ret,
                })
            },
            Struct{fields} => {
                let depth = match depth {
                    None => None,
                    Some(n) => Some(n-1),
                };

                let mut ret = Vec::with_capacity(fields.len());
                for (name, vid) in fields {
                    ret.push((
                            name.clone(),
                            self.reconstruct(*vid, unknown_as_hole, depth)?
                    ));
                }

                Ok(Type::Struct{
                    fields : ret,
                })
            },
            Func{sig} => {
                let mut ret = Vec::with_capacity(sig.len());
                for vid in sig {
                    ret.push(
                            self.reconstruct(*vid, unknown_as_hole, depth)?
                    );
                }

                Ok(Type::Func{
                    sig : ret,
                })
            },
            //_ => unimplemented!(),
        }

    }
}

#[cfg(test)]
mod tests {
    #![allow(unused_imports, unused_macros)]

    use super::*;

    macro_rules! init {
        () => {
            use TypeInfo::*;

            let mut e : Engine<_> = Default::default();

            e.insert_local_type("int", Type::Prim("int"));
            e.insert_local_type("bool", Type::Prim("bool"));
            e.insert_local_type("Option", Type::abstract_n(
                vec!["T"],
                Type::Enum{
                    fields : vec![
                        ("Some", Type::Var("T")),
                        ("None", Type::Prim("()")),
                    ]
                }
            ));

            macro_rules! unk {
                () => {
                    e.insert_info(Unknown)
                };
            }


            macro_rules! info_for_type {
                ($ty:expr) => {
                    e.insert_info_for_type(&$ty).unwrap()
                };
            }

            macro_rules! type_var {
                ($name:expr) => {
                    e.insert_info_for_type(Type::Var($name)).unwrap()
                };
            }

            macro_rules! prim {
                ($name:expr) => {
                    e.insert_info(Prim($name))
                };
            }

            macro_rules! func {
                ($sig0:expr) => {{
                    let mut x = Vec::new();

                    x.push($sig0);

                    e.insert_info(Func{sig : x})
                }};
                ($sig0:expr,$sig1:expr) => {{
                    let mut x = Vec::new();

                    x.push($sig0);
                    x.push($sig1);

                    e.insert_info(Func{sig : x})
                }};
                ($sig0:expr,$sig1:expr,$sig2:expr) => {{
                    let mut x = Vec::new();

                    x.push($sig0);
                    x.push($sig1);
                    x.push($sig2);

                    e.insert_info(Func{sig : x})
                }};
            }

            macro_rules! structure {
                ($name:expr,
                 $n0:expr, $t0:expr) => {{
                    let mut x = Vec::new();

                    x.push(($n0, $t0));

                    e.insert_info(Struct{fields : x})
                }};
                ($name:expr,
                 $n0:expr, $t0:expr,
                 $n1:expr, $t1:expr) => {{
                    let mut x = Vec::new();

                    x.push(($n0, $t0));
                    x.push(($n1, $t1));

                    e.insert_info(Struct{fields : x})
                }};
                ($name:expr,
                 $n0:expr, $t0:expr,
                 $n1:expr, $t1:expr,
                 $n2:expr, $t2:expr) => {{
                    let mut x = Vec::new();

                    x.push(($n0, $t0));
                    x.push(($n1, $t1));
                    x.push(($n2, $t2));

                    e.insert_info(Struct{fields : x})
                }};
            }

            macro_rules! enumeration {
                ($name:expr,
                 $n0:expr, $t0:expr) => {{
                    let mut x = Vec::new();

                    x.push(($n0, $t0));

                    e.insert_info(Enum{fields : x})
                }};
                ($name:expr,
                 $n0:expr, $t0:expr,
                 $n1:expr, $t1:expr) => {{
                    let mut x = Vec::new();

                    x.push(($n0, $t0));
                    x.push(($n1, $t1));

                    e.insert_info(Enum{fields : x})
                }};
                ($name:expr,
                 $n0:expr, $t0:expr,
                 $n1:expr, $t1:expr,
                 $n2:expr, $t2:expr) => {{
                    let mut x = Vec::new();

                    x.push(($n0, $t0));
                    x.push(($n1, $t1));
                    x.push(($n2, $t2));

                    e.insert_info(Enum{fields : x})
                }};
            }

            macro_rules! unify {
                ($a:expr, $b:expr) => {
                    e.unify($a, $b).unwrap()
                };
            }

            macro_rules! type_dbg {
                ($id:expr) => {
                    {
                        println!("{:?}",
                                 e.reconstruct($id, true, None).unwrap());
                        $id
                    }
                }
            }

            macro_rules! type_test {
                ($id:expr) => {
                    {
                        println!("{:?}",
                                 e.reconstruct($id, false, None).unwrap());
                        $id
                    }
                }
            }
        };
    }


    #[test]
    fn infer_func() {
        init!();

        let f1 = func!(
            prim!("bool"),
            unk!());

        let f2 = func!(
            unk!(),
            prim!("int"));

        unify!(
            type_dbg!(f1),
            type_dbg!(f2));

        println!();

        type_test!(f1);
        type_test!(f2);
    }

    #[test]
    fn infer_func2() {
        init!();

        let f1 = func!(
            prim!("int"),
            unk!());

        let f2 = func!(
            prim!("int"),
            unk!());

        let f3 = func!(
            prim!("int"),
            prim!("int"));

        unify!(
            type_dbg!(f1),
            type_dbg!(f2));

        unify!(
            type_dbg!(f2),
            type_dbg!(f3));

        println!();

        type_test!(f1);
        type_test!(f2);
        type_test!(f3);
    }

    #[test]
    fn infer_struct() {
        init!();

        let s1 = structure!(
            None,
            "a", prim!("int"),
            "b", unk!()
        );

        let s2 = structure!(
            None,
            "a", prim!("int"),
            "b", prim!("bool")
        );

        let v1 = unk!();

        // identity function
        func!(v1, v1);

        unify!(v1, s1);
        unify!(v1, s2);

        type_test!(s1);
        type_test!(s2);
    }

    #[test]
    fn infer_generic_enum() {
        init!();

        let s1 = info_for_type!(Type::Apply{
            f : box Type::Var("Option"),
            arg : box Type::Hole,
        });

        let s2 = enumeration!(
            "Option",
            "Some", prim!("int"),
            "None", prim!("()")
        );

        unify!(s1, s2);

        type_test!(s1);
        type_test!(s2);
    }

    #[test]
    #[should_panic]
    fn fail_type_abs() {
        init!();
        info_for_type!(Type::Var("Option"));
    }

}
