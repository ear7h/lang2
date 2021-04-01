#![allow(dead_code)]

pub enum Pattern {
    Var(String), // creates a binding
    Number(String), // match a literal number

    /// match a list
    List{
        /// sub patterns for list elemets
        pats : Vec<Pattern>,
        /// the location of a rest pattern if it exists
        /// the rest is located before the pattern with the specified idx
        /// ex:
        ///      rest_idx == 0 => first pattern is a rest
        ///      rest_idx == len(pats) => last pattern is a rest
        rest_idx : Option<usize>,
    },

    /// Pattern match a struct
    Struct{
        fields : Vec<(String, Box<Pattern>)>,
    },
}


pub enum Expr<Ident> {
    /// Number literal
    Num(usize),
    /// String literal
    Str(Ident),
    /// Struct literal
    Struct{
        name : Option<Ident>,
        fields : Vec<(Ident, Self)>,
    },
    /// scope resolution operator
    Dot(Box<Self>, Ident),
    Match{
        var : Box<Self>,
        arms : Vec<(Pattern, Self)>,
    },
    Call{
        func : Box<Self>,
        args : Vec<Self>,
    },
    Assign{
        dst : Box<Self>,
        src : Box<Self>,
    },
    Block(Vec<Self>),
}

pub struct Func<Ident, Type>{
    pub formals : Vec<(Ident, Type)>,
    pub ret : Type,
    pub body : Expr<Ident>,
}

#[derive(Debug)]
pub enum DefItem<Ident> {
    Type(crate::types::Type<Ident>),
}

#[derive(Debug)]
pub struct Def<Ident> {
    pub name : Ident,
    pub item : DefItem<Ident>,
}
