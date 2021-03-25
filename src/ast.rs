

enum ErrorKind {
    InvalidPattern
}

impl ErrorKind {
    fn message<T>(self, msg : &str) -> Result<T> {
        Err(Error{
            kind : self,
            msg : msg.to_string(),
        })
    }
}


struct Error {
    kind : ErrorKind,
    msg : String
}

type Result<T> = std::result::Result<T, Error>;

type TypeId = usize;

/// a hand written value
/// note that they all have associated `TypeId`s
enum Literal {
    Number(String, TypeId),
    String(String, TypeId),
    Var(String, TypeId),
    Struct{
        type_id : TypeId,
        fields : Vec<(String, Literal)>,
    },
    Func {
        type_id : TypeId,
        args : Vec<(String, TypeId)>,
        ret : TypeId,
        body : Box<Expr>,
    }
}

enum Pattern {
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

enum Expr {
    Literal(Literal),
    Match{
        var : Box<Expr>,
        arms : Vec<(Pattern, Expr)>,
    },
    Call{
        func : Box<Expr>,
        args : Vec<Expr>,
    },
    Assign{
        dst : Box<Expr>,
        src : Box<Expr>,
    },
    Block(Vec<Expr>),
}

struct FuncDecl {
    name : String,
    args : Vec<(String, TypeId)>,
    ret : TypeId,
    body : Expr,
}




