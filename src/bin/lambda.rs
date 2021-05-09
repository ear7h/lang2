
use lang2::types::Type;

fn main() {
    use Type::*;

    let ty = Type::apply_n(
        Var("If"),
        vec![
            Var("True"),
            Struct{
                fields : vec![
                    ("a", Prim("a")),
                    ("b", Prim("b")),
                    ("c", Prim("c")),
                ]
            },
            Func{
                sig : vec![
                    Prim("A"),
                    Prim("B"),
                    Enum{
                        fields : vec![
                            ("a", Prim("A")),
                            ("b", Prim("A")),
                        ]
                    },
                ]
            },
        ]
    );

    let env = vec![
        ("hello", Prim("hello")),
        ("If", Type::abstract_n(vec!["P", "T", "F"],
            Type::apply_n(Var("P"), vec![Var("T"), Var("F")]))),
        ("True", Type::abstract_n(vec!["A", "B"], Var("A"))),
        ("False", Type::abstract_n(vec!["A", "B"], Var("B"))),
    ];

    match Var("If").eval(env.as_slice()) {
        Ok(s) => println!("{}", s),
        Err(s) => println!("{}", s),
    };

    match ty.eval(env.as_slice()) {
        Ok(s) => println!("{}", s),
        Err(s) => println!("{}", s),
    };
}

