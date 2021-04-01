
use lang2::types::Type;

fn main() {
    use Type::*;

    let ty = Type::apply_n(Var("If"),
        vec![
            Var("False"),
            Struct{
                fields : vec![
                    ("a", Prim("a")),
                    ("b", Prim("b")),
                    ("c", Prim("c")),
                ]
            },
            Func{
                sig : vec![
                    Prim("b"),
                    Prim("a"),
                    Enum{
                        fields : vec![
                            ("a", Prim("a")),
                            ("b", Prim("b")),
                        ]
                    },
                ]
            },
        ]);

    let env = vec![
        ("hello", Prim("hello")),
        ("If", Type::abstract_n(vec!["P", "T", "F"],
            Type::apply_n(Var("P"), vec![Var("T"), Var("F")]))),
        ("True", Type::abstract_n(vec!["A", "B"], Var("A"))),
        ("False", Type::abstract_n(vec!["A", "B"], Var("B"))),
    ];

    println!("{:?}", ty.eval(env.as_slice()));
}

