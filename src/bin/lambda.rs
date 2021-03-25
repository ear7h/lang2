
use lang2::types::Type;

fn main() {
    use Type::*;

    let ty = Type::app(Var("If"),
        &[
            Var("False"),
            Struct{
                name : None,
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
                        name : "MyEnum",
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
        ("If", Type::abs( &["P", "T", "F"],
            Type::app(Var("P"), &[Var("T"), Var("F")]))),
        ("True", Type::abs(&["A", "B"], Var("A"))),
        ("False", Type::abs(&["A", "B"], Var("B"))),
    ];

    println!("{:?}", ty.eval(env.as_slice()));
}

