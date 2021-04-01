

/*
use pest::Parser;

#[derive(Parser)]
#[grammar = "grammar.pest"]
pub struct ParserImpl;
*/

use std::sync::Arc;
use std::fmt;

use lazy_static::lazy_static;
use lasso::{ThreadedRodeo, Spur};

type Type = crate::types::Type<Ident>;
type Def = crate::ast::Def<Ident>;
type DefItem = crate::ast::DefItem<Ident>;
type Func = crate::ast::Func<Ident, Type>;
type Expr = crate::ast::Expr<Ident>;



#[derive(Clone, Copy, Hash)]
pub struct Ident {
    key : Spur,
}

impl Ident {
    fn new<S>(s : S) -> Self
    where
        S : AsRef<str>
    {
        let key = IDENTS.get_or_intern(s);
        Ident{
            key : key
        }
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f : &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", IDENTS.resolve(&self.key))
    }
}

impl PartialEq for Ident {
    fn eq(&self, other : &Ident) -> bool {
        self.key == other.key
    }
}

impl Eq for Ident {}

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str {
        IDENTS.resolve(&self.key)
    }
}


lazy_static!{
    static ref IDENTS : Arc<ThreadedRodeo<Spur>> = Arc::new(ThreadedRodeo::new());
}


peg::parser!{grammar parser() for str {
    use super::Type;

    rule letter() -> &'input str
        = s:$(['a'..='z' | 'A'..='Z' | '_']) { s }

    rule digit10() -> &'input str
        = s:$(['0'..='9']) { s }

    pub rule ident() -> Ident
        = s:$(letter() (letter() / digit10())*) {
            Ident::new(s)
        }

    pub rule ident_opt() -> Option<Ident>
        = "_" { None }
        / x:ident() { Some(x) }

    // optional whitespace
    rule _() -> ()
        = [' ' | '\t' | '\r' | '\n']* {}

    // mandatory whitespace
    rule __() -> ()
        = [' ' | '\t' | '\r' | '\n']+ {}

    rule type_field() -> (Ident, Type)
        = id:ident() _ ":" _ ty:type_expr() { (id, ty) }

    rule type_fields() -> Vec<(Ident, Type)>
        = fields:type_field() ** (_ [','] _) { fields }

    rule type_struct() -> Type
        = "struct" _ "{" _ fields:type_fields() _ "}" {
            Type::Struct{
                fields
            }
        }

    rule type_enum() -> Type
        = "enum" _ "{" _ fields:type_fields() _ "}" {
            Type::Enum{
                fields
            }
        }

    rule type_value() -> Type
        = type_struct()
        / type_enum()
        / "_" { Type::Hole }
        / ident:ident() { Type::Var(ident) }
        / "(" x:type_expr() ")" { x }


    rule type_app() -> Type
        = x:type_value() **<1,>  __ {
            let mut it = x.into_iter();
            let f = it.next().unwrap();
            Type::apply_n(f, it)
        }

    rule type_abs_formals() -> Vec<Ident>
        = __ formals:ident() ** __ { formals }
        / _ { Vec::new() }

    rule type_abs_anon() -> Type
        = "type" __
            "_"
            formals:type_abs_formals() _
            "=" _
            body:type_value() {
                Type::abstract_n(formals, body)
            }

    rule type_abs_named() -> (Ident, Type)
        = "type" __
            name:ident()
            formals:type_abs_formals() _
            "=" _
            body:type_value() {
                (
                    name,
                    Type::abstract_n(formals, body)
                )
            }


    pub rule type_expr() -> Type
        = type_abs_anon()
        / type_app()
        / type_value()

    /// TODO: use big numbers
    rule num8() -> usize
        = "0o" n:$(['0'..='7' | '_' ]+) {
            let mut ret = 0;
            for b in n.bytes() {
                if b == ('_' as u8) {
                    continue
                }

                let b = (b - ('0' as u8)) & 7;
                ret = (ret << 3) | (b as usize);
            }

            ret
        }

    rule num16() -> usize
        = "0x" n:$(['0'..='9' | 'a'..='f' | 'A'..='F' | '_']+) {
            let mut ret = 0;
            for b in n.bytes() {
                let b = match (b as char) {
                    '0'..='9' => b - ('0' as u8),
                    'a'..='f' => 10 + b - ('a' as u8),
                    'A'..='F' => 10 + b - ('A' as u8),
                    '_' => continue,
                    _ => unreachable!(),
                };

                ret = (ret << 4) | (b as usize)
            }

            ret
        }

    rule num10() -> usize
        = n:$(['0'..='9' | '_' ]+) {
            let mut ret = 0;
            for b in n.bytes() {
                if b == ('_' as u8) {
                    continue
                }

                let b = (b - ('0' as u8)).min(9);
                ret = (ret * 10) + (b as usize);
            }

            ret
        }

    pub rule num_lit() -> usize
        = num8()
        / num16()
        / num10()

    rule string_escaped() -> ()
        = "\\" ['\\' | '"' | 't' | 'n' | 'r' | 'a' ]

    rule string_unescaped() -> ()
        = s:$([^ '"' | '\\' ]+) {}

    pub rule string_lit() -> String
        = "\"" s:$((string_escaped() / string_unescaped())*) "\"" {
            let mut it = s.chars();
            let mut res = String::new();

            loop {
                let c = if let Some(c) = it.next() {
                    c
                } else {
                    break
                };

                if c != '\\' {
                    res.push(c);
                    continue
                }

                let c = match it.next().unwrap() {
                    '\\' => '\\',
                    '"' => '"',
                    't' => '\t',
                    'n' => '\n',
                    'r' => '\r',
                    'a' => '\u{7}',
                    _ => unreachable!(), // TODO: verify this
                };

                res.push(c);
            }

            res
        }

    rule expr_num() -> Expr
        = x:num_lit() { Expr::Num(x) }

    rule expr_str() -> Expr
        = x:string_lit() { Expr::Str(Ident::new(x)) }

    rule expr_struct_name() -> Option<Ident>
        = "struct" { None }
        / name:ident() { Some(name) }

    rule expr_struct_field() -> (Ident, Expr)
        = field:ident() _ ":" _ value:expr() { (field, value) }

    rule expr_struct() -> Expr
        = name:expr_struct_name()
            "{"  _ fields:expr_struct_field() ** (_ "," _) _ "}" {
                Expr::Struct{name, fields}
            }

    rule expr_dot_tail() -> Vec<Ident>
        = "." _ names:ident() ** (_ "." _) { names }

    rule expr_dot() -> Expr
        = left:
            ( expr_num()
            / expr_str()
            / expr_struct()
            ) _ "." _ names:expr_dot_tail() { todo!() }

    rule expr() -> Expr
        = expr_dot()
        / expr_num()
        / expr_str()
        / expr_struct()

    rule func_ret_type() -> Type
        = "->" _ ty:type_expr() { ty}
        / _ { Type::Struct{fields : Vec::new()} }

    rule func_body() -> Expr
        = _ { Expr::Struct{name : None, fields: Vec::new()} }

    pub rule func() -> Func
        = "fn" __
            name:ident() _
            "(" formals:type_fields() ")" _
            ret:func_ret_type() _
            "{" _ body:func_body() _ "}" {
                Func{formals, ret, body}
            }

    pub rule def() -> Def
        = ty:type_abs_named() {
            Def{
                name : ty.0,
                item : DefItem::Type(ty.1)
            }
        }

    pub rule main() -> Vec<Def>
        = _ defs:def() ** __ _ { defs }
}}


#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn main() {
        let res = parser::main(r#"
            type a = struct{}
            type a = struct{}
        "#);
        println!("{:?}", res);
    }

    #[test]
    fn num_lit() {
        let tests = vec![
            ("0", Some(0)),
            ("10", Some(10)),
            ("42", Some(42)),
            ("4_2", Some(42)),
            ("4_2a", None),
            ("0777", Some(777)),
            ("0o777", Some(0o777)),
            ("0o6_4_4", Some(0o644)),
            ("0o999", None),
            ("0xdeadBEEF", Some(0xdeadBEEF)),
            ("0xdead_BEEF", Some(0xdeadBEEF)),
            ("0xxx", None),
        ];

        for (tc, exp) in tests {
            let got = parser::num_lit(tc);

            assert_eq!(exp, got.ok(), "\ntc: {:?}", tc)
        }
    }

    #[test]
    fn string_lit() {
        let tests = vec![
            (r#""abc\\def""#, Some("abc\\def")),
            (r#""abc\"def""#, Some("abc\"def")),
            (r#""abc\tdef""#, Some("abc\tdef")),
            (r#""abc\ndef""#, Some("abc\ndef")),
            (r#""abc\rdef""#, Some("abc\rdef")),
            (r#""abc\adef""#, Some("abc\u{7}def")),
            (r#""abc\ndef"#, None),
            (r#""abc\""#, None),
        ];

        for (tc, exp) in tests {
            let got = parser::string_lit(tc);
            assert_eq!(exp, got.ok().as_ref().map(String::as_str))
        }
    }

    #[test]
    fn type_expr() {
        let a = Ident::new("a");
        let b = Ident::new("b");
        let c = Ident::new("c");

        let tests = vec![
            (
                "struct {}",
                Some(Type::Struct{
                    fields : vec![],
                })
            ),
            (
                "struct {a : struct{}}",
                Some(Type::Struct{
                    fields : vec![
                        (a, Type::Struct{fields : vec![]})
                    ],
                })
            ),
            (
                "struct {a : b}",
                Some(Type::Struct{
                    fields : vec![
                        (a, Type::Var(b))
                    ],
                })
            ),
            (
                "enum {a : b}",
                Some(Type::Enum{
                    fields : vec![
                        (a, Type::Var(b))
                    ],
                })
            ),
            (
                "enum {a : b} c",
                Some(Type::Apply{
                    f : box Type::Enum{
                        fields : vec![
                            (a, Type::Var(b))
                        ],
                    },
                    arg : box Type::Var(c),
                })
            ),
            (
                "type _ = enum {b : a}",
                Some(Type::Enum{
                    fields : vec![
                        (b, Type::Var(a))
                    ],
                })
            ),
            (
                "type _ a = enum {b : a}",
                Some(Type::Abstract{
                    formal : a,
                    body: box Type::Enum{
                        fields : vec![
                            (b, Type::Var(a))
                        ],
                    }
                })
            ),
            (
                "(type _ a = enum {b : a}) c",
                Some(Type::Apply{
                    f : box Type::Abstract{
                        formal : a,
                        body: box Type::Enum{
                            fields : vec![
                                (b, Type::Var(a))
                            ],
                        }
                    },
                    arg : box Type::Var(c),
                })
            ),
        ];

        for (tc, exp) in tests {
            let got = parser::type_expr(tc);
            assert_eq!(
                exp.as_ref(),
                got.as_ref().ok(),
                "\ntc: {:}\nunparsed: {:?}",
                tc,
                got.as_ref().map(|_|"").map_err(|err| &tc[err.location.offset..]))
        }
    }

    #[test]
    fn ident() {
        let tests = vec![
            ("hello", true),
            ("hell o", false),
            ("0123", false),
            ("my0123", true),
            ("_", true),
            ("_hello", true),
        ];

        for (tc, exp) in tests {
            let got = parser::ident(tc);
            assert_eq!(
                exp.then(|| tc),
                got.ok().as_ref().map(AsRef::as_ref));
        }

    }
}
