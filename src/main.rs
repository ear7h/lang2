
use lang2::ir::*;


fn main() {

    let mut b = Block{
        nlabels: 0,
        ops: Vec::new()
    };

    let x = b.arg(0, "string");
    let f = b.arg(1, "fn (string) -> string");
    let c = b.arg(2, "bool");
    let label = b.new_label();
    let ret1 = b.call(f, vec![x], "string");
    b.cond_jump(c, label, None);
    let ret2 = b.call(f, vec![x], "string");
    b.label(label);
    b.phi(vec![ret1, ret2], "string");

    println!("{:}", b);
}

