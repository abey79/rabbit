use crate::interp::Interp;
use crate::model::{BinOpKind, Block, Comp, CompOpKind, Expr, Stmt, UnaryOpKind};

#[test]
fn proj1_program1() {
    let prog = Block::new(vec![Stmt::print(Expr::integer(42))]);

    println!("{prog:#?}");
    println!("{prog}");
    Interp::interp_block(&prog).unwrap();
}

#[test]
fn proj1_program2() {
    let prog = Block::new(vec![
        Stmt::print(Expr::integer(42)),
        Stmt::print(Expr::float(2.3)),
    ]);

    println!("{prog:#?}");
    println!("{prog}");
    Interp::interp_block(&prog).unwrap();
}

#[test]
fn proj1_program3() {
    let prog = Block::new(vec![
        // print 2 + 3;
        Stmt::print(Expr::bin_op(
            BinOpKind::Add,
            Expr::integer(2),
            Expr::integer(3),
        )),
        // print -5;
        Stmt::print(Expr::unary_op(UnaryOpKind::Neg, Expr::integer(5))),
        // print 2 + 3 * 4;
        Stmt::print(Expr::bin_op(
            BinOpKind::Add,
            Expr::integer(2),
            Expr::bin_op(BinOpKind::Mul, Expr::integer(3), Expr::integer(4)),
        )),
        // print 2 * (3 + 4);
        Stmt::print(Expr::bin_op(
            BinOpKind::Mul,
            Expr::integer(2),
            Expr::bin_op(BinOpKind::Add, Expr::integer(3), Expr::integer(4)),
        )),
        // print (2 + 3) * 4;
        Stmt::print(Expr::bin_op(
            BinOpKind::Mul,
            Expr::bin_op(BinOpKind::Add, Expr::integer(2), Expr::integer(3)),
            Expr::integer(4),
        )),
    ]);

    println!("{prog:#?}");
    println!("{prog}");
    Interp::interp_block(&prog).unwrap();
}

#[test]
fn proj1_program4() {
    let prog = Block::new(vec![
        // const pi = 3.14159;
        Stmt::const_def("pi", None, Expr::float(3.14159)),
        // var radius = 4.0;
        Stmt::var_def("radius", None, Some(Expr::float(4.0))),
        // var perimeter float;
        Stmt::var_def("perimeter", Some("float".into()), None),
        // perimeter = 2.0*radius*pi;
        Stmt::assign(
            "perimeter",
            Expr::bin_op(
                BinOpKind::Mul,
                Expr::bin_op(BinOpKind::Mul, Expr::float(2.0), Expr::variable("radius")),
                Expr::variable("pi"),
            ),
        ),
        // print perimeter;
        Stmt::print(Expr::variable("perimeter")),
    ]);

    println!("{prog:#?}");
    println!("{prog}");
    Interp::interp_block(&prog).unwrap();
}

#[test]
fn proj1_program5() {
    /*
    var a int = 2;
    var b int = 3;
    if a < b {
       print a;
    } else {
       print b;
    }
     */
    let prog = Block::new(vec![
        Stmt::var_def("a", Some("int".into()), Some(Expr::integer(2))),
        Stmt::var_def("b", Some("int".into()), Some(Expr::integer(3))),
        Stmt::if_(
            Expr::comp_op(
                Expr::variable("a"),
                [Comp::new(CompOpKind::Lt, Expr::variable("b"))],
            ),
            Block::new(vec![Stmt::print(Expr::variable("a"))]),
            Some(Block::new(vec![Stmt::print(Expr::variable("b"))])),
        ),
    ]);

    println!("{prog:#?}");
    println!("{prog}");
    Interp::interp_block(&prog).unwrap();
}

#[test]
fn proj1_program6() {
    /*
    var x int = 1;
    var fact int = 1;

    while x < 11 {
        fact = fact * x;
        x = x + 1;
        print fact;
    }
    */

    let prog = Block::new(vec![
        Stmt::var_def("x", Some("int".into()), Some(Expr::integer(1))),
        Stmt::var_def("fact", Some("int".into()), Some(Expr::integer(1))),
        Stmt::while_(
            Expr::comp_op(
                Expr::variable("x"),
                [Comp::new(CompOpKind::Lt, Expr::integer(11))],
            ),
            Block::new(vec![
                Stmt::assign(
                    "fact",
                    Expr::bin_op(BinOpKind::Mul, Expr::variable("fact"), Expr::variable("x")),
                ),
                Stmt::assign(
                    "x",
                    Expr::bin_op(BinOpKind::Add, Expr::variable("x"), Expr::integer(1)),
                ),
                Stmt::print(Expr::variable("fact")),
            ]),
        ),
    ]);

    println!("{prog:#?}");
    println!("{prog}");
    Interp::interp_block(&prog).unwrap();
}

#[test]
fn test_precedence() {
    let prog = Block::new(vec![Stmt::var_def(
        "x",
        Some("int".into()),
        Some(Expr::unary_op(
            UnaryOpKind::Neg,
            Expr::bin_op(BinOpKind::Add, Expr::integer(2), Expr::integer(3)),
        )),
    )]);

    println!("{prog:#?}");
    println!("{prog}");
}
