use ferrous_metal_macro::prog;

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    ADD(usize, usize, usize),
    SUB(usize, usize, usize),
    JMP(i32),
    BZ(usize, i32),
    HALT,
}

#[test]
fn playground() {
    use Instruction::*;

    let test = prog!(
        ADD(0, test, test),
        ADD(0, 0, 0),
        ADD(0, 0, 0),
        ADD(0, 0, 0),
        ADD(0, 0, 0),
        ADD(0, 0, 0),
        test:
        SUB(2, #test2, test),
        ADD(0, 0, 0),
        ADD(0, 0, 0),
        ADD(0, 0, 0),
        ADD(0, 0, 0),
        test2:
        HALT,);
    println!("{:?}", test);
}
