# Ferrous Metal

To provide context on how CPU/VM work, the course provide a tiny bytecode VM in Python and some exercise where a few programs must be completed.

As warmup exercise, I reimplemented the VM in Rust and completed the exercises as tests. I also implemented a `prog!` procedural macro (see the `ferrous_metal_macro` crate) so that programs may be writen with labels and corresponding addresses and relative offsets:

```rust
fn main() {
    let mut machine = FerrousMetal::new();
    
    // compute 42 * 37
    machine.set_reg(R1, 42);
    machine.set_reg(R2, 37);
    machine.run(&prog!(
            CONST(0, R3),
        loop_start:
            BZ(R2, #exit),
            ADD(R1, R3, R3),
            DEC(R2),
            JMP(loop_start),
        exit:
            ADD(R3, R0, R1),    // R0 is always 0!
            HALT,
    ));
    
    assert_eq!(machine.reg(R1), 42 * 37);
}
```