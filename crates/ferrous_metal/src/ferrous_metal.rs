use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Register {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    PC,
}

#[derive(Debug, Clone, Copy)]
#[allow(clippy::upper_case_acronyms)]
pub enum Instruction {
    ADD(Register, Register, Register), // ra, rb, rd       rd = ra + rb
    SUB(Register, Register, Register), // ra, rb, rd       rd = ra - rb
    INC(Register),                     // ra               ra = ra + 1
    DEC(Register),                     // ra               ra = ra - 1
    LT(Register, Register, Register),  // ra, rb, rd       rd = (ra < rb)
    CONST(i32, Register),              // value, rd        rd = value
    LOAD(Register, Register),          // rs, rd           rd = MEMORY[rs]
    STORE(Register, Register),         // rs, rd           MEMORY[rd] = rs
    JMP(i32),                          // address          PC = address
    BZ(Register, i32),                 // rt, offset       if rt == 0: PC = PC + offset
    PRINT(Register),
    HALT, //                  Halts machine
}

pub struct FerrousMetal {
    registers: HashMap<Register, i32>,
    memory: [i32; u16::MAX as usize],
    running: bool,
}

impl FerrousMetal {
    pub fn new() -> Self {
        Self {
            registers: HashMap::new(),
            memory: [0; u16::MAX as usize],
            running: false,
        }
    }

    pub fn run(&mut self, program: &[Instruction]) {
        self.running = true;
        self.set_reg(Register::PC, 0);

        while self.running {
            let pc = self.reg(Register::PC);
            let instruction = program[pc as usize];
            self.set_reg(Register::PC, pc + 1);
            self.execute(instruction);
            self.set_reg(Register::R0, 0);
        }
    }

    fn reg(&self, reg: Register) -> i32 {
        *self.registers.get(&reg).unwrap_or(&0)
    }

    fn set_reg(&mut self, reg: Register, value: i32) {
        self.registers.insert(reg, value);
    }

    pub fn execute(&mut self, instr: Instruction) {
        use Instruction::*;

        match instr {
            ADD(ra, rb, rd) => self.set_reg(rd, self.reg(ra) + self.reg(rb)),
            SUB(ra, rb, rd) => self.set_reg(rd, self.reg(ra) - self.reg(rb)),
            INC(ra) => self.set_reg(ra, self.reg(ra) + 1),
            DEC(ra) => self.set_reg(ra, self.reg(ra) - 1),
            LT(ra, rb, rd) => self.set_reg(rd, if self.reg(ra) < self.reg(rb) { 1 } else { 0 }),
            CONST(val, rd) => self.set_reg(rd, val),
            LOAD(rs, rd) => self.set_reg(rd, self.memory[self.reg(rs) as usize]),
            STORE(rs, rd) => self.memory[self.reg(rd) as usize] = self.reg(rs),
            JMP(address) => self.set_reg(Register::PC, address),
            BZ(rt, offset) => {
                if self.reg(rt) == 0 {
                    self.set_reg(Register::PC, self.reg(Register::PC) + offset)
                }
            }
            PRINT(reg) => println!("{:?} = {}", reg, self.reg(reg)),
            HALT => self.running = false,
        }
    }
}

#[cfg(test)]
mod test {
    use super::Instruction::*;
    use super::Register::*;
    use super::*;
    use ferrous_metal_macro::prog;

    #[test]
    fn test_program1() {
        let mut machine = FerrousMetal::new();
        machine.run(&prog!(
            CONST(3, R1),
            CONST(4, R2),
            CONST(5, R3),
            ADD(R1, R2, R4),
            SUB(R4, R3, R4),
            CONST(1000, R6),
            STORE(R4, R6),
            HALT,
        ));
        assert_eq!(machine.memory[1000], 2);
    }

    #[test]
    fn test_program2() {
        let mut machine = FerrousMetal::new();
        machine.set_reg(R1, 42);
        machine.set_reg(R2, 37);
        machine.run(&prog!(
                // R1 contains a
                // R2 contains b
                CONST(0, R3),       // store result
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

    #[test]
    fn test_program3() {
        let mut machine = FerrousMetal::new();
        machine.memory[1000] = 3;

        machine.run(&prog!(
                CONST(1000, R6),
                LOAD(R6, R1),  // r1 = input
                CONST(1, R2),  // r2 = output
            outer_loop:
                BZ(R1, #exit_outer),

                // multiply r2 by r4
                ADD(R1, R0, R3),
                ADD(R2, R0, R4),
            inner_loop:
                DEC(R3),
                BZ(R3, #exit_inner),
                ADD(R2, R4, R2),
                JMP(inner_loop),
            exit_inner:
                DEC(R1),
                JMP(outer_loop),
            exit_outer:
                CONST(1001, R6),
                STORE(R2, R6),
                HALT,
        ));

        assert_eq!(machine.memory[1001], 6);
    }
}
