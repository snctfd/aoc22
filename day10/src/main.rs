use std::{str::FromStr, io};
use std::io::BufRead;

struct CPU {
    x_reg: i32,
    prog_counter: usize,
    cycle_counter: i32,
    program: Vec<Instruction>,
    signal_str: i32,
    addx_done: bool
}

#[derive(Clone, Copy, Debug)]
enum Instruction {
    AddX(i32),
    Noop
}

#[derive(Debug, PartialEq, Eq)]
struct ParseError;

impl CPU {
    fn step(&mut self) {
        let instr = self.program[self.prog_counter];
        self.cycle_counter += 1;

        if self.cycle_counter % 40 == 20 {
            self.signal_str += self.x_reg * self.cycle_counter;
        }

        self.draw();
        
        match instr {
            Instruction::AddX(n) => self.do_addx(n),
            Instruction::Noop => self.prog_counter += 1
        };
    }

    fn draw(&self) {
        let draw_pos = (self.cycle_counter - 1) % 40;
        if (self.x_reg - draw_pos).abs() <= 1 {
            print!("#");
        } else {
            print!(".")
        }

        if self.cycle_counter % 40 == 0 {
            print!("\n");
        }
    }

    fn do_addx(&mut self, n: i32) {
        if !self.addx_done {
            // do first step; just mark addx as done, do not advance PC
            self.addx_done = true;
        } else {
            // second step; add to X register and mark addx as "not done" for next instruction. advance PC.
            self.x_reg += n;
            self.addx_done = false;
            self.prog_counter += 1;
        }
    }

    fn new(program: Vec<Instruction>) -> CPU {
        CPU { 
            x_reg: 1, 
            prog_counter: 0, 
            cycle_counter: 0, 
            program: program, 
            addx_done: false,
            signal_str: 0
        }
    }
}

impl FromStr for Instruction {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut split = s.split(' ');

        match split.next() {
            Some("noop") => Ok(Instruction::Noop),
            Some("addx") => {
                let n = split
                    .next()
                    .and_then(|n| n.parse::<i32>().ok())
                    .ok_or(ParseError)?;

                Ok(Instruction::AddX(n))
            }
            _ => Result::Err(ParseError)
        }
    }
}

fn main() {
    let stdin = io::stdin();

    let program: Vec<Instruction> = stdin
        .lock()
        .lines()
        .map(|line| Instruction::from_str(&line.unwrap()).unwrap())
        .collect();
    let mut cpu = CPU::new(program);
    while cpu.prog_counter < cpu.program.len() {
        cpu.step();
    }

    println!("total signal strength: {}", cpu.signal_str)
}
