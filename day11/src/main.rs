use std::io;
use std::io::BufRead;

#[derive(Debug, Clone)]
struct Monkey {
    items: Vec<u64>,
    operation: Operation,
    modulus: u64,
    true_monkey: u64,  // throws to this monkey if item % mod == 0
    false_monkey: u64, // throws to this monkey if item % mod != 0
    inspect_counter: u64
}

#[derive(Debug, Clone)]
enum Operation {
    Mul(Operand, Operand),
    Add(Operand, Operand)
}

#[derive(Debug, Clone)]
enum Operand {
    Int(u64),
    OldVal
}

impl Operation {
    fn apply(&self, old_val: u64) -> u64 {
        match self {
            Self::Mul(lhs, rhs) => lhs.val(old_val) * rhs.val(old_val),
            Self::Add(lhs, rhs) => lhs.val(old_val) + rhs.val(old_val)
        }
    }
}

impl Operand {
    fn val(&self, old_val: u64) -> u64 {
        match self {
            Self::Int(n) => *n,
            Self::OldVal => old_val
        }
    }
}

fn read_monkeys() -> Vec<Monkey> {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();
    let mut monkeys = Vec::new();
    

    while lines.next().is_some() { // consumes "Monkey [n]:"
        let items = parse_items(lines.next().unwrap().unwrap()); // consumes "Starting items: [list]"
        let operation = parse_op(lines.next().unwrap().unwrap()).unwrap(); // consumes "Operation: [op]"
        let modulus = parse_int(lines.next().unwrap().unwrap()); // consumes "Test: divisible by [n]"
        let true_monkey = parse_int(lines.next().unwrap().unwrap()); // consumes "If true: throw to monkey [n]"
        let false_monkey = parse_int(lines.next().unwrap().unwrap()); // consumes "If false: throw to monkey [n]"

        monkeys.push(Monkey {
            items,
            operation,
            modulus,
            true_monkey,
            false_monkey,
            inspect_counter: 0
        });

        lines.next(); // skip blank line
    }

    monkeys
}

fn parse_op(line: String) -> Option<Operation> {
    let words: Vec<&str> = line.split(' ').collect();
    let lhs = parse_operand(words[5]);
    let op = words[6];
    let rhs = parse_operand(words[7]);

    match op {
        "+" => Some(Operation::Add(lhs, rhs)),
        "*" => Some(Operation::Mul(lhs, rhs)),
        _ => None
    }
}

fn parse_operand(word: &str) -> Operand {
    match word {
        "old" => Operand::OldVal,
        _ => Operand::Int(word.parse().unwrap())
    }
}

fn parse_items(line: String) -> Vec<u64> {
    line
        .split(' ')
        .flat_map(|word| word.strip_suffix(',').unwrap_or(word).parse::<u64>())
        .collect()
    
}

fn parse_int(line: String) -> u64 {
    line
        .split(' ')
        .last()
        .map(|word| word.parse::<u64>())
        .unwrap().unwrap()
}

fn do_monkey_business(mut monkeys: Vec<Monkey>, rounds: u64, relief: u64) -> u64 {
    // multiply all monkeys' moduli together to find a common modulus
    let monkeys_modulo = monkeys.iter().map(|m| m.modulus).reduce(|x, y| x * y).unwrap();
    
    for _ in 0..rounds {
        for i in 0..monkeys.len() {
            for _ in 0..monkeys[i].items.len() {
                monkeys[i].inspect_counter += 1;
                
                let mut item = monkeys[i].items.pop().unwrap();
                item = monkeys[i].operation.apply(item);
                item /= relief;

                if item % monkeys[i].modulus == 0 {
                    let dst = monkeys[i].true_monkey;
                    monkeys[dst as usize].items.push(item % monkeys_modulo);
                } else {
                    let dst = monkeys[i].false_monkey;
                    monkeys[dst as usize].items.push(item % monkeys_modulo);
                }
            }
        }
    }

    let mut counts: Vec<u64> = monkeys.iter().map(|m| m.inspect_counter).collect();
    counts.sort();
    counts.reverse();

    counts[0] * counts[1]
}

fn main() {
    let monkeys = read_monkeys(); 
    let monkey_business = do_monkey_business(monkeys.clone(), 20, 3);
    let monkey_business_long = do_monkey_business(monkeys.clone(), 10000, 1);
    println!("monkey business: {}", monkey_business);
    println!("long monkey business: {}", monkey_business_long);
}
