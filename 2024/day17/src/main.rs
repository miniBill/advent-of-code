use rayon::prelude::*;

struct Machine {
    a: u64,
    b: u64,
    c: u64,
    program: Vec<u8>,
}

fn main() {
    // let example1 = Machine {
    //     a: 0,
    //     b: 0,
    //     c: 9,
    //     program: vec![2, 6, 5, 5],
    // };
    // let expected_example1_part1 = "1";

    let example2 = Machine {
        a: 2024,
        b: 0,
        c: 0,
        program: vec![0, 3, 5, 4, 3, 0],
    };
    let expected_example2_part2 = 117440;

    if part2(&example2) != expected_example2_part2 {
        panic!("Wrong result for the example");
    } else {
        println!("Example checks out.")
    }

    // let example3 = Machine {
    //     a: 729,
    //     b: 0,
    //     c: 0,
    //     program: vec![0, 1, 5, 4, 3, 0],
    // };
    // let expected_example3_part1 = "4,6,3,5,6,3,5,2,1,0";
}

fn part2(machine: &Machine) -> u64 {
    (0..u64::MAX)
        .into_par_iter()
        .find_first(|initial_a| {
            let mut ip: usize = 0;
            let mut a: u64 = *initial_a;
            let mut b: u64 = machine.b;
            let mut c: u64 = machine.c;
            let mut output_length: usize = 0;
            loop {
                if ip >= machine.program.len() {
                    return output_length == machine.program.len();
                }
                let mut next_ip: usize = ip + 2;
                let operand: u8 = machine.program[ip + 1];
                match machine.program[ip] {
                    0 => a /= 2u64.pow(combo(a, b, c, operand) as u32),
                    1 => b ^= operand as u64,
                    2 => b = combo(a, b, c, operand) % 8,
                    3 => {
                        if a != 0 {
                            next_ip = operand as usize;
                        }
                    }
                    4 => b ^= c,
                    5 => {
                        if output_length == machine.program.len() {
                            return false;
                        }
                        let expected: u8 = machine.program[output_length];
                        let actual: u64 = combo(a, b, c, operand) % 8;
                        if expected as u64 != actual {
                            return false;
                        } else {
                            output_length += 1;
                        }
                    }
                    6 => b /= 2u64.pow(combo(a, b, c, operand) as u32),
                    7 => c /= 2u64.pow(combo(a, b, c, operand) as u32),
                    8_u8..=u8::MAX => panic!("Invalid opcode {}", machine.program[ip]),
                }
                ip = next_ip;
            }
        })
        .expect("No valid a found")
}

/*
        ( Just 6, Just operand ) ->
            runMachine2 (budget - 1) (ip + 2) a (a // pow a b c operand) c program expected

        ( Just 7, Just operand ) ->
            runMachine2 (budget - 1) (ip + 2) a b (a // pow a b c operand) program expected

        ( Just opcode, Just _ ) ->
            Debug.todo ("Missing opcode: " ++ String.fromInt opcode)

        ( _, Nothing ) ->
            Debug.todo "Reading off tape"
*/

fn combo(a: u64, b: u64, c: u64, operand: u8) -> u64 {
    match operand {
        0 => 0,
        1 => 1,
        2 => 2,
        3 => 3,
        4 => a,
        5 => b,
        6 => c,
        7 => panic!("Reserved opcode"),
        8_u8..=u8::MAX => panic!("Invalid opcode"),
    }
}
