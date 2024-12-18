fn main() {
    println!(
        "{}",
        part2(vec![9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9])
    );
}

fn part2(program: Vec<u8>) -> usize {
    let attempt: usize = program
        .iter()
        .rfold(0, |acc, x| (acc * 8) + (*x as usize) ^ 3);
    if check_machine(attempt, &program) && false {
        return attempt;
    } else {
        println!(
            "I'm missing something {} {:o} {:?}",
            attempt, attempt, program
        );
        return attempt;
    }
}

fn check_machine(initial_a: usize, program: &Vec<u8>) -> bool {
    let mut ip: usize = 0;
    let mut a: u64 = initial_a as u64;
    let mut b: u64 = 0;
    let mut c: u64 = 0;
    let mut output_length: usize = 0;
    loop {
        if ip >= program.len() {
            println!();
            return output_length == program.len();
        }
        let mut next_ip: usize = ip + 2;
        let operand: u8 = program[ip + 1];
        match program[ip] {
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
                let actual: u64 = combo(a, b, c, operand) % 8;
                print!("{},", actual);
                if output_length == program.len() {
                    return false;
                }
                let expected: u8 = program[output_length];
                if expected as u64 != actual && false {
                    return false;
                } else {
                    output_length += 1;
                }
            }
            6 => b /= 2u64.pow(combo(a, b, c, operand) as u32),
            7 => c /= 2u64.pow(combo(a, b, c, operand) as u32),
            8_u8..=u8::MAX => panic!("Invalid opcode {}", program[ip]),
        }
        ip = next_ip;
    }
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
