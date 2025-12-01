fn main() {
    let input: String =
        std::fs::read_to_string("inputs/day1.txt").expect("Failed to read the input file");
    let lines = input.split("\n").filter(|l| !l.is_empty());
    let mut pointing_at = 50i32;
    let mut zeroes = 0u32;
    for line in lines {
        let direction: Direction = if line.starts_with("L") {
            Direction::Left
        } else if line.starts_with("R") {
            Direction::Right
        } else {
            panic!("Invalid line \"{}\" - doesn't start with L or R", line);
        };
        let number = match line[1..].parse::<i32>() {
            Ok(number) => number,
            Err(e) => panic!("Invalid line \"{}\" - couldn't parse number: {}", line, e),
        };
        pointing_at = match direction {
            Direction::Left => (pointing_at - number) % 100,
            Direction::Right => (pointing_at + number) % 100,
        };
        if pointing_at == 0 {
            zeroes += 1;
        }
    }
    println!("Zeroes: {}", zeroes)
}

enum Direction {
    Left,
    Right,
}
