fn reflection_count(
    smudge_count: usize, size0: usize, size1: usize,
    get: impl Fn(usize, usize) -> char
) -> usize {
    (1 .. size0).filter(|&line| {
        let mut allowed_defects = smudge_count;
        (0 .. line.min(size0 - line)).all(|i|
            (0 .. size1).all(|j|
                get(line - i - 1, j) == get(line + i, j) || (
                    allowed_defects > 0 && {
                        allowed_defects -= 1;
                        true
                    }
                )
            )
        ) && allowed_defects == 0
    }).sum()
}

fn evaluate_reflection(smudge_count: usize, pattern: &Vec<Vec<char>>) -> usize {
    let height = pattern.len();
    let width = pattern[0].len();
    let vertical_line_count =
        reflection_count(smudge_count, width, height, |i, j| pattern[j][i]);
    let horizontal_line_count =
        reflection_count(smudge_count, height, width, |i, j| pattern[i][j]);
    vertical_line_count + horizontal_line_count * 100
}

fn main() {
    let mut patterns = Vec::new();
    let mut lines = std::io::stdin().lines();
    loop {
        let pattern: Vec<Vec<char>> =
            lines.by_ref().take_while(|line| !line.as_ref().unwrap().is_empty()).
            map(|line| line.unwrap().chars().collect()).collect();
        if pattern.is_empty() {
            break;
        }
        patterns.push(pattern);
    }
    let result_part1: usize = 
        patterns.iter().map(|pattern| evaluate_reflection(0, pattern)).sum();
    println!("Part 1: {result_part1}");
    let result_part2: usize = 
        patterns.iter().map(|pattern| evaluate_reflection(1, pattern)).sum();
    println!("Part 2: {result_part2}");
}
