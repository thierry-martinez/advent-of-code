import sys

lines = iter(sys.stdin)

patterns = next(lines).strip().split(", ")
assert next(lines).strip() == ""
designs = [line.strip() for line in lines]

def count_possible(design, patterns):
    counts = [0 for _ in range(len(design) + 1)]
    counts[0] = 1
    for i in range(1, len(design) + 1):
        s = 0
        for pattern in patterns:
            if i >= len(pattern) and design[i - len(pattern):i] == pattern:
                s += counts[i - len(pattern)]
        counts[i] = s
    return counts[len(design)]

part1 = sum(1 for design in designs if count_possible(design, patterns) > 0)
print(f"Part 1: {part1}")

part2 = sum(count_possible(design, patterns) for design in designs)
print(f"Part 2: {part2}")

