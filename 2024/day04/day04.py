import sys

lines = list(sys.stdin)

def count_xmas_at_point(grid, y, x) -> int:
    if grid[y][x] != "X":
        return 0
    count = 0
    for dy in range(-1, 2):
        for dx in range(-1, 2):
            if (dx, dy) != (0, 0):
                if 0 <= x + 3 * dx < len(grid[0]):
                    if 0 <= y + 3 * dy < len(grid):
                        if ("".join(grid[y + i * dy][x + i * dx] for i in range(4))) == "XMAS":
                            count += 1
    return count

def x_mas_at_point(grid, y, x) -> bool:
    if grid[y][x] != "A" or y < 1 or x < 1 or y >= len(grid) - 1 or x >= len(grid) - 1:
        return False
    if "".join(grid[y + i][x + i] for i in range(-1, 2)) not in { "MAS", "SAM" }:
        return False
    if "".join(grid[y - i][x + i] for i in range(-1, 2)) not in { "MAS", "SAM" }:
        return False
    return True

print(f"Part 1: {sum(count_xmas_at_point(lines, i, j) for i in range(len(lines)) for j in range(len(lines[0])))}")

print(f"Part 2: {sum(1 for i in range(len(lines)) for j in range(len(lines[0])) if x_mas_at_point(lines, i, j))}")
