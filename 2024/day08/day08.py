import itertools
import sys

grid = [line.strip() for line in sys.stdin]

antennas = {}
for y, line in enumerate(grid):
    for x, char in enumerate(line):
        if char != ".":
            antennas.setdefault(char, []).append((x, y))

antinodes = set()
for l in antennas.values():
    for u, v in itertools.permutations(l, 2):
        ux, uy = u
        vx, vy = v
        x = 2 * ux - vx
        y = 2 * uy - vy
        if 0 <= x < len(grid[0]) and 0 <= y < len(grid):
            antinodes.add((x, y))

print(f"Part 1: {len(antinodes)}")

antinodes = set()
for l in antennas.values():
    for u, v in itertools.permutations(l, 2):
        ux, uy = u
        vx, vy = v
        dx = ux - vx
        dy = uy - vy
        x, y = ux, uy
        while True:
            x = x - dx
            y = y - dy
            if not (0 <= x < len(grid[0]) and 0 <= y < len(grid)):
                break
            antinodes.add((x, y))

print(f"Part 2: {len(antinodes)}")
