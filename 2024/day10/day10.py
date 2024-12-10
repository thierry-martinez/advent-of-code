import sys

lines = [line.strip() for line in sys.stdin]

def find_paths(initial, empty, add):
    positions = initial
    for height in range(1, 10):
        new_positions = empty()
        for position in positions:
            for (dx, dy) in ((-1, 0), (1, 0), (0, -1), (0, 1)):
                x, y = position
                x += dx
                y += dy
                if 0 <= x < len(lines[0]) and 0 <= y < len(lines):
                    if int(lines[y][x]) == height:
                        add(new_positions, (x, y))
        positions = new_positions
    return positions

print(f"Part 1: {sum(len(find_paths({(ox, oy)}, lambda: set(), lambda s, e: s.add(e))) for oy in range(len(lines)) for ox in range(len(lines[0])) if lines[oy][ox] == "0")}")

print(f"Part 2: {len(find_paths([(ox, oy) for oy in range(len(lines)) for ox in range(len(lines[0])) if lines[oy][ox] == "0"], lambda: [], lambda l, e: l.append(e)))}")
