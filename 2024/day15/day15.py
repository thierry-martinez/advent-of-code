import sys

lines = [line.strip() for line in sys.stdin]
sep = next(i for i, l in enumerate(lines) if l == "")
grid = list(map(list, lines[:sep]))
sequence = "".join(lines[sep + 1:])

def move(grid, px, py, dx, dy):
    assert grid[py][px] == "@"
    nx = px + dx
    ny = py + dy
    if grid[ny][nx] == "#":
        return (px, py)
    elif grid[ny][nx] != ".":
        ox = nx
        oy = ny
        while True:
            assert grid[oy][ox] == "O"
            ox = ox + dx
            oy = oy + dy
            if grid[oy][ox] == "#":
                return (px, py)
            elif grid[oy][ox] == ".":
                break
        grid[oy][ox] = "O"
    grid[ny][nx] = "@"
    grid[py][px] = "."
    return (nx, ny)

def follow_sequence(grid, sequence, move):
    px, py = next((px, py) for py, l in enumerate(grid) for px, c in enumerate(l) if c == "@")
    directions = {"<": (-1, 0), "^": (0, -1), ">": (1, 0), "v": (0, 1)}
    for cmd in sequence:
        dx, dy = directions[cmd]
        px, py = move(grid, px, py, dx, dy)

def double_symbol(c):
    if c == "#":
        return "##"
    if c == "O":
        return "[]"
    if c == ".":
        return ".."
    if c == "@":
        return "@."

double_grid = [[dc for c in l for dc in double_symbol(c)] for l in grid]

follow_sequence(grid, sequence, move)
coordinates = sum(100 * y + x for y, l in enumerate(grid) for x, c in enumerate(l) if c == "O")
print(f"Part 1: {coordinates}")

def add_to_frontier(grid, frontier, x, y):
    if grid[y][x] == "[":
        frontier |= {x, x + 1}
    elif grid[y][x] == "]":
        frontier |= {x, x - 1}
    else:
        assert False

def double_move(grid, px, py, dx, dy):
    assert grid[py][px] == "@"
    nx = px + dx
    ny = py + dy
    if grid[ny][nx] == "#":
        return (px, py)
    elif grid[ny][nx] != ".":
        if dy == 0:
            ox = nx
            oy = ny
            while True:
                assert grid[oy][ox] in "[]"
                ox = ox + dx
                oy = oy + dy
                if grid[oy][ox] == "#":
                    return (px, py)
                elif grid[oy][ox] == ".":
                    break
            for x in range(ox, px, - dx):
                grid[oy][x] = grid[oy][x - dx]
        else:
            shape = []
            frontier = set()
            add_to_frontier(grid, frontier, nx, ny)
            by = ny
            while frontier:
                shape.append(frontier)
                by += dy
                new_frontier = set()
                for x in frontier:
                    if grid[by][x] == "#":
                        return (px, py)
                    elif grid[by][x] != ".":
                        add_to_frontier(grid, new_frontier, x, by)
                frontier = new_frontier
            last_frontier = set()
            for frontier in reversed(shape):
                for x in frontier:
                    grid[by][x] = grid[by - dy][x]
                for x in last_frontier - frontier:
                    grid[by][x] = "."
                last_frontier = frontier
                by -= dy
            for x in frontier:
                grid[by][x] = "."
    grid[ny][nx] = "@"
    grid[py][px] = "."
    return (nx, ny)

follow_sequence(double_grid, sequence, double_move)
coordinates = sum(100 * y + x for y, l in enumerate(double_grid) for x, c in enumerate(l) if c == "[")

print(f"Part 2: {coordinates}")

