import sys

coords = [map(int, line.strip().split(",")) for line in sys.stdin]

size = 71
#size = 7

grid = [["." for _ in range(size)] for _ in range(size)]

corrupted = 1024
#corrupted = 12
for x, y in coords[:corrupted]:
    grid[y][x] = "#"

def minimum_steps(grid):
    reachable = {(0, 0)}
    new_reachable = reachable
    steps = 0
    while new_reachable:
        next_reachable = set()
        steps += 1
        for px, py in new_reachable:
            for dx, dy in ((-1, 0), (1, 0), (0, -1), (0, 1)):
                nx = px + dx
                ny = py + dy
                if not (0 <= nx < size and 0 <= ny < size):
                    continue
                if grid[ny][nx] == "#":
                    continue
                if (nx, ny) in reachable:
                    continue
                if nx == size - 1 and ny == size - 1:
                    return steps
                next_reachable.add((nx, ny))
        new_reachable = next_reachable
        reachable |= new_reachable

print(f"Part 1: {minimum_steps(grid)}")

def first_cutoff(grid, coords):
    for x, y in coords:
        grid[y][x] = "#"
        if minimum_steps(grid) is None:
            return(x, y)

cx, cy = first_cutoff(grid, coords[corrupted:])
print(f"Part 2: {cx},{cy}")
