import sys

grid = list(sys.stdin)

initial_position = next((x, y) for y, line in enumerate(grid) for x, symbol in enumerate(line) if symbol == "^")

dx, dy = (0, -1)

position = initial_position
visited = { position }

def inside(grid, x, y):
    return 0 <= x < len(grid[0]) and 0 <= y < len(grid)

while True:
    x, y = position
    nx, ny = x + dx, y + dy
    if not inside(grid, nx, ny):
        break
    if grid[ny][nx] == "#":
        dx, dy = - dy, dx
        continue
    position = nx, ny
    visited.add(position)

print(f"Part 1: {len(visited)}")

grid = list(map(list, grid))

def test_obstruction(grid, ox, oy):
    grid[oy][ox] = "#"
    position = initial_position
    direction = (0, -1)
    visited_dir = { (position, direction) }
    while True:
        x, y = position
        dx, dy = direction
        nx, ny = x + dx, y + dy
        if not inside(grid, nx, ny):
            in_loop = False
            break
        if grid[ny][nx] == "#":
            direction = - dy, dx
            continue
        position = nx, ny
        if (position, direction) in visited_dir:
            in_loop = True
            break
        visited_dir.add((position, direction))
    grid[oy][ox] = "."
    return in_loop

#print(f"Part 2: {sum(1 for ox, oy in visited if test_obstruction(grid, ox, oy) )}")

def add_obstacle_to_compressed_grid(grid, compressed, ox, oy):
    undo = []
    for dy in range(-1, 2):
        for dx in range(-1, 2):
            if dx != dy:
                x, y = ox, oy
                while True:
                    x, y = x - dx, y - dy
                    if not inside(grid, x, y) or grid[y][x] == "#":
                        break
                    undo.append((x, y, dx, dy, compressed[y][x].get((dx, dy))))
                    compressed[y][x][(dx, dy)] = (ox, oy)
    return undo

def compress_grid(grid):
    compressed = [[dict() for _x in range(len(grid[0]))] for _y in range(len(grid))]
    for y in range(len(grid)):
        for x in range(len(grid[0])):
            if grid[y][x] == "#":
                add_obstacle_to_compressed_grid(grid, compressed, x, y)
    return compressed

def fast_test_obstruction(grid, compressed, ox, oy):
    undo = add_obstacle_to_compressed_grid(grid, compressed, ox, oy)
    position = initial_position
    direction = (0, -1)
    visited_dir = { (position, direction) }
    while True:
        x, y = position
        dx, dy = direction
        obstacle = compressed[y][x].get((dx, dy))
        if obstacle is None:
            in_loop = False
            break
        ox, oy = obstacle
        position = ox - dx, oy - dy
        direction = - dy, dx
        if (position, direction) in visited_dir:
            in_loop = True
            break
        visited_dir.add((position, direction))
    for (x, y, dx, dy, value) in undo:
        compressed[y][x][(dx, dy)] = value
    return in_loop

compressed = compress_grid(grid)

print(f"Part 2: {sum(1 for ox, oy in visited if fast_test_obstruction(grid, compressed, ox, oy) )}")
