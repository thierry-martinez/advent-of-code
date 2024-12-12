import sys

grid = [line.strip() for line in sys.stdin]

NEIGHBORS = ((-1, 0), (1, 0), (0, -1), (0, 1))

def inside(grid, x, y):
    return 0 <= x < len(grid[0]) and 0 <= y < len(grid)

def fill_region(grid, x, y):
    name = grid[y][x]
    points = []
    region = []
    new_points = [(x, y)]
    region_grid[y][x] = region
    while len(new_points) > 0:
        region.extend(new_points)
        next_points = []
        for (x, y) in new_points:
            for (dx, dy) in NEIGHBORS:
                nx = x + dx
                ny = y + dy
                if inside(grid, nx, ny) and grid[ny][nx] == name and region_grid[ny][nx] == None:
                    region_grid[ny][nx] = region
                    next_points.append((nx, ny))
        new_points = next_points
    return region

regions = []
region_grid = [[None for _ in range(len(grid[0]))] for _ in range(len(grid))]
for y in range(len(grid)):
    for x in range(len(grid[0])):
        if region_grid[y][x] == None:
            regions.append(fill_region(grid, x, y))

s = 0
for region in regions:
    area = len(region)
    perimeter = 0
    for (x, y) in region:
        plots = 4
        for (dx, dy) in NEIGHBORS:
            nx = x + dx
            ny = y + dy
            if (nx, ny) in region:
                plots -= 1
        perimeter += plots
    s += area * perimeter

print(f"Part 1: {s}")

s = 0
for region in regions:
    area = len(region)
    perimeter = 0
    fences_grid = [[{ direction: False for direction in NEIGHBORS } for _ in range(len(grid[0]))] for _ in range(len(grid))]
    for (x, y) in region:
        plant_fence_count = 0
        plant_fences = fences_grid[y][x]
        for (dx, dy) in NEIGHBORS:
            if (x + dx, y + dy) not in region and not plant_fences[(dx, dy)]:
                plant_fence_count += 1
                plant_fences[(dx, dy)] = True
                fx = x - dy
                fy = y - dx
                while (fx, fy) in region and (fx + dx, fy + dy) not in region:
                    fences_grid[fy][fx][(dx, dy)] = True
                    fx = fx - dy
                    fy = fy - dx
                fx = x + dy
                fy = y + dx
                while (fx, fy) in region and (fx + dx, fy + dy) not in region:
                    fences_grid[fy][fx][(dx, dy)] = True
                    fx = fx + dy
                    fy = fy + dx
        perimeter += plant_fence_count
    s += area * perimeter
    
print(f"Part 2: {s}")
