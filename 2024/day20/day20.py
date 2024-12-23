import sys

grid = [line.strip() for line in sys.stdin]

start_pos = next((x, y) for (y, line) in enumerate(grid) for (x, c) in enumerate(line) if c == "S")

def compute_timecode(grid, start_pos):
    timecodes = [[None for _ in range(len(grid[0]))] for _ in range(len(grid))]
    x, y = start_pos
    current_timecode = 0
    timecodes[y][x] = 0
    while True:
        timecodes[y][x] = current_timecode
        current_timecode += 1
        for dx, dy in ((-1, 0), (1, 0), (0, -1), (0, 1)):
            nx = x + dx
            ny = y + dy
            if grid[ny][nx] == "#":
                continue
            if timecodes[ny][nx] is not None:
                continue
            if grid[ny][nx] == "E":
                timecodes[ny][nx] = current_timecode
                return timecodes
            break
        else:
            assert False
        (y, x) = (ny, nx)

timecodes = compute_timecode(grid, start_pos)

def part1(grid, timecodes):
    cheats = {}
    for y, line in enumerate(grid):
        for x, c in enumerate(line):
            if c == "#":
                for dx, dy in ((-1, 0), (1, 0), (0, -1), (0, 1)):
                    px, py = x - dx, y - dy
                    nx, ny = x + dx, y + dy
                    if 0 <= nx < len(grid[0]) and 0 <= ny < len(grid) and 0 <= px < len(grid[0]) and 0 <= py < len(grid) and timecodes[py][px] is not None and timecodes[ny][nx] is not None:
                        if timecodes[py][px] < timecodes[ny][nx]:
                            cheat = timecodes[ny][nx] - timecodes[py][px] - 2
                            cheats[cheat] = cheats.get(cheat, 0) + 1
    return sum(count for cheat, count in cheats.items() if cheat >= 100)

print(f"Part 1: {part1(grid, timecodes)}")

def part2(grid, timecodes):
    cheats = {}
    max_cheat_duration = 20
    for y, line in enumerate(grid):
        for x, c in enumerate(line):
            source = timecodes[y][x]
            if source is not None:
                for cy in range(max(- y, - max_cheat_duration), min(len(grid) - y, max_cheat_duration + 1)):
                    for cx in range(max(- x, - max_cheat_duration + abs(cy)), min(len(grid[0]) - x, max_cheat_duration + 1 - abs(cy))):
                        assert 0 <= y + cy < len(grid)
                        assert 0 <= x + cx < len(grid[0])
                        assert 0 <= abs(cy) + abs(cx) <= max_cheat_duration
                        target = timecodes[y + cy][x + cx]
                        if target is not None and target > source + abs(cx) + abs(cy):
                            cheat = target - source - abs(cx) - abs(cy)
                            cheats[cheat] = cheats.get(cheat, 0) + 1
    return sum(count for cheat, count in cheats.items() if cheat >= 100)

print(f"Part 2: {part2(grid, timecodes)}")

