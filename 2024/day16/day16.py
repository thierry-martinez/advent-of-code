import sys

grid = [line.strip() for line in sys.stdin]

def find_exit(grid):
    sx, sy = next((sx, sy) for (sy, l) in enumerate(grid) for (sx, c) in enumerate(l) if c == "S")
    positions = {(sx, sy, 1, 0): (0, {(sx, sy)})}
    new_positions = positions
    best_found = None
    while new_positions:
        next_positions = []
        for (x, y, dx, dy), (score, tiles) in new_positions.items():
            nx = x + dx
            ny = y + dy
            if 0 <= ny < len(grid) and 0 <= nx < len(grid[0]):
                nscore = score + 1
                ntiles = tiles | {(nx, ny)}
                if grid[ny][nx] == "E":
                    if best_found is None:
                        best_found = nscore, ntiles
                    else:
                        best_score, best_tiles = best_found
                        if best_score > nscore:
                            best_found = nscore, ntiles
                        elif best_score == nscore:
                            best_found = nscore, best_tiles | ntiles
                if grid[ny][nx] == ".":
                    next_positions.append((nx, ny, dx, dy, nscore, ntiles))
            next_positions.append((x, y, - dy, dx, score + 1000, tiles))
            next_positions.append((x, y, dy, - dx, score + 1000, tiles))
        new_positions = {}
        for (x, y, dx, dy, score, tiles) in next_positions:
            if best_found is not None and best_found[0] < score:
                continue
            known = positions.get((x, y, dx, dy))
            if known is None:
                positions[(x, y, dx, dy)] = score, tiles
                new_positions[(x, y, dx, dy)] = score, tiles
            else:
                known_score, known_tiles = known
                if score < known_score:
                    positions[(x, y, dx, dy)] = score, tiles
                    new_positions[(x, y, dx, dy)] = score, tiles
                elif score == known_score:
                    positions[(x, y, dx, dy)] = score, tiles | known_tiles
                    new_positions[(x, y, dx, dy)] = score, tiles | known_tiles
    return best_found

score, tiles = find_exit(grid)
print(f"Part 1: {score}")
print(f"Part 2: {len(tiles)}")
