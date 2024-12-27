import sys

codes = [line.strip() for line in sys.stdin]

digit_keyboard = [
    "789",
    "456",
    "123",
    " 0A",
]

arrow_keyboard = [
    " ^A",
    "<v>"
]

def all_paths(x, y, nx, ny, keyboard):
    if x == nx and y == ny:
        return ["A"]
    if x > nx:
        x_sequence = "<" * (x - nx)
    elif x < nx:
        x_sequence = ">" * (nx - x)
    if y > ny:
        y_sequence = "^" * (y - ny)
    elif y < ny:
        y_sequence = "v" * (ny - y)
    if x == nx:
        return [y_sequence + "A"]
    if y == ny:
        return [x_sequence + "A"]
    if keyboard[ny][x] == " ":
        return [x_sequence + y_sequence + "A"]
    if keyboard[y][nx] == " ":
        return [y_sequence + x_sequence + "A"]
    return [x_sequence + y_sequence + "A", y_sequence + x_sequence + "A"]

def find_char(keyboard, char):
    return next((x, y) for y, line in enumerate(keyboard) for x, c in enumerate(line) if c == char)

def find_shortest_arrows(memo_table, path, level_count):
    if level_count == 0:
        return len(path)
    x, y = find_char(arrow_keyboard, "A")
    sum_length = 0
    for char in path:
        try:
            nx, ny, min_length = memo_table[(x, y, char, level_count)]
        except KeyError:
            nx, ny = find_char(arrow_keyboard, char)
            paths = all_paths(x, y, nx, ny, arrow_keyboard)
            min_length = min(find_shortest_arrows(memo_table, path, level_count - 1) for path in paths)
            memo_table[(x, y, char, level_count)] = (nx, ny, min_length)
        x, y = nx, ny
        sum_length += min_length
    return sum_length

def find_shortest_digits(memo_table, code, level_count):
    x, y = find_char(digit_keyboard, "A")
    sum_length = 0
    for char in code:
        nx, ny = find_char(digit_keyboard, char)
        paths = all_paths(x, y, nx, ny, digit_keyboard)
        sum_length += min(find_shortest_arrows(memo_table, path, level_count) for path in paths)
        x, y = nx, ny
    return sum_length

def solve(memo_table, codes, level_count):
    s = 0
    for code in codes:
        length = find_shortest_digits(memo_table, code, level_count)
        numeric_part = int(code.replace("A", ""))
        s += length * numeric_part
    return s

memo_table = {}

assert find_shortest_digits(memo_table, "029A", 0) == len("<A^A>^^AvvvA")
assert find_shortest_digits(memo_table, "029A", 1) == len("v<<A>>^A<A>AvA<^AA>A<vAAA>^A")

print(f"Part 1: {solve(memo_table, codes, 2)}")
print(f"Part 2: {solve(memo_table, codes, 25)}")
