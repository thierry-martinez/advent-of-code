import re
import sys

re_button_a = re.compile(r"Button A: X\+(\d+), Y\+(\d+)")
re_button_b = re.compile(r"Button B: X\+(\d+), Y\+(\d+)")
re_prize = re.compile(r"Prize: X=(\d+), Y=(\d+)")

machines = []
lines = iter(sys.stdin)
try:
    while True:
        ax, ay = re_button_a.match(next(lines)).groups()
        bx, by = re_button_b.match(next(lines)).groups()
        px, py = re_prize.match(next(lines)).groups()
        machines.append((int(ax), int(ay), int(bx), int(by), int(px), int(py)))
        assert(next(lines).strip() == "")
except StopIteration:
    pass

def count_tokens(machines, offset):
    tokens = 0
    for ax, ay, bx, by, px, py in machines:
        px += offset
        py += offset
        b, rb = divmod(py * ax - px * ay, by * ax - bx * ay)
        if b < 0 or rb != 0:
            continue
        a, ra = divmod(px - b * bx, ax)
        if a < 0 or ra != 0:
            continue
        tokens += 3 * a + b
    return tokens

tokens = count_tokens(machines, 0)
print(f"Part 1: {tokens}")

tokens = count_tokens(machines, 10000000000000)
print(f"Part 1: {tokens}")
