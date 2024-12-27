import sys

lines = [line.strip() for line in sys.stdin]

keys = []
locks = []

it = iter(lines)
has_contents = True
while has_contents:
    device = []
    while True:
        try:
            line = next(it)
        except StopIteration:
            has_contents = False
            break
        if line == "":
            break
        device.append(line)
    if device[0] == "#####" and device[-1] != "#####":
        pins = [next(l - 1 for l, line in enumerate(device) if line[i] == ".") for i in range(5)]
        locks.append(pins)
    elif device[0] != "#####" and device[-1] == "#####":
        pins = [next(l - 1 for l, line in enumerate(reversed(device)) if line[i] == ".") for i in range(5)]
        keys.append(pins)
    else:
        assert False

overlaps = 0
for key in keys:
    for lock in locks:
        if all(k + l <= 5 for k, l in zip(key, lock)):
            overlaps += 1

print(f"Part 1: {overlaps}")
