import sys

values = list(map(int, sys.stdin.read().split()))

def blink(stones):
    new_stones = []
    for stone in stones:
        if stone == 0:
            new_stones.append(1)
        else:
            s = str(stone)
            if len(s) % 2 == 0:
                new_stones.append(int(s[0:len(s) // 2]))
                new_stones.append(int(s[len(s) // 2:]))
            else:
                new_stones.append(stone * 2024)
    return new_stones

stones = values
for _ in range(25):
    stones = blink(stones)
print(f"Part 1: {len(stones)}")

def add_stone(stones, number, count):
    stones[number] = stones.get(number, 0) + count

def faster_blink(stones):
    new_stones = {}
    for stone, count in stones.items():
        if stone == 0:
            add_stone(new_stones, 1, count)
        else:
            s = str(stone)
            if len(s) % 2 == 0:
                add_stone(new_stones, int(s[0:len(s) // 2]), count)
                add_stone(new_stones, int(s[len(s) // 2:]), count)
            else:
                add_stone(new_stones, stone * 2024, count)
    return new_stones

stones = {}
for stone in values:
    add_stone(stones, stone, 1)
for _ in range(75):
    stones = faster_blink(stones)

print(f"Part 2: {sum(stones.values())}")
