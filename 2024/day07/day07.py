import sys

lines = list(sys.stdin)

equations = []
for line in lines:
    test_value, tail = line.split(":")
    numbers = tail.split()
    equations.append((int(test_value), list(map(int, numbers))))

def could_be_true(equation, ops):
    test_value, numbers = equation
    first, *tail = numbers
    possible_values = { first }
    for value in tail:
        possible_values = { op(possible, value) for possible in possible_values for op in ops }
    return test_value in possible_values

print(f"Part 1: {sum(equation[0] for equation in equations if could_be_true(equation, [lambda x, y: x + y, lambda x, y: x * y ]))}")

#print(f"Part 2: {sum(equation[0] for equation in equations if could_be_true(equation, [lambda x, y: x + y, lambda x, y: x * y, lambda x, y: int(str(x) + str(y))]))}")

def could_be_true_faster(equation, dual_ops):
    test_value, numbers = equation
    first, *tail = numbers
    possible_values = { test_value }
    for value in reversed(tail):
        new_possible_values = set()
        for possible in possible_values:
            for op in dual_ops:
                if result := op(possible, value):
                    new_possible_values.add(result)
        possible_values = new_possible_values
    return first in possible_values

def deconcat(x, y):
    sx = str(x)
    sy = str(y)
    if len(sx) > len(sy) and sx.endswith(sy):
        return int(sx.removesuffix(sy))

print(f"Part 2: {sum(equation[0] for equation in equations if could_be_true_faster(equation, [lambda x, y: x - y if x >= y else None, lambda x, y: x // y if x % y == 0 else None, deconcat]))}")
