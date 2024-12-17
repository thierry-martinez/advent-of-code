import re
import sys

lines = iter(sys.stdin)
ra = int(re.match("Register A: (\d+)", next(lines)).groups(1)[0])
rb = int(re.match("Register B: (\d+)", next(lines)).groups(1)[0])
rc = int(re.match("Register C: (\d+)", next(lines)).groups(1)[0])
assert(next(lines).strip() == "")
program = list(map(int, re.match("Program: (.*)$", next(lines)).groups(1)[0].split(",")))

def eval_combo_operand(ra, rb, rc, operand):
    if operand <= 3:
        return operand
    if operand == 4:
        return ra
    if operand == 5:
        return rb
    if operand == 6:
        return rc
    assert False

def interpret(ra, rb, rc, program):
    pc = 0
    output = []
    while pc < len(program):
        instr = program[pc]
        operand = program[pc + 1]
        if instr == 0:
            ra = ra // 2 ** eval_combo_operand(ra, rb, rc, operand)
        elif instr == 1:
            rb = rb ^ operand
        elif instr == 2:
            rb = eval_combo_operand(ra, rb, rc, operand) % 8
        elif instr == 3:
            if ra != 0:
                pc = operand
                continue
        elif instr == 4:
            rb = rb ^ rc
        elif instr == 5:
            output.append(eval_combo_operand(ra, rb, rc, operand) % 8)
        elif instr == 6:
            rb = ra // 2 ** eval_combo_operand(ra, rb, rc, operand)
        elif instr == 7:
            rc = ra // 2 ** eval_combo_operand(ra, rb, rc, operand)
        pc += 2
    return output

output = interpret(ra, rb, rc, program)

part1 = ",".join(map(str, output))
print(f"Part 1: {part1}")

def search(ra, i):
    if i >= len(program):
        return ra
    for v in range(0, 8):
        tested = 8 * ra + v
        if interpret(tested, rb, rc, program) == program[- i - 1:]:
            result = search(tested, i + 1)
            if result is not None:
                return result
    return None

print(f"Part 2: {search(0, 0)}")
