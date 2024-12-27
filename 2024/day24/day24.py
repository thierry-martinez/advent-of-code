from __future__ import annotations
import abc
from dataclasses import dataclass
import enum
import re
import sys
import typing

class Evaluable(abc.ABC):
    @abc.abstractmethod
    def eval(self, defs: dict[str, Evaluable]) -> bool:
        ...


@dataclass
class InitialValue:
    wire: str
    value: bool

    def eval(self, defs: dict[str, Evaluable]) -> bool:
        return self.value

    def get_xor_arguments(self, defs: dict[str, Evaluable]) -> set[str]:
        return {self.wire}

class Op(enum.Enum):
    AND = enum.auto()
    OR = enum.auto()
    XOR = enum.auto()

@dataclass
class Gate:
    in0: str
    op: Op
    in1: str
    out: str

    def eval(self, defs: dict[str, Evaluable]) -> bool:
        in0 = defs[self.in0].eval(defs)
        in1 = defs[self.in1].eval(defs)
        if self.op == Op.AND:
            return in0 and in1
        if self.op == Op.OR:
            return in0 or in1
        if self.op == Op.XOR:
            return in0 != in1
        typing.assert_never(self.op)

    def get_xor_arguments(self, defs: dict[str, Evaluable]) -> set[str]:
        if self.op != Op.XOR:
            return {self.out}
        return defs[self.in0].get_xor_arguments(defs) | defs[self.in1].get_xor_arguments(defs)

def parse():
    lines = [line.strip() for line in sys.stdin]
    empty_line_index = next(i for i, line in enumerate(lines) if line == "")
    defs = {}
    for line in lines[:empty_line_index]:
        wire, value = line.split(": ")
        defs[wire] = InitialValue(wire, value == "1")
    re_gate = re.compile("([^ ]+) (AND|OR|XOR) ([^ ]+) -> ([^ ]+)")
    for line in lines[empty_line_index + 1:]:
        in0, op, in1, out = re_gate.match(line).groups()
        defs[out] = Gate(in0, Op[op], in1, out)
    return defs

def part1(defs):
    z_wires = reversed(sorted([wire for wire in defs.keys() if wire[0] == "z"]))
    result = 0
    for wire in z_wires:
        result = result * 2 + defs[wire].eval(defs)
    return result

def part2(defs):
    nbits_x = len([wire for wire in defs.keys() if wire[0] == "x"])
    nbits_y = len([wire for wire in defs.keys() if wire[0] == "y"])
    nbits_z = len([wire for wire in defs.keys() if wire[0] == "z"])
    assert nbits_x == nbits_y and nbits_z == nbits_x + 1
    assert isinstance(defs["z00"], Gate)
    assert defs["z00"].in0 == "y00"
    assert defs["z00"].op == Op.XOR
    assert defs["z00"].in1 == "x00"
    r = next(wire for wire, d in defs.items() if isinstance(d, Gate) and d.op == Op.AND and {d.in0, d.in1} == {"x00", "y00"})
    swapped = []
    def swap(a, b):
        swapped.append(a)
        swapped.append(b)
        def_a = defs[a]
        def_b = defs[b]
        defs[b] = def_a
        defs[a] = def_b
    for i in range(1, nbits_z - 1):
        x = f"x{i:02}"
        y = f"y{i:02}"
        z = f"z{i:02}"
        s = next(wire for wire, d in defs.items() if isinstance(d, Gate) and d.op == Op.XOR and {d.in0, d.in1} == {x, y})
        try:
            t = next(wire for wire, d in defs.items() if isinstance(d, Gate) and d.op == Op.XOR and {d.in0, d.in1} == {s, r})
        except StopIteration:
            t, = [wire for wire, d in defs.items() if isinstance(d, Gate) and d.op == Op.XOR and r in {d.in0, d.in1}]
            s0, = {defs[t].in0, defs[t].in0} - {r}
            swap(s, s0)
            s = s0
        if t != z:
            swap(t, z)

        a = next(wire for wire, d in defs.items() if isinstance(d, Gate) and d.op == Op.AND and {d.in0, d.in1} == {x, y})
        b = next(wire for wire, d in defs.items() if isinstance(d, Gate) and d.op == Op.AND and {d.in0, d.in1} == {s, r})
        r = next(wire for wire, d in defs.items() if isinstance(d, Gate) and d.op == Op.OR and {d.in0, d.in1} == {a, b})
    return ",".join(sorted(swapped))

defs = parse()
print(f"Part 1: {part1(defs)}")

print(f"Part 2: {part2(defs)}")
