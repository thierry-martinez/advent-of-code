from pathlib import Path
import re
import sys
with Path("input").open() as f:
    data = str(f.read())

pattern = re.compile(r"mul\((\d{1,3}),(\d{1,3})\)")
result = sum(int(a) * int(b) for a, b in pattern.findall(data))
print(f"Part 1: {result}")

# (?s:.): dot matches all
# (?:...): non-capturing parentheses
pattern = re.compile(r"don't\(\)(?s:.)*?(?:do\(\)|$)|mul\((\d{1,3}),(\d{1,3})\)")
result = sum(int(a) * int(b) for a, b in pattern.findall(data) if a)
print(f"Part 2: {result}")
