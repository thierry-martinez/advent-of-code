import itertools
import sys

safe_count = 0

def check_safe(levels):
    already_increasing = None
    for a, b in itertools.pairwise(levels):
        increasing = a < b

        if already_increasing == None:
            already_increasing = increasing
        elif increasing != already_increasing:
            return False

        if not (1 <= abs(a - b) <= 3):
            return False
    return True

for report in sys.stdin:
    levels = list(map(int, report.split()))
    if check_safe(levels):
        safe_count += 1
        continue
    for i in range(len(levels)):
        if check_safe(levels[0:i] + levels[(i + 1):]):
            safe_count += 1
            break

print(safe_count)
