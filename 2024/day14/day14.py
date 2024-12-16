import math
import re
import statistics
import sys

re_robot = re.compile(r"p=(\d+),(\d+) v=(-?\d+),(-?\d+)")
robots = []
for line in sys.stdin:
    px, py, vx, vy = re_robot.match(line).groups()
    robots.append((int(px), int(py), int(vx), int(vy)))

width = 101
height = 103

#width = 11
#height = 7

seconds = 100
quadrants = [[0 for _ in range(2)] for _ in range(2)]
mx = (width - 1) // 2
my = (height - 1) // 2
for px, py, vx, vy in robots:
    x = (px + seconds * vx) % width
    y = (py + seconds * vy) % height
    if x != mx and y != my:
        quadrants[int(y < my)][int(x < mx)] += 1
part1 = math.prod(n for q in quadrants for n in q)
print(f"Part 1: {part1}")

def find_minimum_variance_index(positions, velocities, length):
    def evolve():
        for _ in range(length):
            yield statistics.variance(positions)
            for i in range(len(positions)):
                positions[i] = (positions[i] + velocities[i]) % length
    return min(enumerate(evolve()), key=lambda p: p[1])[0]

# Looking for the frame that minimizes the variance.

# First, we look on each component.
# (Note that the x-component is width-periodic and the y-component is height-periodic.)
xmin = find_minimum_variance_index([px for px, py, vx, vy in robots], [vx for px, py, vx, vy in robots], width)
ymin = find_minimum_variance_index([py for px, py, vx, vy in robots], [vy for px, py, vx, vy in robots], height)

# The index [T] of the frame that minimizes the variance is such that
# T = xmin + p * width = ymin + q * height
# for p and q some integers,
# and we look for the smallest such T, that is to say 0 <= T < width * height
# (because the whole animation is (width * height)-periodic).
# We will find a first solution by asserting q = 0 (for instance),
# and then we will slide this solution to fit inside the window 0 <= T < width * height.
# If q = 0, then we have xmin + p * width = ymin
# p * width = ymin - xmin
# We now need an inverse for width. Note that we only need an equation on p modulo height
# because p * width will be modulo width * height. By Fermat's little theorem,
# width' = width ^ (height - 2) mod height is an inverse of width modulo height.
# Therefore,

p = (ymin - xmin) * pow(width, height - 2, mod=height)
t = xmin + p * width

t = t % (width * height)
print(f"Part 2: {t}")
