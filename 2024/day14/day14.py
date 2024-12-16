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

xs = [x for x, _, _, _ in robots]
min_var_x = statistics.variance(xs)
min_var_x_index = 0
for index in range(1, width):
    for i in range(len(robots)):
        (_, _, vx, _) = robots[i]
        xs[i] = (xs[i] + vx) % width
    var_x = statistics.variance(xs)
    if var_x < min_var_x:
        min_var_x = var_x
        min_var_x_index = index

ys = [y for _, y, _, _ in robots]
min_var_y = statistics.variance(ys)
min_var_y_index = 0
for index in range(1, height):
    for i in range(len(robots)):
        (_, _, _, vy) = robots[i]
        ys[i] = (ys[i] + vy) % height
    var_y = statistics.variance(ys)
    if var_y < min_var_y:
        min_var_y = var_y
        min_var_y_index = index

if min_var_y_index > min_var_x_index:
    step_x = (min_var_y_index - min_var_x_index) * pow(width, height - 2, mod=height)
    base_time = min_var_x_index + step_x * width
else:
    step_y = (min_var_x_index - min_var_y_index) * pow(height, width - 2, mod=width)
    base_time = min_var_y_index + step_y * height
time = base_time % (width * height)
print(f"Part 2: {time}")
