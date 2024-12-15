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
q0 = 0
q1 = 0
q2 = 0
q3 = 0
mx = (width - 1) // 2
my = (height - 1) // 2
for px, py, vx, vy in robots:
    x = (px + seconds * vx) % width
    y = (py + seconds * vy) % height
    if x < mx:
        if y < my:
            q0 += 1
        elif y > my:
            q1 += 1
    elif x > mx:
        if y < my:
            q2 += 1
        elif y > my:
            q3 += 1
print(f"Part 1: {q0 * q1 * q2 * q3}")

positions = [(x, y) for x, y, _, _ in robots]
second = 0
ox = None
sx = None
oy = None
sy = None
while sx is None or sy is None:
    #print(second)
    #for y in range(height):
    #    line = ""
    #    for x in range(width):
    #        if (x, y) in positions:
    #            line += "*"
    #        else:
    #            line += " "
    #    print(line)
    vx = statistics.variance([x for x, y in positions])
    vy = statistics.variance([y for x, y in positions])
    if vx < 700:
        if sx is None:
            if ox is None:
                ox = second
            else:
                sx = second - ox
    if vy < 700:
        if sy is None:
            if oy is None:
                oy = second
            else:
                sy = second - oy
    for i in range(len(robots)):
        (x, y) = positions[i]
        (_, _, vx, vy) = robots[i]
        x = (x + vx) % width
        y = (y + vy) % height
        positions[i] = (x, y)
    second += 1

if oy > ox:
    step_x = (oy - ox) * pow(sx, sy - 2, mod=sy)
    base_time = ox + step_x * sx
else:
    step_y = (ox - oy) * pow(sy, sx - 2, mod=sx)
    base_time = oy + step_y * sy
time = base_time % (sx * sy)
print(f"Part 2: {time}")
