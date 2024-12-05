import sys

lines = list(sys.stdin)

blank_line = next(i for i, line in enumerate(lines) if line.strip() == "")

ordering = [list(map(int, line.split("|"))) for line in lines[:blank_line]]
updates = [list(map(int, line.split(","))) for line in lines[blank_line + 1:]]

def in_order(ordering, update):
    indexes = { v: i for i, v in enumerate(update) }
    for u, v in ordering:
        i = indexes.get(u)
        j = indexes.get(v)
        if i is not None and j is not None and i > j:
            return False
    return True

def middle_element(array):
    return array[len(array) >> 1]

print(f"Part 1: {sum([middle_element(update) for update in updates if in_order(ordering, update)])}")

def add_to_updated(precedences, updated, update, page):
    if page in updated:
        return
    for prec in precedences[page]:
        if prec in update:
            add_to_updated(precedences, updated, update, prec)
    updated.append(page)

def get_precedences(ordering):
    precedences = {}
    for u, v in ordering:
        precedences.setdefault(v, []).append(u)
    return precedences

def sort_order(precedences, update):
    updated = []
    for page in update:
        add_to_updated(precedences, updated, update, page)
    return updated

precedences = get_precedences(ordering)

print(f"Part 2: {sum([middle_element(sort_order(precedences, update)) for update in updates if not in_order(ordering, update)])}")
