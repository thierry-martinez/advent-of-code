import sys

links = [line.strip().split("-") for line in sys.stdin]

link_map = {}
for a, b in links:
    link_map.setdefault(a, set()).add(b)
    link_map.setdefault(b, set()).add(a)

three_sets = {frozenset([a, b, c]) for a, b in links for c in link_map[a] & link_map[b] if any(s.startswith("t") for s in (a, b, c))}

part1 = len(three_sets)
print(f"Part 1: {part1}")

cliques = three_sets
while True:
    new_cliques = set()
    for s in cliques:
        neighbors, *others = [link_map[y] for y in s]
        for x in neighbors.intersection(*others):
            new_cliques.add(s | {x})
    if not new_cliques:
        break
    cliques = new_cliques

max_clique, = cliques
part2 = ",".join(sorted(max_clique))
print(f"Part 2: {part2}")
