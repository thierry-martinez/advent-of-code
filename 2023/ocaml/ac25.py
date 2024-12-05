import collections
import itertools
import sys
import networkx as nx
import networkx.algorithms.flow
import time

def parse_graph(input):
    G = nx.Graph()
    table = collections.defaultdict(itertools.count().__next__)
    for line in input:
        lhs, rhs = line.strip().split(": ", 1)
        rhs_list = rhs.split(" ")
        G.add_edges_from([(lhs, rhs) for rhs in rhs_list], capacity=1)
    return G

G = parse_graph(sys.stdin)
node = min(G, key=G.degree)
print(G.edges(node))
tic = time.perf_counter()
cut = nx.minimum_edge_cut(G)
toc = time.perf_counter()
print(f"{toc - tic:0.4f} seconds")
print(cut)
tic = time.perf_counter()
cut = nx.minimum_edge_cut(G, flow_func= networkx.algorithms.flow.edmonds_karp)
toc = time.perf_counter()
print(f"{toc - tic:0.4f} seconds")
print(cut)
tic = time.perf_counter()
def edmonds_karp(*args, **kwargs):
    return networkx.algorithms.flow.edmonds_karp(*args, **kwargs, cutoff=4)
cut = nx.minimum_edge_cut(G, flow_func=edmonds_karp)
toc = time.perf_counter()
print(f"{toc - tic:0.4f} seconds")
print(cut)
tic = time.perf_counter()
cut = nx.stoer_wagner(G)
toc = time.perf_counter()
print(f"{toc - tic:0.4f} seconds")
