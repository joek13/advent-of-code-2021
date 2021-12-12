import collections
import sys
import typing


def build_input_graph() -> typing.Dict[str, typing.List[str]]:
    # default adjacency list
    # if a we lookup a new node, just assume it has no neighbors yet
    graph = collections.defaultdict(list)

    for line in sys.stdin:
        # strip off trailing newline
        line = line.rstrip()

        # get source and destination
        src, dest = line.split("-")

        # add destination to adjacency list for source
        graph[src].append(dest)
        # graph is undirected, so we can move in either direction
        graph[dest].append(src)

    return graph


def is_small(name: str) -> bool:
    """Returns whether a cave is small based on its name

    Args:
        name (str): Cave name

    Returns:
        bool: Whether it is small
    """
    return name == name.lower()


# DFS
def num_paths(graph, start_node, visited=[]):
    total_paths = 0

    # if we start at the end node, only one path to get to end node
    if start_node == "end":
        return 1

    # basic idea:
    # number of paths from V to end is equal to
    # sum of the number of paths from each of V's neighbors to end
    for neighbor in graph[start_node]:
        if not is_small(neighbor):
            # large neighbor? always valid to continue
            total_paths += num_paths(graph, neighbor,
                                     visited=visited + [start_node])
        elif neighbor not in visited:
            # small neighbor? only valid if we have not yet visited
            total_paths += num_paths(graph, neighbor,
                                     visited=visited + [start_node])

    return total_paths


if __name__ == "__main__":
    graph = build_input_graph()

    paths = num_paths(graph, "start")
    print(paths)
