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


def num_paths(graph, start_node, visited=[], have_revisited_small_cave=False):
    total_paths = 0

    # if we start at the end node, only one path to get to end node
    if start_node == "end":
        return 1

    # basic idea:
    # number of paths from V to end is equal to
    # sum of the number of paths from each of V's neighbors to end
    for neighbor in graph[start_node]:
        # never revisit the start node
        if neighbor == "start":
            continue

        if not is_small(neighbor):
            # large neighbor? always valid to continue
            total_paths += num_paths(graph, neighbor,
                                     visited=visited + [start_node],
                                     have_revisited_small_cave=have_revisited_small_cave)
        else:
            # small neighbor
            # check whether we have already visited a small cave twice on this path
            if neighbor not in visited:
                # we have not yet visited the cave
                total_paths += num_paths(graph, neighbor,
                                         visited=visited + [start_node],
                                         have_revisited_small_cave=have_revisited_small_cave)
            elif not have_revisited_small_cave:
                # we have already visited this small cave,
                # but we can visit this cave twice
                total_paths += num_paths(graph, neighbor, visited=visited +
                                         [start_node], have_revisited_small_cave=True)
            # else, we have already visited the cave and cannot revisit a small cave
            # so this is a dead end

    return total_paths


if __name__ == "__main__":
    graph = build_input_graph()

    paths = num_paths(graph, "start")
    print(paths)
