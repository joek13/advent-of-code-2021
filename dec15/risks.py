import sys
import dataclasses


def read_input():
    # generate grid of risk levels
    grid = []

    for line in sys.stdin:
        line = line.rstrip()  # strip trailing whitespace
        # each char is an integer entry in the row
        grid.append([int(c) for c in line])

    return grid


def get_neighbors(cell):
    i, j = cell
    return [(i+1, j), (i-1, j), (i, j+1), (i, j-1)]


@dataclasses.dataclass
class CellInfo:
    tentative_cost: int = -1
    known: bool = False


def cheapest_path(grid):
    nrows = len(grid)
    ncols = len(grid[0])

    # dijkstra's algorithm

    # maps vertex (i,j) -> (tentative_cost: int, known: bool)
    info = {}

    # initialize our info map
    for r in range(nrows):
        for c in range(ncols):
            info[(r, c)] = CellInfo()

    # we know the cost to get to (0,0) is its risk level
    info[(0, 0)] = CellInfo(tentative_cost=0)

    while unknown := [cell for (cell, cell_info) in info.items() if not cell_info.known]:
        # select lowest-cost unknown node
        current = min(
            # current node is lowest-cost unknown node whose cost is not -1
            # (which we can take to represent infinity)
            # filter out -1
            [cell for cell in unknown if info[cell].tentative_cost != -1],
            key=lambda cell: info[cell].tentative_cost  # min by cost
        )

        # get its tentative cost
        tentative_cost = info[current].tentative_cost

        # mark it as known
        info[current].known = True

        # update tentative cost of all neighbors
        for neighbor in get_neighbors(current):
            if neighbor in info:  # bounds check
                i, j = neighbor
                # cost of getting to neighbor via current
                prov_cost = tentative_cost + grid[i][j]

                # check if getting to neighbor via current is better
                # than our current known best way to neighbor
                if info[neighbor].tentative_cost == -1 or prov_cost < info[neighbor].tentative_cost:
                    info[neighbor].tentative_cost = prov_cost

    # min cost to get to bottom-right cell
    return info[(nrows - 1, ncols - 1)].tentative_cost


if __name__ == "__main__":
    grid = read_input()
    path_cost = cheapest_path(grid)
    print(path_cost)
