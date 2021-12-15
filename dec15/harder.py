import sys
import dataclasses
import heapq
import typing


def read_input():
    # generate grid of risk levels
    grid = []

    for line in sys.stdin:
        line = line.rstrip()  # strip trailing whitespace
        # each char is an integer entry in the row
        grid.append([int(c) for c in line])

    return grid


def get_neighbors(cell, nrows, ncols):
    i, j = cell
    neighbors = [(i+1, j), (i-1, j), (i, j+1), (i, j-1)]
    return [(r, c) for (r, c) in neighbors if 0 <= r < nrows and 0 <= c < ncols]


@dataclasses.dataclass(order=True)
class CellInfo:
    # estimated cost from start to goal if we go thru this node
    tentative_cost: int
    cell: typing.Tuple[int, int] = dataclasses.field(compare=False)


def cheapest_path(grid):
    nrows = len(grid)
    ncols = len(grid[0])

    # cell we are trying to reach
    goal = (nrows - 1, ncols - 1)

    # a* search algorithm

    # map from vertex (i, j) -> lowest risk path from (0,0) to (i,j)
    cost_to = {
        (0, 0): 0
    }

    def heuristic(cell):
        # this is an admissible heuristic, since it never overestimates the cost to
        # reach the goal from a given cell.
        i, j = cell
        # how much would the path cost if all the cells were 1?
        return goal[0] - i + goal[1] - j

    # heap of open search
    open_set = [
        # cost of zero for (0,0)
        CellInfo(heuristic((0, 0)), (0, 0))
    ]

    # hashset of cells in the open set
    # so we can efficiently test for membership
    cells_in_open_set = set(
        [(0, 0)]
    )

    while len(open_set) > 0:
        current = heapq.heappop(open_set)
        # remove reference to this cell in open set
        cells_in_open_set.remove(current.cell)

        # have found the cheapest path to the goal
        if current.cell == goal:
            return cost_to[current.cell]

        for neighbor in get_neighbors(current.cell, nrows, ncols):
            i, j = neighbor
            # cost to get to current cell + cost to get from current cell to neighbor
            tentative_cost = cost_to[current.cell] + grid[i][j]
            if neighbor not in cost_to or tentative_cost < cost_to[neighbor]:
                cost_to[neighbor] = tentative_cost

                estimated_cost_thru_neighbor = tentative_cost + \
                    heuristic(neighbor)

                if neighbor not in cells_in_open_set:
                    heapq.heappush(open_set,
                                   CellInfo(estimated_cost_thru_neighbor, neighbor))
                    cells_in_open_set.add(neighbor)
                else:
                    # find and update this cell in the heap
                    # need to call heapify to maintain heap property
                    # this is not super efficient, but should be relatively rare

                    # search for the cell info
                    for cell_info in open_set:
                        if cell_info.cell == neighbor:
                            # update its value
                            cell_info.tentative_cost = estimated_cost_thru_neighbor
                            break

                    # call heapify to restore heap property
                    heapq.heapify(open_set)
    raise RuntimeError("found no path!")


if __name__ == "__main__":
    grid = read_input()

    # tile grid five times
    nrows = 5 * len(grid)
    ncols = 5 * len(grid[0])

    big_grid = [[0 for _ in range(ncols)] for _ in range(nrows)]

    for i in range(nrows):
        row_index = i % len(grid)
        row_offset = i // len(grid)

        for j in range(ncols):
            col_index = j % len(grid[0])
            col_offset = j // len(grid[0])

            # reindexing: we need to go 1-9 and loop around to 1
            # same as going 0-8 and looping around to zero, all plus one
            big_grid[i][j] = (grid[row_index][col_index] - 1 +
                              row_offset + col_offset) % 9 + 1

    path_cost = cheapest_path(big_grid)
    print(path_cost)
