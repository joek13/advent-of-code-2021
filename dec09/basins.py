"""Sorry. I cheated and wrote part 2 of today in Python.
"""

import functools
import operator
import sys


def read_grid_rows():
    """Generator that reads rows from stdin.
    """
    for line in sys.stdin:
        yield [int(c) for c in line.rstrip()]


grid = list(read_grid_rows())

# height of matrix is just number of rows
height = len(grid)
# width of matrix is length of first row
width = len(grid[0])

# each basin is uniquely defined by a low point
# so if we find the basin of each low point, we know the size of each basin


def is_in_bounds(point):
    """Predicate that tests whether (i,j) is in bounds
    """
    i, j = point
    return 0 <= i < height and 0 <= j < width


def neighbors_in_bounds(point):
    i, j = point
    return filter(is_in_bounds, [(i-1, j), (i+1, j), (i, j-1), (i, j+1)])


def is_low_point(point):
    i, j = point
    # only check in-bounds neighbors
    for (r, c) in neighbors_in_bounds(point):
        if not is_in_bounds((r, c)):
            continue

        if grid[i][j] >= grid[r][c]:
            # not a low point
            return False

    return True


# constructs 2d array of coordinates (i,j)
# and then flattens along the first axis so we have a list of all spaces in the graph
coords = sum([[(i, j) for j in range(width)] for i in range(height)], [])
low_points = filter(is_low_point, coords)

# now, we floodfill from each low point
basin_sizes = []

basins = [[0 for _ in range(width)] for _ in range(height)]

for basin_id, low_point in enumerate(low_points):
    count = 0
    visited = []
    to_visit = [low_point]

    while len(to_visit) > 0:
        next_cell = to_visit.pop(0)

        if next_cell in visited:
            continue

        i, j = next_cell

        # mark cell as visited so we do not double-count
        visited.append(next_cell)

        # count this cell
        count += 1

        basins[i][j] = basin_id + 1

        for neighbor in neighbors_in_bounds(next_cell):
            r, c = neighbor
            if grid[r][c] != 9:
                to_visit.append(neighbor)

    basin_sizes.append(count)

largest_basins = sorted(basin_sizes, reverse=True)[:3]

print("Visualization:")

for row in basins:
    for entry in row:
        print(entry if entry != 0 else ".", end="")
    print()

print("Answer:")
print(functools.reduce(operator.mul, largest_basins, 1))
