# Sorry. Using Python again.

import sys
import itertools


def read_grid_rows():
    """Generator yielding rows of the input grid.
    """
    for line in sys.stdin:
        # strip off trailing newline
        # read each char as an int
        yield [int(c) for c in line.rstrip()]


def get_neighbors(cell, nrows, ncols):
    """Returns list of cell neighbor coordinates within bounds.

    Args:
        cell: Tuple (i,j) of target
        nrows: number of rows in grid
        ncols: number of cols in grid
    """
    i, j = cell

    # all neighbors (including diagonals)
    all_neighbors = [
        (i-1, j-1), (i-1, j), (i-1, j+1),
        (i, j-1), (i, j+1),
        (i+1, j-1), (i+1, j), (i+1, j+1)
    ]

    # return only those in bounds
    return [(r, c) for (r, c) in all_neighbors if 0 <= r < nrows and 0 <= c < ncols]


def simulate_step(grid):
    nrows = len(grid)  # length of grid = # of rows
    ncols = len(grid[0])  # length of first row = # of cols

    # list of octopi to process flashes for
    flashes = []

    # iterate over all cells
    for i in range(nrows):
        for j in range(ncols):
            # increment by 1
            grid[i][j] += 1

            # if energy > 9, we need to process flash
            if grid[i][j] > 9:
                flashes.append((i, j))

    # list of octopi that have already flashed
    flashed = []

    while len(flashes) > 0:
        target = flashes.pop(0)

        # zero out this octopus
        i, j = target
        grid[i][j] = 0
        # mark as flashed so we do not visit again
        flashed.append(target)

        for (r, c) in get_neighbors(target, nrows, ncols):
            # do not bother updating already-flashed cells
            # or cells that are going to flash
            if (r, c) not in flashed and (r, c) not in flashes:
                grid[r][c] += 1

                if grid[r][c] > 9:
                    flashes.append((r, c))

    return len(flashed)  # return number of flashes that occured this step


def print_grid(grid):
    show = "\n".join(["".join(map(str, line)) for line in grid])
    print(show)


if __name__ == "__main__":
    grid = list(read_grid_rows())  # read and collect input grid

    total_flashes = 0

    print("After 0 steps:")
    print_grid(grid)
    for i in range(100):
        total_flashes += simulate_step(grid)
        print(f"After {i+1} steps:")
        print_grid(grid)
        print(f"({total_flashes} flashes)")

    print(total_flashes)
