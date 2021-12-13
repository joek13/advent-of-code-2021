import sys
import re


def read_input():
    points = []

    # read input
    # instead of parsing as grid, we read list of points
    # (will make mirroring points across fold line easier)
    for line in sys.stdin:
        line = line.rstrip()

        if line == "":
            # empty line means end of points
            break

        split = line.split(",")

        x = int(split[0])
        y = int(split[1])

        points.append((x, y))

    folds = []

    # read folds
    for line in sys.stdin:
        line = line.rstrip()

        # regex for vertical lines
        ver_line = re.compile("fold along x=(\d+)")
        # regex for horizontal lines
        hoz_line = re.compile("fold along y=(\d+)")

        if match := ver_line.match(line):
            folds.append(("vertical", int(match[1])))
        elif match := hoz_line.match(line):
            folds.append(("horizontal", int(match[1])))

    return points, folds


def simulate_fold(points, fold):
    fold_dir, fold_axis = fold

    if fold_dir == "horizontal":  # fold along y=k
        new_points = [
            # if we are below the fold axis, remain where we are
            # otherwise, mirror across the fold axis
            ((x, y) if y <= fold_axis else (x, fold_axis - (y - fold_axis)))
            for (x, y) in points
        ]

        # possible that some y-values are negative if fold brought bottom half above top half

        # minimum value of y across all points
        min_y = min([y for (_, y) in new_points])

        # if min_y < 0, then add -min_y to each point's y coordinate
        # else, just add zero to each point
        new_points = [(x, y + max(0, -min_y)) for (x, y) in new_points]

        return new_points
    elif fold_dir == "vertical":  # fold along x=k
        new_points = [
            # if we are below the fold axis, remain where we are
            # otherwise, mirror across the fold axis
            ((x, y) if x <= fold_axis else (fold_axis - (x - fold_axis), y))
            for (x, y) in points
        ]

        # possible that some x-values are negative
        # min x across all points
        min_x = min([x for (x, _) in new_points])

        # if min_x < 0, then add -min_x to each point's x coordinate
        # else, just add zero
        new_points = [(x + max(0, -min_x), y) for (x, y) in new_points]

        return new_points

    raise ValueError(f"Unrecognized fold {fold_dir}")


def show_points(points):
    """Prints out a visual representation of the list of coordinates.
    """
    width = max([x for (x, _) in points])
    height = max([y for (_, y) in points])

    for i in range(height + 1):
        for j in range(width + 1):
            if (j, i) in points:
                print("#", end="")
            else:
                print(" ", end="")
        print()


if __name__ == "__main__":
    points, folds = read_input()

    print(f"{len(points)} points to start")
    for i, fold in enumerate(folds):
        # hack: list to set and back removes duplicates
        points = list(set(simulate_fold(points, fold)))
        print(f"{len(points)} points after fold {i+1}")

    show_points(points)
