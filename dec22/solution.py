import re
import functools
import operator


cuboid_regex = re.compile(r"(?:x|y|z)=(-?\d+)..(-?\d+)")


def read_input(part1=False):
    # returns list of tuples (instr, cuboid)
    # where instr = "on"/"off"
    # cuboid is list of 3 2-tuples (x1, x2), (y1, y2), (z1, z2)
    inputs = []
    try:
        while True:
            # instr = "on"/"off"
            # cuboid_str is comma-separated list of bounds
            instr, cuboid_str = input().split(" ")
            cuboid = []

            out_of_range = False

            for range in cuboid_str.split(","):
                match = cuboid_regex.match(range)

                if part1 and max(abs(int(match[1])), abs(int(match[2]))) > 50:
                    # part 1: skip if cube is out of range -50 <= x <= 50
                    out_of_range = True
                else:
                    # first capturing group = beginning of range
                    # second capturing group = end of range
                    cuboid.append((int(match[1]), int(match[2])))

            if not out_of_range:
                inputs.append((instr, cuboid))

    except EOFError:
        pass

    return inputs

# idea: store "regions" which are just cuboids that share the same status
# every time we introduce a new cuboid, have to compute intersection so that
# we can use inclusion-exclusion to avoid double-counting


def compute_intersection(cuboid_a, cuboid_b):
    # intersection with empty set is itself empty
    if cuboid_a is None or cuboid_b is None:
        return None

    # for each axis: if the extremes are entirely outside one another,
    # the intersection is empty
    for (ax1, ax2), (bx1, bx2) in zip(cuboid_a, cuboid_b):
        if not (bx1 <= ax2 <= bx2 or ax1 <= bx2 <= ax2):
            return None

    # intersection is:
    #   the largest of each min
    #   the smallest of each max
    # (on each coordinate)
    return [(max(ax1, bx1), min(ax2, bx2)) for (ax1, ax2), (bx1, bx2) in zip(cuboid_a, cuboid_b)]


def volume(cuboid):
    # (need plus 1 since we are using inclusive boundaries)
    return functools.reduce(operator.mul, (x2 - x1 + 1 for x1, x2 in cuboid), 1)


def choose_intersections(cuboids, k=1):
    # returns list of all possible intersections between groups of k cuboids

    if k < 1:
        # not possible to pick fewer than one cuboid
        return None
    if k == 1:
        # intersections of groups of 1 cuboids = the cuboids themselves
        return cuboids
    else:
        # recursive case:
        intersections = []
        for i, cube in enumerate(cuboids):
            # pick a starting cube,
            # and intersect it with each of the ways
            # to pick (k-1) cubes from the remaining cubes

            for recur in choose_intersections(cuboids[i+1:], k=k-1):
                intersection = compute_intersection(cube, recur)

                if intersection is not None:
                    intersections.append(intersection)

        return intersections


if __name__ == "__main__":
    instructions = read_input(part1=False)

    # V(A U B U C) = V(A) + V(B) + V(C) - V(AB) - V(BC) - V(AC) + V(ABC)

    # cuboids whose volumes we count
    positive_cuboids = []

    # cuboids whose volumes we subtract
    # (to avoid double counting)
    negative_cuboids = []

    for (instr, cuboid) in instructions:
        if instr == "on":
            to_add = [cuboid]
        else:
            to_add = []

        to_sub = []

        for other_cuboid in positive_cuboids:
            intersect = compute_intersection(cuboid, other_cuboid)
            if intersect:
                to_sub.append(intersect)

        for other_cuboid in negative_cuboids:
            intersect = compute_intersection(cuboid, other_cuboid)
            if intersect:
                to_add.append(intersect)

        positive_cuboids += to_add
        negative_cuboids += to_sub

    print(sum(map(volume, positive_cuboids)) -
          sum(map(volume, negative_cuboids)))
