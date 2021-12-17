import re
import itertools


def read_input():
    """Reads input and returns a corners of rectangle representing the target area.
    """
    input_regexp = re.compile(
        "target area: x=([-\d]+)..([-\d]+), y=([-\d]+)..([-\d]+)")
    match = input_regexp.match(input())

    x1 = int(match[1])
    x2 = int(match[2])
    y1 = int(match[3])
    y2 = int(match[4])

    return (min(x1, x2), min(y1, y2)), (max(x1, x2), max(y1, y2))


def sign(x):
    if x >= 0:
        return 1
    else:
        return -1


def trajectory_hits(velocity, target_area):
    (x1, y1), (x2, y2) = target_area

    vx, vy = velocity
    x, y = 0, 0

    # position x is really just vx + vx - 1 + vx - 2 + ... until we reach zero
    # this series in general is (vx)(vx + 1) / 2
    if vx * (vx + 1) // 2 < x1:
        return False, None  # can't make it to the zone at all

    max_altitude = 0

    while True:
        # move by vx,vy
        x += vx
        y += vy

        # check for new max alt
        if y > max_altitude:
            max_altitude = y

        # apply drag
        if vx != 0:
            vx -= sign(vx) * 1
        # apply gravity
        vy -= 1

        # check whether we are in target area
        if x1 <= x <= x2 and y1 <= y <= y2:
            return True, max_altitude  # projectile in target area

        if vx == 0 and not (x1 <= x <= x2):
            return False, None  # not moving horizontally, and we are not above/below the target zone

        if vx >= 0 and x > x2:
            # moving right, and we have passed the target zone
            return False, None

        if vx <= 0 and x < x1:
            # moving left, and we have passed the target zone
            return False, None

        if vy < 0 and y < y1:
            return False, None  # below the target area, and we hvae no upward momentum


if __name__ == "__main__":
    target = read_input()
    print("target area:", target)
    (x1, y1), (x2, y2) = target

    count = 0  # count of working velocities
    max_alt = 0

    max_alt = 0
    for vx in itertools.count(0):
        if vx > x2:
            break  # will skip past the zone on the first step

        for vy in range(min(y1, 0), 2000):
            hits, alt = trajectory_hits((vx, vy), target)

            if hits:
                count += 1

                if alt > max_alt:
                    max_alt = alt

    print(count, "working velocities")
    print(max_alt, "max altitude")
