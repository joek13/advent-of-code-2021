import sys
import ast


def add_pair(a, b):
    return reduce([a, b])


def add_to_leftmost(num, a):
    if isinstance(num, int):
        return num + a
    else:
        return [add_to_leftmost(num[0], a), num[1]]


def add_to_rightmost(num, a):
    if isinstance(num, int):
        return num + a
    else:
        return [num[0], add_to_rightmost(num[1], a)]


def explode_leftmost(num, depth=0):
    # explodes the leftmost pair with sufficient nesting.
    # returns tuple of:
    # - new value for this number,
    # - "left overflow": value being pushed left from the explosion
    # - "right overflow": value being pushed right from the explosion
    # - boolean representing whether an explosion occured anywhere in this number

    if isinstance(num, int):
        # regular number
        # return without changing anything

        return num, 0, 0, False

    # snailfish pair
    # check if needs to be exploded

    if depth >= 4:
        # replace this number with zero
        # left overflow = left elem
        # right overflow = right elem

        assert isinstance(num[0], int)
        assert isinstance(num[1], int)

        # explodes the pair
        # pair becomes zero
        # left element overflows left
        # right element overflows right
        return 0, num[0], num[1], True
    else:
        # apply recursively to left elem
        left = num[0]

        new_left, overflow_left, overflow_right, left_exploded = explode_leftmost(
            left, depth=depth+1)

        right = num[1]

        # if there was an explosion on the left, we are done
        if left_exploded:
            # apply the rightward overflow
            return [new_left, add_to_leftmost(right, overflow_right)], overflow_left, 0, True
        else:
            # no left explosion, check right
            new_right, overflow_left, overflow_right, right_exploded = explode_leftmost(
                right, depth=depth+1)

            if right_exploded:
                return [add_to_rightmost(left, overflow_left), new_right], 0, overflow_right, True

        # no explosions, just return the number
        return [left, right], 0, 0, False


def split_leftmost(num):
    # takes a number and splits the leftmost regular number
    # returns tuple of:
    # - new number
    # - boolean representing whether a split has occurred

    if isinstance(num, int):
        # is a regular number

        if num >= 10:
            # hack for getting ceiling integer division
            # https://stackoverflow.com/questions/14822184/is-there-a-ceiling-equivalent-of-operator-in-python
            return [num // 2, -(num // -2)], True
        else:
            return num, False
    else:
        left = num[0]
        right = num[1]
        new_left, left_split = split_leftmost(left)

        if left_split:
            return [new_left, right], True
        else:
            new_right, right_split = split_leftmost(right)

            if right_split:
                return [left, new_right], True

        return [left, right], False


def reduce(num):

    def reduce_once(num):
        # first, we try to explode
        num, _, _, exploded = explode_leftmost(num)

        if not exploded:
            # if nothing explodes, then we split
            num, split = split_leftmost(num)

        return num, (exploded or split)

    new_num, changed = reduce_once(num)

    while changed:
        new_num, changed = reduce_once(new_num)

    return new_num


def magnitude(num):
    left = num[0]
    right = num[1]

    if isinstance(left, int):
        left_magnitude = 3 * left
    else:
        left_magnitude = 3 * magnitude(left)

    if isinstance(right, int):
        right_magnitude = 2 * right
    else:
        right_magnitude = 2 * magnitude(right)

    return left_magnitude + right_magnitude


if __name__ == "__main__":
    cumulative_sum = None

    # part 1 - find the answer to the homework assignment

    for line in sys.stdin:
        line = line.rstrip()
        # use python AST to parse lists
        next_num = ast.literal_eval(line)

        if cumulative_sum is None:
            cumulative_sum = next_num
        else:
            cumulative_sum = add_pair(cumulative_sum, next_num)

    print(magnitude(cumulative_sum))
