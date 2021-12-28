import typing
import functools
import itertools
import operator

# tuple of (2d list of values, "background" color for the infinite border)
Image = typing.Tuple[typing.List[typing.List[bool]], bool]


def get_neighborhood(point):
    i, j = point

    # 9x9 grid centered on (i,j)
    neighborhood = [
        (i-1, j-1), (i-1, j), (i-1, j+1),
        (i, j-1), (i, j), (i, j+1),
        (i+1, j-1), (i+1, j), (i+1, j+1)
    ]

    return neighborhood


def bit_array_to_int(bit_array: typing.List[bool]) -> int:
    return functools.reduce(lambda s, x: 2*s + x, bit_array, 0)


def enhance(input_image: Image, algorithm: typing.List[bool]) -> Image:
    # new image needs to be padded out by one pixel.
    # if the input image is represented by *'s, then each of the +'s
    # could potentially be turned on through enhancement.
    # the .'s will certainly stay off, since they have only have off pixels
    # in their neighborhood.
    #    . . . . . .
    #    . + + + + .
    #    . + * * + .
    #    . + + + + .
    #    . . . . . .

    # unpack input image
    image_mat, bg = input_image

    input_rows = len(image_mat)
    input_cols = len(image_mat[0])

    output_rows = input_rows + 2  # one row above and one below
    output_cols = input_cols + 2  # one row left and one right

    # allocate empty image
    output_image = [[False for _ in range(output_cols)]
                    for _ in range(output_rows)]

    for i in range(output_rows):
        for j in range(output_cols):
            # read neighborhood in original image
            # reindex since we added a row of padding
            original_row, original_col = (i - 1, j - 1)

            vals = []
            neighborhood = get_neighborhood((original_row, original_col))

            # for each point in neighborhood,
            for (r, c) in neighborhood:
                # perform bounds check
                if 0 <= r < input_rows and 0 <= c < input_cols:
                    # if in bounds, append the value
                    vals.append(image_mat[r][c])
                else:
                    # if out of bounds, append whatever our background is
                    vals.append(bg)

            # convert array of bools to corresponding binary value
            algorithm_index = bit_array_to_int(vals)

            # lookup algorithm at this index
            output_image[i][j] = algorithm[algorithm_index]

    # each of the "background cells" (i.e., outside of the padded output image)
    # will all behave the same way, since their neighborhoods are always either
    # all 1s or all 0s. let's see what it is, and look up the appropriate value
    # from the algorithm.

    if bg:
        new_bg = algorithm[511]
    else:
        new_bg = algorithm[0]

    return (output_image, new_bg)


def read_input() -> typing.Tuple[typing.List[bool], Image]:
    # reads input and returns tuple of (image enhancement algorithm, input image)
    algorithm_string = input()

    # converts '#' to True entry and '.' to False
    algorithm = [(True if ch == '#' else False) for ch in algorithm_string]
    assert len(algorithm) == 512

    input()  # skip line

    image = []

    while True:
        try:
            # read single row
            line = input()
            # convert to bool
            image.append([(True if ch == '#' else False) for ch in line])
        except EOFError:
            break

    return algorithm, (image, False)  # by default infinite background = False


def show_image(image):
    for row in image:
        for cell in row:
            if cell:
                print("#", end="")
            else:
                print(".", end="")
        print()


if __name__ == "__main__":
    algorithm, image = read_input()

    iterations = 50

    for _ in range(iterations):
        image = enhance(image, algorithm)

    # flatten the image matrix
    image_flattened = functools.reduce(operator.concat, image[0], [])
    # count number of True entries
    print(sum((1 if x else 0) for x in image_flattened))
