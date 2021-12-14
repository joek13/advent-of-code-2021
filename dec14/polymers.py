import sys


def read_input():
    # read first line with starting polymer
    initial_polymer = input()

    substitution_rules = {}
    # each line thereafter contains a substitution rule
    for line in sys.stdin:
        line = line.rstrip()

        if line == "":
            continue

        pattern, subst = line.split(" -> ")

        substitution_rules[pattern] = subst

    return initial_polymer, substitution_rules


def apply_substitution(input_polymer: str, substitution_rules) -> str:
    """Applies the substitution rules to the input polymer,
    returning a new polymer.
    """

    # idea: we avoid concurrent modification (and messing around with indices)
    # by just building up the string as we scan through the input polymer.
    # every time we encounter a pair with a corresponding substitution,
    # we just insert the character in the working output string.

    # output string to be built up
    output = ""

    for ch, ch_next in zip(input_polymer, input_polymer[1:]):
        pattern = ch + ch_next

        output += ch  # add first character

        if pattern in substitution_rules:
            # add the new character in the middle
            subst = substitution_rules[pattern]
            output += subst

        # no need to add the last letter, as it is first of the next pair

    # add the final character, since it is never first of a pair
    output += input_polymer[-1]

    return output


if __name__ == "__main__":
    initial_polymer, subst_rules = read_input()

    polymer = initial_polymer

    for i in range(40):
        polymer = apply_substitution(polymer, subst_rules)

    # need to count the most and least common element
    element_counts = {
        elem: len([x for x in polymer if x == elem])
        for elem in set(polymer)
    }

    print(max(element_counts.values()) - min(element_counts.values()))
