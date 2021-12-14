import sys
import collections


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


def build_pairs(polymer):
    counter = collections.Counter()

    for ch, ch_next in zip(polymer, polymer[1:]):
        counter[ch + ch_next] += 1

    # edge case: add one "lone" pair for the last element
    # if we count elements by first member of the pair the last element never gets counted
    # so this way, XYZ becomes XY YZ Z
    counter[polymer[-1]] += 1

    return counter


def simulate_step(pair_counts, substitution_rules):
    new_counts = pair_counts.copy()

    for (pair, count) in pair_counts.items():
        if pair in substitution_rules and count > 0:
            subst = substitution_rules[pair]

            # if we have rule XZ -> XYZ
            # then a pair XZ becomes two pairs: XY, YZ
            new_counts[pair] -= count

            fst = pair[0]
            snd = pair[1]

            new_counts[fst + subst] += count
            new_counts[subst + snd] += count

    return new_counts


if __name__ == "__main__":
    initial_polymer, subst_rules = read_input()

    # idea: instead of maintaining the actual string of polymers, just maintain a map of pairs and their associated counts
    pairs = build_pairs(initial_polymer)

    for i in range(40):
        pairs = simulate_step(pairs, subst_rules)

    # count elements
    counts = collections.Counter()

    for (pair, count) in pairs.items():
        elem = pair[0]

        counts[elem] += count

    print(max(counts.values()) - min(counts.values()))
