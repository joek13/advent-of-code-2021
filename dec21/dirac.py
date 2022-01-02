import functools

"""dynamic programming solution
idea: memoize by arr[player 1 pos, player 2 pos, player 1 score, player 2 score] = (universes in which player 1 wins, universes in which player 2 wins)

"""


@functools.cache
def winning_universes(player_1_pos, player_2_pos, player_1_score, player_2_score):
    if player_2_score >= 21:
        return (0, 1)

    # tuples (sum, number of ways to roll this sum)
    rolls = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

    winning_1 = 0
    winning_2 = 0

    for roll, n in rolls:
        # where is player 1 after this roll?
        new_pos = player_1_pos + roll
        # reindex so we can just modulo 10
        new_pos = ((new_pos - 1) % 10) + 1

        # how many universes do the players win starting from these conditions?
        recur_winning_2, recur_winning_1 = winning_universes(player_2_pos, new_pos,
                                                             player_2_score, player_1_score + new_pos)

        winning_1 += n * recur_winning_1
        winning_2 += n * recur_winning_2

    return winning_1, winning_2


print(max(winning_universes(5, 10, 0, 0)))
