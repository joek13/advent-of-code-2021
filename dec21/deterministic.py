NUM_PLAYERS = 2

# idea: represent player positions as 0-9 instead of 1-10
# that way, we can compute wrapping using modulo 10.
player_positions = [4, 9]
player_scores = [0, 0]

rolls = 0


def roll(num_rolls: int) -> int:
    return num_rolls


while not any(score >= 1000 for score in player_scores):
    for player_idx in range(NUM_PLAYERS):
        # sum of three rolls will be n + n + 1 + n + 2, where n is the number of rolls plus one
        player_positions[player_idx] = (
            player_positions[player_idx] + (rolls + 1) * 3 + 3) % 10
        # add to player's score
        # add 1, since we use 0-9 instead of 1-10
        player_scores[player_idx] += player_positions[player_idx] + 1
        # count these rolls
        rolls += 3

losing_score = next(filter(lambda x: x < 1000, player_scores))
print(losing_score * rolls)
