# Day 21

# Part 1

player1_score <- 0
player2_score <- 0
player1_location <- 9
player2_location <- 3
die_value <- 0
die_rolls <- 0

while(player1_score < 1000 & player2_score < 1000) {
  player1_die_roll1 <- die_value + 1
  player1_die_roll2 <- die_value + 2
  player1_die_roll3 <- die_value + 3
  if (player1_die_roll1 >100) {player1_die_roll1 < player1_die_roll1 - 100}
  if (player1_die_roll2 >100) {player1_die_roll2 < player1_die_roll2 - 100}
  if (player1_die_roll3 >100) {player1_die_roll3 < player1_die_roll3 - 100}

  die_total1 <- player1_die_roll1 + player1_die_roll2 + player1_die_roll3

  player1_location <- player1_location + (die_total1 %% 10)
  if (player1_location > 10) {player1_location <- player1_location - 10}
  player1_score <- player1_score + player1_location

  die_value <- die_value + 3
  if (die_value > 100) { die_value <- die_value - 100}

  if (player1_score < 1000) {
    player2_die_roll1 <- die_value + 1
    player2_die_roll2 <- die_value + 2
    player2_die_roll3 <- die_value + 3
    if (player2_die_roll1 >100) {player2_die_roll1 < player2_die_roll1 - 100}
    if (player2_die_roll2 >100) {player2_die_roll2 < player2_die_roll2 - 100}
    if (player2_die_roll3 >100) {player2_die_roll3 < player2_die_roll3 - 100}

    die_total2 <- player2_die_roll1 + player2_die_roll2 + player2_die_roll3

  player2_location <- player2_location + (die_total2 %% 10)
  if (player2_location > 10) {player2_location <- player2_location - 10}
  player2_score <- player2_score + player2_location

  die_value <- die_value + 3
  if (die_value > 100) { die_value <- die_value - 100}

  die_rolls <- die_rolls + 6
  } else {
      die_rolls <- die_rolls + 3
  }

  cat("die_rolls: ", die_rolls, "   die_value: ", die_value, "   player1_location: ", player1_location, "   player1_score: ", player1_score,
      "   player2_location: ", player2_location, "   player2_score: ", player2_score,"\n")
}

cat("die_rolls: ", die_rolls, "   player1_score: ", player1_score, "   player2_score: ", player2_score, "\n")
if (player1_score > player2_score) {
  cat("Answer: ", player2_score * die_rolls)
} else {
    cat("Answer: ", player1_score * die_rolls)
}
