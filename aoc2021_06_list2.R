# AOC2021 - Day 7

library(tidyverse)

aoc2021_07_1 <- read_lines("data/aoc2021_07_1.txt")

# Part 1

vec <- sort(as.numeric(unlist(strsplit(aoc2021_07_1, split=","))))
vec_min <- min(vec)
vec_max <- max(vec)
position <- seq(from = vec_min, to = vec_max)

total_fuel <- c()
for (select_pos in position) {
  fuel <- c()

    for (select_vec in vec) {
    fuel <- c(fuel, abs(select_vec - select_pos))
  }
  total_fuel <- c(total_fuel, sum(fuel))
}

min_fuel_part1 <- min(total_fuel)
min_fuel_pos_part1 <- which.min(total_fuel) - 1
cat("Part1: Minimum fuel: ", min_fuel_part1, "  at position: ", min_fuel_pos_part1, "\n")

# Part 2

total_fuel <- c()
for (select_pos in position) {
  fuel <- c()

  for (select_vec in vec) {
    n_pos <- abs(select_vec - select_pos)
    n_fuel <- n_pos * (n_pos + 1) / 2
    fuel <- c(fuel, n_fuel)
  }
  total_fuel <- c(total_fuel, sum(fuel))
}

min_fuel_part2 <- min(total_fuel)
min_fuel_pos_part2 <- which.min(total_fuel) - 1
cat("Part2: Minimum fuel: ", min_fuel_part2, "  at position: ", min_fuel_pos_part2, "\n")
