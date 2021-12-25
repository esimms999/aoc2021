# AOC2021 - Day 6 new

library(tidyverse)

aoc2021_06_1 <- read_lines("data/aoc2021_06_1.txt")

# Part 1

vec <- unlist(strsplit(aoc2021_06_1, split=","))
fish_count_init <- table(vec)

fish_count <- c(day0=0,
                day1=fish_count_init[[1]],
                day2=fish_count_init[[2]],
                day3=fish_count_init[[3]],
                day4=fish_count_init[[4]],
                day5=0,
                day6=0,
                day7=0,
                day8=0,
                day9=0)

if ("5" %in% names(fish_count_init)) {
  fish_count["day5"] <- fish_count_init[[5]]
}

cat("Day: 1  ", fish_count, "\n")

for (day in c(1:80)) {
  if (fish_count["day0"] > 0) {
    fish_count["day7"] <- fish_count["day7"]+
      fish_count["day0"]
    fish_count["day9"] <- fish_count["day0"]
  }
  fish_count <- Lag(fish_count, -1)
  fish_count["day9"] <- 0
  cat("Day: ", day, "  ", fish_count, "\n")
}
sum(fish_count, na.rm = TRUE)
