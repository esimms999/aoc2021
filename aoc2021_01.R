library(tidyverse)

aoc2021_01_1 <- read_csv("data/aoc2021_01_1.txt", show_col_types = FALSE)

# Part 1
aoc2021_01_1 %>%
  mutate(INC_DEC = if_else(DEPTH > lag(DEPTH), "INCREASED", "DECREASED")) %>%
  count(INC_DEC)

# Part 2
aoc2021_01_1 %>%
  mutate(WINDOW_SUM = DEPTH + lead(DEPTH) + lead(DEPTH, 2)) %>%
  mutate(INC_DEC = if_else(WINDOW_SUM > lag(WINDOW_SUM), "INCREASED", "DECREASED")) %>%
  count(INC_DEC)
