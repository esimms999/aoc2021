# AOC2021 - Day 2

library(tidyverse)

aoc2021_02_1 <- read_csv("data/aoc2021_02_1.txt", show_col_types = FALSE)

# Part 1
aoc2021_02_1 %>%
  group_by(DIRECTION) %>%
  summarize(TOTAL_AMOUNT = sum(AMOUNT)) %>%
  pivot_wider(names_from = DIRECTION, values_from = TOTAL_AMOUNT) %>%
  transmute(ANSWER = forward * (down - up))

# Part 2
aoc2021_02_1 %>%
  mutate(AIM = case_when(DIRECTION == "down" ~ AMOUNT,
                         DIRECTION == "up" ~ 0 - AMOUNT,
                         TRUE ~ 0)) %>%
  mutate(TOTAL_AIM = cumsum((AIM))) %>%
  mutate(DEPTH = 0) %>%
  mutate(DEPTH = if_else(DIRECTION == "forward", TOTAL_AIM * AMOUNT, 0)) %>%
  filter(DIRECTION == "forward") %>%
  summarize(HORIZ = sum(AMOUNT), TOTAL_DEPTH = sum(DEPTH)) %>%
  transmute(ANSWER = HORIZ * TOTAL_DEPTH)



