# AOC2021 - Day 3
# Brute force - needs some work!

library(tidyverse)
library(data.table)

aoc2021_03_1 <- read_csv("data/aoc2021_03_1.txt", show_col_types = FALSE)

# Part 2

BIT_COLS <- str_c("BIT_", str_pad(1:12, width = 2, pad = 0))
split_bits <- aoc2021_03_1 %>% separate(DIAGNOSTIC, into=BIT_COLS,
                               seq(1:11), remove = FALSE)

POS <- split_bits %>%
  mutate(POS_01 = if_else(sum(as.numeric(BIT_01)) >= 501, "1", "0"),
         POS_02 = if_else(sum(as.numeric(BIT_02)) >= 501, "1", "0"),
         POS_03 = if_else(sum(as.numeric(BIT_03)) >= 501, "1", "0"),
         POS_04 = if_else(sum(as.numeric(BIT_04)) >= 501, "1", "0"),
         POS_05 = if_else(sum(as.numeric(BIT_05)) >= 501, "1", "0"),
         POS_06 = if_else(sum(as.numeric(BIT_06)) >= 501, "1", "0"),
         POS_07 = if_else(sum(as.numeric(BIT_07)) >= 501, "1", "0"),
         POS_08 = if_else(sum(as.numeric(BIT_08)) >= 501, "1", "0"),
         POS_09 = if_else(sum(as.numeric(BIT_09)) >= 501, "1", "0"),
         POS_10 = if_else(sum(as.numeric(BIT_10)) >= 501, "1", "0"),
         POS_11 = if_else(sum(as.numeric(BIT_11)) >= 501, "1", "0"),
         POS_12 = if_else(sum(as.numeric(BIT_12)) >= 501, "1", "0"),
         )

POS2 <- POS %>%
  mutate(bucket_01 = if_else(POS_01 == BIT_01, "OXY", "SCR"),
         bucket_02 = if_else(POS_02 == BIT_02, "OXY", "SCR"),
         bucket_03 = if_else(POS_03 == BIT_03, "OXY", "SCR"),
         bucket_04 = if_else(POS_04 == BIT_04, "OXY", "SCR"),
         bucket_05 = if_else(POS_05 == BIT_05, "OXY", "SCR"),
         bucket_06 = if_else(POS_06 == BIT_06, "OXY", "SCR"),
         bucket_07 = if_else(POS_07 == BIT_07, "OXY", "SCR"),
         bucket_08 = if_else(POS_08 == BIT_08, "OXY", "SCR"),
         bucket_09 = if_else(POS_09 == BIT_09, "OXY", "SCR"),
         bucket_10 = if_else(POS_10 == BIT_10, "OXY", "SCR"),
         bucket_11 = if_else(POS_11 == BIT_11, "OXY", "SCR"),
         bucket_12 = if_else(POS_12 == BIT_12, "OXY", "SCR"))

POS3 <- POS2 %>%
  mutate(bucket_02 = if_else(bucket_01 != bucket_02, "", bucket_02),
         bucket_03 = if_else(bucket_02 != bucket_03, "", bucket_03),
         bucket_04 = if_else(bucket_03 != bucket_04, "", bucket_04),
         bucket_05 = if_else(bucket_04 != bucket_05, "", bucket_05),
         bucket_06 = if_else(bucket_05 != bucket_06, "", bucket_06),
         bucket_07 = if_else(bucket_06 != bucket_07, "", bucket_07),
         bucket_08 = if_else(bucket_07 != bucket_08, "", bucket_08),
         bucket_09 = if_else(bucket_08 != bucket_09, "", bucket_09),
         bucket_10 = if_else(bucket_09 != bucket_10, "", bucket_10),
         bucket_11 = if_else(bucket_10 != bucket_11, "", bucket_11),
         bucket_12 = if_else(bucket_11 != bucket_12, "", bucket_12))

