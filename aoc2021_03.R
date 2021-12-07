# AOC2021 - Day 3
# Brute force - needs some work!

library(tidyverse)
library(data.table)

aoc2021_03_1 <- read_csv("data/aoc2021_03_1.txt", show_col_types = FALSE)

# Part 1
split_bits <- aoc2021_03_1 %>% separate(DIAGNOSTIC, c("BIT_01", "BIT_02", "BIT_03", "BIT_04", "BIT_05", "BIT_06",
                                                      "BIT_07", "BIT_08", "BIT_09", "BIT_10", "BIT_11", "BIT_12"), seq(1:11))

gamma_01 <- split_bits %>%
  count(BIT_01, "1") %>%
  pivot_wider(names_from = BIT_01, values_from = n, names_prefix = "COUNT_") %>%
  transmute(gamma_01 = if_else(COUNT_1 > COUNT_0, "1", "0")) %>%
  mutate(epsilon_01 = if_else(gamma_01 == "0", "1", "0"))

gamma_02 <- split_bits %>%
  count(BIT_02, "1") %>%
  pivot_wider(names_from = BIT_02, values_from = n, names_prefix = "COUNT_") %>%
  transmute(gamma_02 = if_else(COUNT_1 > COUNT_0, "1", "0")) %>%
  mutate(epsilon_02 = if_else(gamma_02 == "0", "1", "0"))

gamma_03 <- split_bits %>%
  count(BIT_03, "1") %>%
  pivot_wider(names_from = BIT_03, values_from = n, names_prefix = "COUNT_") %>%
  transmute(gamma_03 = if_else(COUNT_1 > COUNT_0, "1", "0")) %>%
  mutate(epsilon_03 = if_else(gamma_03 == "0", "1", "0"))

gamma_04 <- split_bits %>%
  count(BIT_04, "1") %>%
  pivot_wider(names_from = BIT_04, values_from = n, names_prefix = "COUNT_") %>%
  transmute(gamma_04 = if_else(COUNT_1 > COUNT_0, "1", "0")) %>%
  mutate(epsilon_04 = if_else(gamma_04 == "0", "1", "0"))

gamma_05 <- split_bits %>%
  count(BIT_05, "1") %>%
  pivot_wider(names_from = BIT_05, values_from = n, names_prefix = "COUNT_") %>%
  transmute(gamma_05 = if_else(COUNT_1 > COUNT_0, "1", "0")) %>%
  mutate(epsilon_05 = if_else(gamma_05 == "0", "1", "0"))

gamma_06 <- split_bits %>%
  count(BIT_06, "1") %>%
  pivot_wider(names_from = BIT_06, values_from = n, names_prefix = "COUNT_") %>%
  transmute(gamma_06 = if_else(COUNT_1 > COUNT_0, "1", "0")) %>%
  mutate(epsilon_06 = if_else(gamma_06 == "0", "1", "0"))

gamma_07 <- split_bits %>%
  count(BIT_07, "1") %>%
  pivot_wider(names_from = BIT_07, values_from = n, names_prefix = "COUNT_") %>%
  transmute(gamma_07 = if_else(COUNT_1 > COUNT_0, "1", "0")) %>%
  mutate(epsilon_07 = if_else(gamma_07 == "0", "1", "0"))

gamma_08 <- split_bits %>%
  count(BIT_08, "1") %>%
  pivot_wider(names_from = BIT_08, values_from = n, names_prefix = "COUNT_") %>%
  transmute(gamma_08 = if_else(COUNT_1 > COUNT_0, "1", "0")) %>%
  mutate(epsilon_08 = if_else(gamma_08 == "0", "1", "0"))

gamma_09 <- split_bits %>%
  count(BIT_09, "1") %>%
  pivot_wider(names_from = BIT_09, values_from = n, names_prefix = "COUNT_") %>%
  transmute(gamma_09 = if_else(COUNT_1 > COUNT_0, "1", "0")) %>%
  mutate(epsilon_09 = if_else(gamma_09 == "0", "1", "0"))

gamma_10 <- split_bits %>%
  count(BIT_10, "1") %>%
  pivot_wider(names_from = BIT_10, values_from = n, names_prefix = "COUNT_") %>%
  transmute(gamma_10 = if_else(COUNT_1 > COUNT_0, "1", "0")) %>%
  mutate(epsilon_10 = if_else(gamma_10 == "0", "1", "0"))

gamma_11 <- split_bits %>%
  count(BIT_11, "1") %>%
  pivot_wider(names_from = BIT_11, values_from = n, names_prefix = "COUNT_") %>%
  transmute(gamma_11 = if_else(COUNT_1 > COUNT_0, "1", "0")) %>%
  mutate(epsilon_11 = if_else(gamma_11 == "0", "1", "0"))

gamma_12 <- split_bits %>%
  count(BIT_12, "1") %>%
  pivot_wider(names_from = BIT_12, values_from = n, names_prefix = "COUNT_") %>%
  transmute(gamma_12 = if_else(COUNT_1 > COUNT_0, "1", "0")) %>%
  mutate(epsilon_12 = if_else(gamma_12 == "0", "1", "0"))

combine_12 <- merge(gamma_01, gamma_02)
combine_34 <- merge(gamma_03, gamma_04)
combine_56 <- merge(gamma_05, gamma_06)
combine_78 <- merge(gamma_07, gamma_08)
combine_910 <- merge(gamma_09, gamma_10)
combine_1112 <- merge(gamma_11, gamma_12)

combine_1234 <- merge(combine_12, combine_34)
combine_5678 <- merge(combine_56, combine_78)
combine_9101112 <- merge(combine_910, combine_1112)

combine_12345678 <- merge(combine_1234, combine_5678)
combine_all <- merge(combine_12345678, combine_9101112)

df <- combine_all %>%
  mutate(gamma = str_c(gamma_01, gamma_02, gamma_03, gamma_04, gamma_05, gamma_06,
                       gamma_07, gamma_08, gamma_09, gamma_10, gamma_11, gamma_12)) %>%
  mutate(epsilon = str_c(epsilon_01, epsilon_02, epsilon_03, epsilon_04, epsilon_05, epsilon_06,
                         epsilon_07, epsilon_08, epsilon_09, epsilon_10, epsilon_11, epsilon_12)) %>%
  mutate(answer = strtoi(gamma, base = 2) * strtoi(epsilon, base = 2))
df

# Part 2 - incomplete

split_bits <- aoc2021_03_1 %>% separate(DIAGNOSTIC, c("BIT_01", "BIT_02", "BIT_03", "BIT_04", "BIT_05", "BIT_06",
                                                      "BIT_07", "BIT_08", "BIT_09", "BIT_10", "BIT_11", "BIT_12"),
                                        seq(1:11), remove = FALSE)

POS <- split_bits %>%
  mutate(POS_01 = sum(as.numeric(BIT_01)),
         POS_02 = sum(as.numeric(BIT_02)),
         POS_03 = sum(as.numeric(BIT_03)),
         POS_04 = sum(as.numeric(BIT_04)),
         POS_05 = sum(as.numeric(BIT_05)),
         POS_06 = sum(as.numeric(BIT_06)),
         POS_07 = sum(as.numeric(BIT_07)),
         POS_08 = sum(as.numeric(BIT_08)),
         POS_09 = sum(as.numeric(BIT_09)),
         POS_10 = sum(as.numeric(BIT_10)),
         POS_11 = sum(as.numeric(BIT_11)),
         POS_12 = sum(as.numeric(BIT_12))) %>%
  mutate(POS_01_flag = if_else(POS_01 >= 500, "HIGH", "LOW"),
         POS_02_flag = if_else(POS_02 >= 500, "HIGH", "LOW"),
         POS_03_flag = if_else(POS_03 >= 500, "HIGH", "LOW"),
         POS_04_flag = if_else(POS_04 >= 500, "HIGH", "LOW"),
         POS_05_flag = if_else(POS_05 >= 500, "HIGH", "LOW"),
         POS_06_flag = if_else(POS_06 >= 500, "HIGH", "LOW"),
         POS_07_flag = if_else(POS_07 >= 500, "HIGH", "LOW"),
         POS_08_flag = if_else(POS_08 >= 500, "HIGH", "LOW"),
         POS_09_flag = if_else(POS_09 >= 500, "HIGH", "LOW"),
         POS_10_flag = if_else(POS_10 >= 500, "HIGH", "LOW"),
         POS_11_flag = if_else(POS_11 >= 500, "HIGH", "LOW"),
         POS_12_flag = if_else(POS_12 >= 500, "HIGH", "LOW")) %>%
  select(-c(POS_01, POS_02, POS_03, POS_04, POS_05, POS_06,
            POS_07, POS_08, POS_09, POS_10, POS_11, POS_12))

POS_01 <- POS %>%
    mutate(bucket_01 = if_else( (POS_01_flag == "HIGH" & BIT_01 == "1") |
                                (POS_01_flag == "LOW" & BIT_01 == "0"), "OXY", "SCR"))
POS_02 <- POS_01 %>%
    mutate(bucket_02 = if_else( (POS_02_flag == "HIGH" & BIT_02 == "1") |
                                (POS_02_flag == "LOW" & BIT_02 == "0"), "OXY", "SCR")) %>%
    mutate(bucket_02 = if_else(bucket_01 != bucket_02, "", bucket_02))

POS_03 <- POS_02 %>%
    mutate(bucket_03 = if_else( (POS_03_flag == "HIGH" & BIT_03 == "1") |
                                (POS_03_flag == "LOW" & BIT_03 == "0"), "OXY", "SCR")) %>%
    mutate(bucket_03 = if_else(bucket_02 != bucket_03, "", bucket_03))

POS_04 <- POS_03 %>%
    mutate(bucket_04 = if_else( (POS_04_flag == "HIGH" & BIT_04 == "1") |
                                (POS_04_flag == "LOW" & BIT_04 == "0"), "OXY", "SCR")) %>%
    mutate(bucket_04 = if_else(bucket_03 != bucket_04, "", bucket_04))

POS_05 <- POS_04 %>%
    mutate(bucket_05 = if_else( (POS_05_flag == "HIGH" & BIT_05 == "1") |
                                (POS_05_flag == "LOW" & BIT_05 == "0"), "OXY", "SCR")) %>%
    mutate(bucket_05 = if_else(bucket_04 != bucket_05, "", bucket_05))

POS_06 <- POS_05 %>%
    mutate(bucket_06 = if_else( (POS_06_flag == "HIGH" & BIT_06 == "1") |
                                (POS_06_flag == "LOW" & BIT_06 == "0"), "OXY", "SCR"))%>%
    mutate(bucket_06 = if_else(bucket_05 != bucket_06, "", bucket_06))

POS_07 <- POS_06 %>%
  mutate(bucket_07 = if_else( (POS_07_flag == "HIGH" & BIT_07 == "1") |
                                (POS_07_flag == "LOW" & BIT_07 == "0"), "OXY", "SCR")) %>%
  mutate(bucket_07 = if_else(bucket_06 != bucket_07, "", bucket_07))

POS_08 <- POS_07 %>%
  mutate(bucket_08 = if_else( (POS_08_flag == "HIGH" & BIT_08 == "1") |
                                (POS_08_flag == "LOW" & BIT_08 == "0"), "OXY", "SCR")) %>%
  mutate(bucket_08 = if_else(bucket_07 != bucket_08, "", bucket_08))

POS_09 <- POS_08 %>%
  mutate(bucket_09 = if_else( (POS_09_flag == "HIGH" & BIT_09 == "1") |
                                (POS_09_flag == "LOW" & BIT_09 == "0"), "OXY", "SCR")) %>%
  mutate(bucket_09 = if_else(bucket_08 != bucket_09, "", bucket_09))

POS_10 <- POS_09 %>%
  mutate(bucket_10 = if_else( (POS_10_flag == "HIGH" & BIT_10 == "1") |
                                (POS_10_flag == "LOW" & BIT_10 == "0"), "OXY", "SCR")) %>%
  mutate(bucket_10 = if_else(bucket_09 != bucket_10, "", bucket_10))

POS_11 <- POS_10 %>%
  mutate(bucket_11 = if_else( (POS_11_flag == "HIGH" & BIT_11 == "1") |
                                (POS_11_flag == "LOW" & BIT_11 == "0"), "OXY", "SCR")) %>%
  mutate(bucket_11 = if_else(bucket_10 != bucket_11, "", bucket_11))

POS_12 <- POS_11 %>%
  mutate(bucket_12 = if_else( (POS_12_flag == "HIGH" & BIT_12 == "1") |
                                (POS_12_flag == "LOW" & BIT_12 == "0"), "OXY", "SCR")) %>%
  mutate(bucket_12 = if_else(bucket_11 != bucket_12, "", bucket_12))

POS_12 %>% filter(bucket_12 == "OXY")
abc <-POS_12 %>% filter(bucket_08 == "SCR")



abc %>% filter(xxx == DIAGNOSTIC) %>%
  select(DIAGNOSTIC, xxx)
  mutate(xxx = str_c(BIT_01, BIT_02, BIT_03, BIT_04, BIT_05, BIT_06, BIT_07, BIT_08, BIT_09, BIT_10,BIT_11, BIT_12))

