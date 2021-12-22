# Day 14

# Part 1

orig <- "NNCB"

orig_length_less_one <- str_length(orig) - 1

aoc2021_14_1 <- read_csv("data/aoc2021_14_1_example.txt", show_col_types = FALSE)

# Split into pairs

answer <- str("")

for (j in 1:10) {
  for (i in 1:orig_length_less_one) {
    two_char <- substring(orig, i, i+1)
    char1 <- substring(two_char, 1, 1)
    char2 <- substring(two_char, 2, 2)
    zz <-match(two_char, aoc2021_14_1$KEY)
    result <- str_trim(aoc2021_14_1$RESULT[zz])
    if (i == orig_length_less_one) {
      three_char <- paste(char1, result, char2)
    } else {
      three_char <- paste(char1, result)
    }
    answer <- str_replace_all(paste(answer, three_char), " ", "")
    # cat("two_char: ", two_char, "   char1: ", char1, "   char2: ", char2, "   zz: ", zz, "   result:", result, "   answer: ", answer, "\n")
  }
  cat("ANSWER: ", answer, "\n")
  orig <- answer
  orig_length_less_one <- str_length(orig) - 1
  if (j < 10) { answer <- NULL }
}

count_chars(answer)




