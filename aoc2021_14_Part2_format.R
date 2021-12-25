# aoc2021_14_1 <- read_csv("data/aoc2021_14_1_example.txt")
# KEY <- dplyr::pull(aoc2021_14_1,KEY)
# RESULT <- dplyr::pull(aoc2021_14_1, RESULT)

# for (i in 1:length(KEY)) {
#   cat(str_replace_all(paste("condition(x == '", KEY[i], "', '", RESULT[i], "'),", "\n"), " ", ""))
# }

fmt1 <- value(
  condition(x=='CH','B'),
  condition(x=='HH','N'),
  condition(x=='CB','H'),
  condition(x=='NH','C'),
  condition(x=='HB','C'),
  condition(x=='HC','B'),
  condition(x=='HN','C'),
  condition(x=='NN','C'),
  condition(x=='BH','H'),
  condition(x=='NC','B'),
  condition(x=='NB','B'),
  condition(x=='BN','B'),
  condition(x=='BB','N'),
  condition(x=='BC','B'),
  condition(x=='CC','N'),
  condition(x=='CN','C'))

orig_string <- "NNCB"

for (j in 1:10) {
  if (j > 1) { rm(new_list) }
  new_list <- vector("list", str_length(orig_string) - 1)

  for (i in 1:(str_length(orig_string)-1)) {
    two_char <- str_sub(orig_string, i, i+1)
    new_two_char <- str_c(str_sub(two_char, 1, 1), fapply(two_char, fmt1))
    new_list[[i]] <- new_two_char
  }

  new_string <- str_replace_all(str_c(toString(new_list), str_sub(orig_string, i+1)), "[, ]", "")
  cat(j, "\n")
  orig_string <- new_string
}

count_chars(orig_string)

