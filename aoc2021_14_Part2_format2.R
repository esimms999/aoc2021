# aoc2021_14_1 <- read_csv("data/aoc2021_14_1_example.txt")
# KEY <- dplyr::pull(aoc2021_14_1,KEY)
# RESULT <- dplyr::pull(aoc2021_14_1, RESULT)

# for (i in 1:length(KEY)) {
#   cat(str_replace_all(paste("condition(x == '", KEY[i], "', '", RESULT[i], "'),", "\n"), " ", ""))
# }

library(fmtr)

fmt1 <- value(
  condition(x=='CH','CB'),
  condition(x=='HH','HN'),
  condition(x=='CB','CH'),
  condition(x=='NH','NC'),
  condition(x=='HB','HC'),
  condition(x=='HC','HB'),
  condition(x=='HN','HC'),
  condition(x=='NN','NC'),
  condition(x=='BH','BH'),
  condition(x=='NC','NB'),
  condition(x=='NB','NB'),
  condition(x=='BN','BB'),
  condition(x=='BB','BN'),
  condition(x=='BC','BB'),
  condition(x=='CC','CN'),
  condition(x=='CN','CC'))

orig_string <- "NNCB"

for (j in 1:15) {
  orig_string_length_less_one <- str_length(orig_string) - 1
  new_list <- vector("list", orig_string_length_less_one)

  for (i in 1:orig_string_length_less_one) {
    two_char <- str_sub(orig_string, i, i+1)
    new_list[[i]] <- fapply(two_char, fmt1, 2)
  }

  orig_string <- str_c(paste(new_list, collapse = ""), str_sub(orig_string, i+1))

  if (j > 1) { rm(new_list) }
  cat("End of loop j= ", j, "\n")
}

count_chars(orig_string)

