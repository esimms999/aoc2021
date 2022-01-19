# aoc2021_14_1 <- read_csv("data/aoc2021_14_1_example.txt")
# KEY <- dplyr::pull(aoc2021_14_1,KEY)
# RESULT <- dplyr::pull(aoc2021_14_1, RESULT)

# for (i in 1:length(KEY)) {
#   cat(str_replace_all(paste("condition(x == '", KEY[i], "', '", RESULT[i], "'),", "\n"), " ", ""))
# }

library(fmtr)

fmt1 <- value(
  condition(x=='CH','CBBH'),
  condition(x=='HH','HNNH'),
  condition(x=='CB','CHHB'),
  condition(x=='NH','NCCH'),
  condition(x=='HB','HCCB'),
  condition(x=='HC','HBBC'),
  condition(x=='HN','HCCN'),
  condition(x=='NN','NCCN'),
  condition(x=='BH','BHHH'),
  condition(x=='NC','NBBC'),
  condition(x=='NB','NBBB'),
  condition(x=='BN','BBBN'),
  condition(x=='BB','BNNB'),
  condition(x=='BC','BBBC'),
  condition(x=='CC','CNNC'),
  condition(x=='CN','CCCN'))

orig_string <- "NNCB"
orig_string_last_char <- str_sub(orig_string, -1)

current_list <- substring(orig_string, first = 1:(nchar(orig_string) - 1), last = 2:nchar(orig_string))

for (j in 1:10) {
  new_list <- vector("list", 2 * length(current_list))
  cat("Progress: ")
  #fmt_list <- fapply(current_list, fmt1)
  cat("1")

  for (i in 1:length(current_list)) {
    four_char <- fmtr::fapply(current_list[[i]], fmt1)

    new_list[[(2*i)-1]] <- substr(four_char, 1, 2)
    new_list[[2*i]] <- substr(four_char, 3, 4)
  }
  cat("2")

  if (j<10) { current_list <- new_list }
  cat("End of loop j= ", j, "\n")
}

final_list <- vector("list", (length(new_list)/2) + 1)
for (i in 1:(length(new_list)/2)) {
  final_list[[i]] <- new_list[[2 * i - 1]]
}
final_list[[i+1]] <- orig_string_last_char
total_counts <- count_chars(final_list)
answer <- total_counts[[1]] - total_counts[[length(total_counts)]]

cat("Answer: ", answer, "\n")
total_counts
