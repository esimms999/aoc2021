 aoc2021_14_1 <- read_csv("data/aoc2021_14_1_example.txt") %>%
   mutate(RESULT1 = str_c(str_sub(KEY,1,1), RESULT),
          RESULT2 = str_c(RESULT, str_sub(KEY,2,2)))
 KEY <- dplyr::pull(aoc2021_14_1,KEY)
 RESULT1 <- dplyr::pull(aoc2021_14_1, RESULT1)
 RESULT2 <- dplyr::pull(aoc2021_14_1, RESULT2)

 for (i in 1:length(KEY)) {
   cat(str_replace_all(paste("condition(x == '", KEY[i], "', '", RESULT1[i], "'),", "\n"), " ", ""))
 }

 cat("\n\n\n")


 for (i in 1:length(KEY)) {
    cat(str_replace_all(paste("condition(x == '", KEY[i], "', '", RESULT2[i], "'),", "\n"), " ", ""))
 }
