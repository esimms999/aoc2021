# Day 14

library(ds4psy)

# Part 1

orig_string <- "PHVCVBFHCVPFKBNHKNBO"
aoc2021_14_1 <- read_csv("data/aoc2021_14_1.txt", show_col_types = FALSE)

orig_vector <- vector("character")

for (j in 1:10) {

   for (i in 1:str_length(orig_string)-1) {
     new_vec_element <- str_sub(orig_string, i, i+1)
     if (i == 1) {
       orig_vector <- new_vec_element
     } else {
       orig_vector <- c(orig_vector, new_vec_element)
     }
   }

   orig_df <- tibble(KEY = orig_vector)

   df2 <- left_join(orig_df, aoc2021_14_1,
                    by = "KEY") %>%
     mutate(new_vals = str_c(str_sub(KEY, 1, 1), RESULT)) %>%
     mutate(new_vals = if_else(is.na(lead(KEY)), str_c(new_vals, str_sub(KEY, 2, 2)), new_vals))

   orig_string <- str_replace_all(toString(df2$new_vals), "[, ]", "")

   cat("Loop: ", j, "\n")
}

count_chars(orig_string)




