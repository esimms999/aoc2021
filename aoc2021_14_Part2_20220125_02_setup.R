df1 <- tibble(
  KEY = c("BB", "BC", "BH", "BN", "CB", "CC", "CH", "CN", "HB", "HC", "HH", "HN", "NB", "NC", "NH", "NN"),
  RES1 = c(4, 1, 3, 1, 7, 8, 5, 6, 10, 9, 12, 10, 13, 13, 14, 14),
  RES2 = c(13, 2, 11, 4, 9, 14, 3, 8, 5, 2, 15, 8, 1, 2, 7, 8),
  ORIG = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1),
  PRES = rep(0, 16)
)
df1
orig_string <- "PHVCVBFHCVPFKBNHKNBO"
aoc2021_14_1 <- read_csv("data/aoc2021_14_1.txt", show_col_types = FALSE)

df11 <- aoc2021_14_1 %>%
  arrange(KEY) %>%
  mutate(ID=row_number(),
         RES1_char=paste(substr(KEY,1,1), NEW_CHAR, sep=""),
         RES2_char=paste(NEW_CHAR, substr(KEY,2,2), sep="")) %>%
  mutate(RES1=0,
         RES2=0) %>%
  select(ID, KEY, RES1_char, RES2_char, RES1, RES2)
df11

for (i in 1:nrow(df11)) {
  for (j in 1:nrow(df11)) {
    if (df11$RES1_char[i]==df11$KEY[j]) {
      df11$RES1[i]=df11$ID[j]
    }
    if (df11$RES2_char[i]==df11$KEY[j]) {
      df11$RES2[i]=df11$ID[j]
    }
  }
}
df11

df12 <- df11 %>%
  select(-ID) %>%
  mutate(PRES=0)
df12

ORIG <- c(0,0,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,2,
          0,0,0,1,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,
          1,0,0,0,0,1,0,0,0,0,1,0,0,1,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,
          0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,1,0,0)
df12$ORIG <-ORIG
df12 <- df12 %>%
  select(-RES1_char, -RES2_char)
df12
df1 <-df12
