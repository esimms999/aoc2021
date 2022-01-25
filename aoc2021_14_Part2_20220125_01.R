df1 <- tibble(
              KEY = c("BB", "BC", "BH", "BN", "CB", "CC", "CH", "CN", "HB", "HC", "HH", "HN", "NB", "NC", "NH", "NN"),
              RES1 = c(4, 1, 3, 1, 7, 8, 5, 6, 10, 9, 12, 10, 13, 13, 14, 14),
              RES2 = c(13, 2, 11, 4, 9, 14, 3, 8, 5, 2, 15, 8, 1, 2, 7, 8),
              ORIG = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1),
              PRES = rep(0, 16)
              )
df1

next_rep <- function(df) {
  for (i in 1:16) {
    if (df$ORIG[i] > 0) {
      res1 <- df$RES1[i]
      res2 <- df$RES2[i]
      df$PRES[res1] <- df$PRES[res1] + df$ORIG[i]
      df$PRES[res2] <- df$PRES[res2] + df$ORIG[i]
    }
  }
  df$ORIG <- df$PRES
  df$PRES <- rep(0, 16)
  df
}

for (j in 1:40) {
  df1 <- next_rep(df1)
}


df2 <- df1 %>%
  select(KEY, ORIG) %>%
  mutate(char1=substr(KEY,1,1),
         char2=substr(KEY,2,2)) %>%
  mutate(B=0, C=0, H=0, N=0)

df3 <- df2 %>%
  mutate(B=if_else(char1=="B",B+ORIG,B),
         B=if_else(char2=="B",B+ORIG,B),
         C=if_else(char1=="C",C+ORIG,C),
         C=if_else(char2=="C",C+ORIG,C),
         H=if_else(char1=="H",H+ORIG,H),
         H=if_else(char2=="H",H+ORIG,H),
         N=if_else(char1=="N",N+ORIG,N),
         N=if_else(char2=="N",N+ORIG,N))

df4 <- df3 %>%
  select(B, C, H, N) %>%
  mutate(B_count=ceiling(sum(B)/2),
         C_count=ceiling(sum(C)/2),
         H_count=ceiling(sum(H)/2),
         N_count=ceiling(sum(N)/2)) %>%
  select(B_count, C_count, H_count, N_count) %>%
  head(1)

options(digits = 16)
max(df4) - min(df4)
