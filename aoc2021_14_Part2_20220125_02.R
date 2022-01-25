df1 <- tibble(
              KEY = c("BB", "BC", "BH", "BN", "CB", "CC", "CH", "CN", "HB", "HC", "HH", "HN", "NB", "NC", "NH", "NN"),
              RES1 = c(4, 1, 3, 1, 7, 8, 5, 6, 10, 9, 12, 10, 13, 13, 14, 14),
              RES2 = c(13, 2, 11, 4, 9, 14, 3, 8, 5, 2, 15, 8, 1, 2, 7, 8),
              ORIG = c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1),
              PRES = rep(0, 16)
              )
df

next_rep <- function(df) {
  for (i in 1:nrow(df)) {
    if (df$ORIG[i] > 0) {
      res1 <- df$RES1[i]
      res2 <- df$RES2[i]
      df$PRES[res1] <- df$PRES[res1] + df$ORIG[i]
      df$PRES[res2] <- df$PRES[res2] + df$ORIG[i]
    }
  }
  df$ORIG <- df$PRES
  df$PRES <- rep(0, nrow(df))
  df
}

for (j in 1:40) {
  df1 <- next_rep(df1)
}


df2 <- df1 %>%
  select(KEY, ORIG) %>%
  mutate(char1=substr(KEY,1,1),
         char2=substr(KEY,2,2)) %>%
  mutate(B=0, C=0, F=0, H=0, K=0, N=0, O=0, P=0, S=0, V=0)

df3 <- df2 %>%
  mutate(B=if_else(char1=="B",B+ORIG,B),
         B=if_else(char2=="B",B+ORIG,B),
         C=if_else(char1=="C",C+ORIG,C),
         C=if_else(char2=="C",C+ORIG,C),
         F=if_else(char1=="F",F+ORIG,F),
         F=if_else(char2=="F",F+ORIG,F),
         H=if_else(char1=="H",H+ORIG,H),
         H=if_else(char2=="H",H+ORIG,H),
         K=if_else(char1=="K",K+ORIG,K),
         K=if_else(char2=="K",K+ORIG,K),
         N=if_else(char1=="N",N+ORIG,N),
         N=if_else(char2=="N",N+ORIG,N),
         O=if_else(char1=="O",O+ORIG,O),
         O=if_else(char2=="O",O+ORIG,O),
         P=if_else(char1=="P",P+ORIG,P),
         P=if_else(char2=="P",P+ORIG,P),
         S=if_else(char1=="S",S+ORIG,S),
         S=if_else(char2=="S",S+ORIG,S),
         V=if_else(char1=="V",V+ORIG,V),
         V=if_else(char2=="V",V+ORIG,V))

df4 <- df3 %>%
  select(B, C, F, H, K, N, O, P, S, V) %>%
  mutate(B_count=ceiling(sum(B)/2),
         C_count=ceiling(sum(C)/2),
         F_count=ceiling(sum(F)/2),
         H_count=ceiling(sum(H)/2),
         K_count=ceiling(sum(K)/2),
         N_count=ceiling(sum(N)/2),
         O_count=ceiling(sum(O)/2),
         P_count=ceiling(sum(P)/2),
         S_count=ceiling(sum(S)/2),
         V_count=ceiling(sum(V)/2)) %>%
  select(B_count, C_count, F_count, H_count, K_count, N_count,
         O_count, P_count, S_count, V_count) %>%
  head(1)

options(digits = 16)
max(df4) - min(df4)
