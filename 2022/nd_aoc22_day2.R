library(tidyverse)

#cheminAcces <- "C:/Users/ndubreu/Desktop/aoc22"
cheminAcces <- "E:/aoc22"

fichier <- paste(cheminAcces, "/day2.txt",
                 sep = "",
                 collapse = NULL)

day <- read_table(fichier,
                  col_names = FALSE)

## Star 1

move<-function(X1,X2){
  pos<<-case_when(
    (X1=="A" & X2=="Y") ~ (pos + 2 + 6),
    (X1=="A" & X2=="X")  ~ (pos + 1 + 3),
    (X1=="A" & X2=="Z") ~ (pos + 3 + 0),
    (X1=="B" & X2=="X") ~ (pos + 1 + 0),
    (X1=="B" & X2=="Y")  ~ (pos + 2 + 3),
    (X1=="B" & X2=="Z") ~ (pos + 3 + 6),
    (X1=="C" & X2=="X") ~ (pos + 1 + 6),
    (X1=="C" & X2=="Y")  ~ (pos + 2 + 0),
    (X1=="C" & X2=="Z") ~ (pos + 3 + 3),
    TRUE ~ pos)
}

pos <- 0
pmap(day,move)

print(pos)
# 14069

## Star 2

dayBis <- day %>% dplyr::mutate(X3=case_when(
  (X1=="A" & X2=="Y") ~ "X",
  (X1=="A" & X2=="X")  ~ "Z",
  (X1=="A" & X2=="Z") ~ "Y",
  (X1=="B" & X2=="X") ~ "X",
  (X1=="B" & X2=="Y")  ~ "Y",
  (X1=="B" & X2=="Z") ~ "Z",
  (X1=="C" & X2=="X") ~ "Y",
  (X1=="C" & X2=="Y")  ~ "Z",
  (X1=="C" & X2=="Z") ~ "X",
  TRUE ~ "?"))

move2<-function(X1,X2,X3){
  pos<<-case_when(
    (X1=="A" & X3=="Y") ~ (pos + 2 + 6),
    (X1=="A" & X3=="X")  ~ (pos + 1 + 3),
    (X1=="A" & X3=="Z") ~ (pos + 3 + 0),
    (X1=="B" & X3=="X") ~ (pos + 1 + 0),
    (X1=="B" & X3=="Y")  ~ (pos + 2 + 3),
    (X1=="B" & X3=="Z") ~ (pos + 3 + 6),
    (X1=="C" & X3=="X") ~ (pos + 1 + 6),
    (X1=="C" & X3=="Y")  ~ (pos + 2 + 0),
    (X1=="C" & X3=="Z") ~ (pos + 3 + 3),
    TRUE ~ pos)
}

pos <- 0
pmap(dayBis,move2)

print(pos)
#12411
