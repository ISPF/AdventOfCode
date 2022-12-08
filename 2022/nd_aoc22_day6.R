library(tidyverse)

#cheminAcces <- "C:/Users/ndubreu/Desktop/aoc22"
cheminAcces <- "E:/aoc22"

fichier <- paste(cheminAcces, "/day6.txt",
                 sep = "",
                 collapse = NULL)

day <- readLines(fichier) %>% unlist


## Star 1

day <- day[1]

longueur <- nchar(day[1]) - 3

for (i in 1:longueur){
  if (substring(day, i+3, i+3) != substring(day, i, i)
      & substring(day, i+3, i+3) != substring(day, i+1, i+1)
      & substring(day, i+3, i+3) != substring(day, i+2, i+2)
      & substring(day, i+2, i+2) != substring(day, i+1, i+1)
      & substring(day, i+2, i+2) != substring(day, i, i)
      & substring(day, i+1, i+1) != substring(day, i, i)
  ){
    print(i + 3)
    break
  }
}
# 1766

#########################################

## Star 2

premier <- FALSE

for (i in 0:longueur) {
  doublon <- FALSE
  for (j in 1:14) {
    for (k in 1:14) {
      if (j != k & substring(day, i+j, i+j) == substring(day, i+k, i+k)) {
        doublon <<- TRUE
        break
      }
    }
  }
  if (!premier & !doublon ) {
    print(i + 14)
    premier <<- TRUE
  }
}



