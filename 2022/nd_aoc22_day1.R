library(tidyverse)

#cheminAcces <- "C:/Users/ndubreu/Desktop/aoc22"
cheminAcces <- "E:/aoc22"

fichier <- paste(cheminAcces, "/day1.txt",
                 sep = "",
                 collapse = NULL)

day <- as.numeric(readLines(fichier)) 

value <- day

somme <- 0

sommeMax1 <- 0
sommeMax2 <- 0
sommeMax3 <- 0

for (currentIndex in 1:2255) {
  if (is.na(value[currentIndex])) {
    if (somme > sommeMax3){
      if (somme > sommeMax2){
        if (somme > sommeMax1){
          sommeMax3 <- sommeMax2
          sommeMax2 <- sommeMax1
          sommeMax1 <- somme
        }else{
          sommeMax3 <- sommeMax2
          sommeMax2 <- somme
        }
      }else{
        sommeMax3 <- somme
      }
    }
    somme <- 0
  } else {
    somme <- somme + value[currentIndex]
  }
}

# Star 1
sommeMax1

# Star 2
sommeMax1 + sommeMax2 + sommeMax3


