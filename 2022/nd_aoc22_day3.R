library(tidyverse)

#cheminAcces <- "C:/Users/ndubreu/Desktop/aoc22"
cheminAcces <- "E:/aoc22"

fichier <- paste(cheminAcces, "/day3.txt",
                 sep = "",
                 collapse = NULL)

## Star 1

day <- readLines(fichier)

ensembleLettres <- c(letters, LETTERS)

result <- 0
for (numLigne in 1:300) {
  X1 <- day[numLigne]
  print(numLigne)
  premier <- substr(X1, 0, nchar(X1) / 2)
  deuxieme <- substr(X1, nchar(X1) / 2 + 1, nchar(X1))
  nbCaracteres <- nchar(X1) / 2
  i <- 1
  for (i in 1:nbCaracteres) {
    if (grepl(substring(deuxieme, i, i), premier)) {
      result <- result + match(substring(deuxieme, i, i), ensembleLettres)
      break
    }
  }
}

print(result)

## Star 2

result <- 0
for (numGroupe in 1:100) {
  premier <- day[numGroupe * 3]
  deuxieme <- day[numGroupe * 3 - 1]
  troisieme <- day[numGroupe * 3 - 2]
  nbCaracteres <- nchar(deuxieme)
  i <- 1
  for (i in 1:nbCaracteres) {
    if (grepl(substring(deuxieme, i, i), premier) &
        grepl(substring(deuxieme, i, i), troisieme)) {
      result <- result + match(substring(deuxieme, i, i), ensembleLettres)
      break
    }
  }
}

print(result)
