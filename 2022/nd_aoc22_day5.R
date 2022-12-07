library(tidyverse)

#cheminAcces <- "C:/Users/ndubreu/Desktop/aoc22"
cheminAcces <- "E:/aoc22"

fichier <- paste(cheminAcces, "/day5.txt",
                 sep = "",
                 collapse = NULL)

day <- read_table(fichier,
                  col_names = FALSE) 

day1 <- subset(day %>% dplyr::mutate(nombre = X2 , depart = X4, arrivee = X6), select = c("nombre","depart","arrivee"))

vec1 <- c("Q","W","P","Z","Z","R","H","D")
vec2 <- c("V","B","R","W","Q","H","F")
vec3 <- c("C","V","S","H")
vec4 <- c("H","F","G")
vec5 <- c("P","G","J","B","Z")
vec6 <- c("Q","T","J","H","W","F","L")
vec7 <- c("Z","T","W","D","L","V","J","N")
vec8 <- c("D","T","Z","C","J","G","H","F")
vec9 <- c("W","P","V","M","B","H")


## Star 1

liste <- list(vec1, vec2, vec3, vec4, vec5, vec6, vec7, vec8, vec9)

move <- function(nombre,depart,arrivee) {
  
  vecDepart <- liste[[depart]]
  longueurVecDepart <- length(vecDepart)
  vecArrive <- liste[[arrivee]]
  
  for (i in 1:nombre) {
    vecArrive <-
      append(vecArrive, vecDepart[longueurVecDepart - i + 1])
  }
  
  for (i in 1:nombre) {
    vecDepart <-  vecDepart[-(longueurVecDepart - i + 1)]
  }
  
  liste[[depart]] <- vecDepart 
  liste[[arrivee]] <- vecArrive
  
  liste <<- list(liste[[1]], liste[[2]], liste[[3]], liste[[4]], liste[[5]], liste[[6]], liste[[7]], liste[[8]], liste[[9]])
}

pmap(day1,move)

for (i in 1:9){
  print(liste[[i]][length(liste[[i]])])
}

########################################

# Star 2

liste <- list(vec1, vec2, vec3, vec4, vec5, vec6, vec7, vec8, vec9)

move2 <- function(nombre,depart,arrivee) {
  
  vecDepart <- liste[[depart]]
  longueurVecDepart <- length(vecDepart)
  vecArrive <- liste[[arrivee]]
  
  for (i in 1:nombre) {
    vecArrive <-
      append(vecArrive, vecDepart[longueurVecDepart - nombre + i])
  }
  
  for (i in 1:nombre) {
    vecDepart <-  vecDepart[-(longueurVecDepart - i + 1)]
  }
  
  liste[[depart]] <- vecDepart 
  liste[[arrivee]] <- vecArrive
  
  liste <<- list(liste[[1]], liste[[2]], liste[[3]], liste[[4]], liste[[5]], liste[[6]], liste[[7]], liste[[8]], liste[[9]])
}

pmap(day1,move2)

for (i in 1:9){
  print(liste[[i]][length(liste[[i]])])
}


