library(tidyverse)

#cheminAcces <- "C:/Users/ndubreu/Desktop/aoc22"
cheminAcces <- "E:/aoc22"

fichier <- paste(cheminAcces, "/day5.txt",
                 sep = "",
                 collapse = NULL)

day <- read_table(fichier,
                  col_names = FALSE) 

day1 <- subset(day %>% dplyr::mutate(nombre = X2 , depart = X4, arrivee = X6), select = c("nombre","depart","arrivee"))


## Star 1

vec1 <- c("Q","W","P","Z","Z","R","H","D")
vec2 <- c("V","B","R","W","Q","H","F")
vec3 <- c("C","V","S","H")
vec4 <- c("H","F","G")
vec5 <- c("P","G","J","B","Z")
vec6 <- c("Q","T","J","H","W","F","L")
vec7 <- c("Z","T","W","D","L","V","J","N")
vec8 <- c("D","T","Z","C","J","G","H","F")
vec9 <- c("W","P","V","M","B","H")

move <- function(nombre,depart,arrivee) {
  
  if (depart == 1) {
    vecDepart <- vec1
  } else if (depart == 2) {
    vecDepart <- vec2
  } else if (depart == 3) {
    vecDepart <- vec3
  } else if (depart == 4) {
    vecDepart <- vec4
  } else if (depart == 5) {
    vecDepart <- vec5
  } else if (depart == 6) {
    vecDepart <- vec6
  } else if (depart == 7) {
    vecDepart <- vec7
  } else if (depart == 8) {
    vecDepart <- vec8
  } else if (depart == 9) {
    vecDepart <- vec9
  } 
  
  longueurVecDepart <- length(vecDepart)
  
  if (arrivee == 1) {
    for (i in 1:nombre) {
      vec1 <<- append(vec1, vecDepart[longueurVecDepart - i + 1])
    }
  } else if (arrivee == 2) {
    for (i in 1:nombre) {
      vec2 <<- append(vec2, vecDepart[longueurVecDepart - i + 1])
    }
  } else if (arrivee == 3) {
    for (i in 1:nombre) {
      vec3 <<- append(vec3, vecDepart[longueurVecDepart - i + 1])
    }
  } else if (arrivee == 4) {
    for (i in 1:nombre) {
      vec4 <<- append(vec4, vecDepart[longueurVecDepart - i + 1])
    }
  } else if (arrivee == 5) {
    for (i in 1:nombre) {
      vec5 <<- append(vec5, vecDepart[longueurVecDepart - i + 1])
    }
  } else if (arrivee == 6) {
    for (i in 1:nombre) {
      vec6 <<- append(vec6, vecDepart[longueurVecDepart - i + 1])
    }
  } else if (arrivee == 7) {
    for (i in 1:nombre) {
      vec7 <<- append(vec7, vecDepart[longueurVecDepart - i + 1])
    }
  } else if (arrivee == 8) {
    for (i in 1:nombre) {
      vec8 <<- append(vec8, vecDepart[longueurVecDepart - i + 1])
    }
  } else if (arrivee == 9) {
    for (i in 1:nombre) {
      vec9 <<- append(vec9, vecDepart[longueurVecDepart - i + 1])
    }
  }
  
  if (depart == 1) {
    for (i in 1:nombre) {
      vec1 <<- vec1[-(longueurVecDepart - i + 1)]
    }
  } else if (depart == 2) {
    for (i in 1:nombre) {
      vec2 <<- vec2[-(longueurVecDepart - i + 1)]
    }
  } else if (depart == 3) {
    for (i in 1:nombre) {
      vec3 <<- vec3[-(longueurVecDepart - i + 1)]
    }
  } else if (depart == 4) {
    for (i in 1:nombre) {
      vec4 <<- vec4[-(longueurVecDepart - i + 1)]
    }
  } else if (depart == 5) {
    for (i in 1:nombre) {
      vec5 <<- vec5[-(longueurVecDepart - i + 1)]
    }
  } else if (depart == 6) {
    for (i in 1:nombre) {
      vec6 <<- vec6[-(longueurVecDepart - i + 1)]
    }
  } else if (depart == 7) {
    for (i in 1:nombre) {
      vec7 <<- vec7[-(longueurVecDepart - i + 1)]
    }
  } else if (depart == 8) {
    for (i in 1:nombre) {
      vec8 <<- vec8[-(longueurVecDepart - i + 1)]
    }
  } else if (depart == 9) {
    for (i in 1:nombre) {
      vec9 <<- vec9[-(longueurVecDepart - i + 1)]
    }
  } 

}

pmap(day1,move)

print(vec1[length(vec1)])
print(vec2[length(vec2)])
print(vec3[length(vec3)])
print(vec4[length(vec4)])
print(vec5[length(vec5)])
print(vec6[length(vec6)])
print(vec7[length(vec7)])
print(vec8[length(vec8)])
print(vec9[length(vec9)])

########################################

# Star 2

vec1 <- c("Q","W","P","Z","Z","R","H","D")
vec2 <- c("V","B","R","W","Q","H","F")
vec3 <- c("C","V","S","H")
vec4 <- c("H","F","G")
vec5 <- c("P","G","J","B","Z")
vec6 <- c("Q","T","J","H","W","F","L")
vec7 <- c("Z","T","W","D","L","V","J","N")
vec8 <- c("D","T","Z","C","J","G","H","F")
vec9 <- c("W","P","V","M","B","H")

move2 <- function(nombre,depart,arrivee) {
  
  if (depart == 1) {
    vecDepart <- vec1
  } else if (depart == 2) {
    vecDepart <- vec2
  } else if (depart == 3) {
    vecDepart <- vec3
  } else if (depart == 4) {
    vecDepart <- vec4
  } else if (depart == 5) {
    vecDepart <- vec5
  } else if (depart == 6) {
    vecDepart <- vec6
  } else if (depart == 7) {
    vecDepart <- vec7
  } else if (depart == 8) {
    vecDepart <- vec8
  } else if (depart == 9) {
    vecDepart <- vec9
  } 
  
  longueurVecDepart <- length(vecDepart)
  
  if (arrivee == 1) {
    for (i in 1:nombre) {
      vec1 <<- append(vec1, vecDepart[longueurVecDepart - nombre + i])
    }
  } else if (arrivee == 2) {
    for (i in 1:nombre) {
      vec2 <<- append(vec2, vecDepart[longueurVecDepart - nombre + i])
    }
  } else if (arrivee == 3) {
    for (i in 1:nombre) {
      vec3 <<- append(vec3, vecDepart[longueurVecDepart - nombre + i])
    }
  } else if (arrivee == 4) {
    for (i in 1:nombre) {
      vec4 <<- append(vec4, vecDepart[longueurVecDepart - nombre + i])
    }
  } else if (arrivee == 5) {
    for (i in 1:nombre) {
      vec5 <<- append(vec5, vecDepart[longueurVecDepart - nombre + i])
    }
  } else if (arrivee == 6) {
    for (i in 1:nombre) {
      vec6 <<- append(vec6, vecDepart[longueurVecDepart - nombre + i])
    }
  } else if (arrivee == 7) {
    for (i in 1:nombre) {
      vec7 <<- append(vec7, vecDepart[longueurVecDepart - nombre + i])
    }
  } else if (arrivee == 8) {
    for (i in 1:nombre) {
      vec8 <<- append(vec8, vecDepart[longueurVecDepart - nombre + i])
    }
  } else if (arrivee == 9) {
    for (i in 1:nombre) {
      vec9 <<- append(vec9, vecDepart[longueurVecDepart - nombre + i])
    }
  }
  
  if (depart == 1) {
    for (i in 1:nombre) {
      vec1 <<- vec1[-(longueurVecDepart - i + 1)]
    }
  } else if (depart == 2) {
    for (i in 1:nombre) {
      vec2 <<- vec2[-(longueurVecDepart - i + 1)]
    }
  } else if (depart == 3) {
    for (i in 1:nombre) {
      vec3 <<- vec3[-(longueurVecDepart - i + 1)]
    }
  } else if (depart == 4) {
    for (i in 1:nombre) {
      vec4 <<- vec4[-(longueurVecDepart - i + 1)]
    }
  } else if (depart == 5) {
    for (i in 1:nombre) {
      vec5 <<- vec5[-(longueurVecDepart - i + 1)]
    }
  } else if (depart == 6) {
    for (i in 1:nombre) {
      vec6 <<- vec6[-(longueurVecDepart - i + 1)]
    }
  } else if (depart == 7) {
    for (i in 1:nombre) {
      vec7 <<- vec7[-(longueurVecDepart - i + 1)]
    }
  } else if (depart == 8) {
    for (i in 1:nombre) {
      vec8 <<- vec8[-(longueurVecDepart - i + 1)]
    }
  } else if (depart == 9) {
    for (i in 1:nombre) {
      vec9 <<- vec9[-(longueurVecDepart - i + 1)]
    }
  } 
  
}

pmap(day1,move2)
## 567

print(vec1[length(vec1)])
print(vec2[length(vec2)])
print(vec3[length(vec3)])
print(vec4[length(vec4)])
print(vec5[length(vec5)])
print(vec6[length(vec6)])
print(vec7[length(vec7)])
print(vec8[length(vec8)])
print(vec9[length(vec9)])

