library(tidyverse)

#cheminAcces <- "C:/Users/ndubreu/Desktop/aoc22"
cheminAcces <- "E:/aoc22"

fichier <- paste(cheminAcces, "/day4.txt",
                 sep = "",
                 collapse = NULL)

day <- read_table(fichier,
                  col_names = FALSE) %>% separate(X1,  c("A1", "A2", "B1", "B2"))


## Star 1

move <- function(A1, A2, B1, B2) {
  pos <<-
    case_when((
      as.numeric(A1) <= as.numeric(B1) &&
        as.numeric(A2) >= as.numeric(B2)
    ) ~ (pos + 1),
    (
      as.numeric(B1) <= as.numeric(A1) &&
        as.numeric(B2) >= as.numeric(A2)
    ) ~ (pos + 1),
    TRUE ~ pos
    )
}

pos <- 0
pmap(day, move)
## 567

# Star 2
dayBis <-
  day %>% dplyr::mutate(pos = case_when((as.numeric(A2) < as.numeric(B1)) ~ 1,
                                        (as.numeric(B2) < as.numeric(A1)) ~ 1,
                                        TRUE ~ 0
  ))
dayBis %>% group_by(pos) %>% count()
# 907
