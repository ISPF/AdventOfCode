source("src/functions.R")
name <- "laurent"
year <- 2020
day <- 22

input <- aoc_get_inputfile(day, year, name)

input <- unlist(str_split(input, "\n\n"))
names(input) <- c("Player 1", "Player 2")

list1 <- unlist(str_split(input[1], "\n"))
list2 <- unlist(str_split(input[2], "\n"))
list1 <- list1[-1]
list2 <- list2[-1]
list2 <- list2[-26]