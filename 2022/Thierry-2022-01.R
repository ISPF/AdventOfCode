source("src/functions.R")
name <- "Thierry"
year <- 2022
day <- 1

input <- aoc_get_inputfile(day, year, name)
inputs <- unlist(str_split(input, "\n\n"))
sumlist <- sort(unlist(lapply(lapply(inputs, FUN=function(x) unlist(str_split(x,"\n"))), FUN=function(x) sum(as.integer(unlist(x))))),decreasing=TRUE)

sumlist[1]
sum(unlist(sumlist[1:3]))
