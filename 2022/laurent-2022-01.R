source("src/functions.R")
name <- "laurent"
year <- 2022
day <- 1

input <- aoc_get_inputfile(day, year, name)

inputs <- unlist(str_split(input, "\n\n"))
listInputs <- lapply(inputs, FUN=function(x) unlist(str_split(x,"\n")))
sumlist <- lapply(listInputs, FUN=function(x) sum(as.integer(x[[1]])))
indiceMax <- which.max(sumlist)
sumlist[indiceMax]