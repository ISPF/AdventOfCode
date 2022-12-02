source("src/functions.R")
name <- "laurent"
year <- 2022
day <- 1

input <- aoc_get_inputfile(day, year, name)

inputs <- unlist(str_split(input, "\n\n"))
listInputs <- lapply(inputs, FUN=function(x) as.integer(unlist(str_split(x,"\n"))))
sumlist <- sapply(listInputs, sum, na.rm=TRUE)
sorted_sumlist <- sort(sumlist,decreasing=TRUE)

#reponse 1
sorted_sumlist[1]
#reponse 2
sum(sorted_sumlist[1:3])
