source("src/functions.R")
name <- "Thierry"
year <- 2022
day <- 1

input <- aoc_get_inputfile(day, year, name)
inputs <- unlist(str_split(input, "\n\n"))
sumlist <- sort(unlist(lapply(lapply(inputs, FUN=function(x) unlist(str_split(x,"\n"))), FUN=function(x) sum(as.integer(unlist(x))))),decreasing=TRUE)

sumlist[1]
sum(unlist(sumlist[1:3]))


input2 <- data.table(inputs)

input2[, inputclean := lapply(inputs, FUN=function(x) unlist(str_split(x,"\n")))]
input2[, inputclean := type.convert(inputclean, as.is=TRUE)]
input2[,sommes := sapply(inputclean,sum,na.rm=T)]

input2 <- input2[order(sommes,decreasing=T)]
input2[,sommes]
input2[1,sommes]
sum(input2[1:3,sommes])
