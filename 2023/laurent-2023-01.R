source("src/functions.R")
name <- "laurent"
year <- 2023
day <- 1

input <- aoc_get_inputfile(day, year, name)
inputs <- unlist(str_split(input, "\n"))
inputs <- toupper(inputs)[1:1000]
listInputs <- lapply(inputs, FUN=function(x) gsub("[A-Z]", "", x))
prems <- lapply(listInputs, FUN=function(x) as.integer(paste0(substring(x,1,1), substring(x, nchar(x),nchar(x)))))
sumlist <- Reduce("+", prems)
