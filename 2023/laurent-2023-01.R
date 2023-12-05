source("src/functions.R")
name <- "laurent"
year <- 2023
day <- 1

input <- aoc_get_inputfile(day, year, name)
inputs <- unlist(str_split(input, "\n"))
inputs <- inputs[1:1000]
listInputs <- lapply(inputs, FUN=function(x) gsub("[a-z]", "", x))
prems <- lapply(listInputs, FUN=function(x) as.integer(paste0(substring(x,1,1), substring(x, nchar(x),nchar(x)))))
sumlist <- Reduce("+", prems)
sumlist

# inputs <- read_lines("two1nine
# eightwothree
# abcone2threexyz
# xtwone3four
# 4nineeightseven2
# zoneight234
# 7pqrstsixteen")

inputs2 <- copy(inputs)
inputs2 <- gsub("one","one1one",inputs2)
inputs2 <- gsub("two","two2two",inputs2)
inputs2 <- gsub("three","three3three",inputs2)
inputs2 <- gsub("four","four4four",inputs2)
inputs2 <- gsub("five","five5five",inputs2)
inputs2 <- gsub("six","six6six",inputs2)
inputs2 <- gsub("seven","seven7seven",inputs2)
inputs2 <- gsub("eight","eight8eight",inputs2)
inputs2 <- gsub("nine","nine9nine",inputs2)

listInputs2 <- lapply(inputs2, FUN=function(x) gsub("[a-z]", "", x))
prems2 <- lapply(listInputs2, FUN=function(x) as.integer(paste0(substring(x,1,1), substring(x, nchar(x),nchar(x)))))
sumlist2 <- Reduce("+", prems2)
sumlist2
