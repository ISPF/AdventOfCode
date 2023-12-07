source("src/functions.R")
name <- "matthieu"
year <- 2023
day <- 1

input <- aoc_get_inputfile(day, year, name)

inputs <- unlist(str_split(input, "\n"))
listInputs <- lapply(inputs, FUN=function(x) as.integer(unlist(str_split(x,"\n"))))
sumlist <- sapply(listInputs, sum, na.rm=TRUE)
sorted_sumlist <- sort(sumlist,decreasing=TRUE)

# part 1
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

dt <- data.table(input=unlist(str_split(input, "\n")))
dt[, inputInt := as.numeric(gsub("\\D", "", input))]
dt[, firstDigit := substr(inputInt, 1, 1)]
dt[, lastDigit := substrRight(inputInt, 1)]

dt[, combinedDigits := as.integer(paste(firstDigit, lastDigit, sep = ""))]
dt <- dt[!is.na(combinedDigits)]
dt[, sum(combinedDigits)]

# part 2
dt <- data.table(input=unlist(str_split(input, "\n")))
dt[, new_Input := str_replace_all(input, "zero", "zero0zero")]
dt[, new_Input := str_replace_all(new_Input, "one", "one1one")]
dt[, new_Input := str_replace_all(new_Input, "two", "two2two")]
dt[, new_Input := str_replace_all(new_Input, "three", "three3three")]
dt[, new_Input := str_replace_all(new_Input, "four", "four4four")]
dt[, new_Input := str_replace_all(new_Input, "five", "five5five")]
dt[, new_Input := str_replace_all(new_Input, "six", "six6six")]
dt[, new_Input := str_replace_all(new_Input, "seven", "seven7seven")]
dt[, new_Input := str_replace_all(new_Input, "eight", "eight8eight")]
dt[, new_Input := str_replace_all(new_Input, "nine", "nine9nine")]

dt[, inputInt := as.numeric(gsub("\\D", "", new_Input))]
dt[, firstDigit := substr(inputInt, 1, 1)]
dt[, lastDigit := substrRight(inputInt, 1)]

dt[, combinedDigits := as.integer(paste(firstDigit, lastDigit, sep = ""))]
dt <- dt[!is.na(combinedDigits)]
dt[, sum(combinedDigits)]
