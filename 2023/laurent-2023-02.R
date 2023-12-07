source("src/functions.R")
library(stringr)

name <- "laurent"
year <- 2023
day <- 2

input <- aoc_get_inputfile(day, year, name)
inputs <- unlist(str_split(input, "\n"))
inputs <- inputs[1:100]

#  input <- "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
# Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
# Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
# Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
# Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
#  inputs <- unlist(str_split(input, "\n"))

MaxRED <- 12
MaxGREEN <- 13
MaxBLUE <- 14

dt <- data.table(input=inputs)
dt[,c("game", "input"):=tstrsplit(input,": ")]
dt[, game:=as.integer(gsub("Game ", "", game))]

getRGBValues <- function(x) {
  res <- c(
    as.numeric(gsub(" red"  , "", str_extract(x, "[[:digit:]]+ red"))),
    as.numeric(gsub(" green", "", str_extract(x, "[[:digit:]]+ green"))),
    as.numeric(gsub(" blue" , "", str_extract(x, "[[:digit:]]+ blue"))))
  res[is.na(res)] <- 0
  res
}

isGameValid <- function(gameNumber){
  #gameNumber <- 9
  i <- dt[gameNumber, input]
  j <- str_split(i,"; ")[[1]]
  k <- lapply(j, getRGBValues)
  l <- lapply(k, FUN=function(x){
    x[1]<=MaxRED & x[2]<=MaxGREEN & x[3]<=MaxBLUE
  })
  all(unlist(l))
}
dt$valid <- unlist(lapply(1:100, isGameValid))
dt[(valid), sum(game)]

getPowerValue <- function(gameNumber){
  #gameNumber <- 9
  i <- dt[gameNumber, input]
  j <- str_split(i,"; ")[[1]]
  k <- lapply(j, getRGBValues)
  dt2 <- as.data.table(t(as.data.table(k)))
  colnames(dt2) <- c("R", "G", "B")
  minValues <- apply(dt2,2,max)
  Reduce("*", minValues)
}

dt$power <- unlist(lapply(1:100, getPowerValue))
dt[, sum(power)]

