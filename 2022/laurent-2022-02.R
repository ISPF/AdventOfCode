source("src/functions.R")
name <- "laurent"
year <- 2022
day <- 2
input <- aoc_get_inputfile(day, year, name)

points <- 1:3
names(points) <- c("Rock", "Paper", "Scissor")

getName <- function(i) {
 fcase(i=='A', "Rock",
       i=='X', "Rock",
       i=='B', "Paper",
       i=='Y', "Paper",
       i=='C', "Scissor",
       i=='Z', "Scissor")
}

getResultGame <- function(i,j) {
  fcase(i==j,3,
        i=="Rock"    & j=="Paper",   6,
        i=="Rock"    & j=="Scissor", 0,
        i=="Paper"   & j=="Rock",    0,
        i=="Paper"   & j=="Scissor", 6,
        i=="Scissor" & j=="Rock",    6,
        i=="Scissor" & j=="Paper",   0)
}

dt <- data.table(input=unlist(str_split(input, "\n")))
dt <- dt[input!=""]
dt[,c("lui", "moi") := tstrsplit(input, " ")]
dt[,luiString:=getName(lui)]
dt[,moiString:=getName(moi)]
dt[,score1.1:=points[moiString]]
dt[,score1.2:=getResultGame(luiString, moiString)]
dt[,score1:=score1.1+score1.2]
dt[,sum(score1)]
