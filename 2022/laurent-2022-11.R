source("src/functions.R")
input <- aoc_get_inputfile(11, 2022, "laurent")

# input="Monkey 0:
#   Starting items: 79, 98
#   Operation: new = old * 19
#   Test: divisible by 23
#     If true: throw to monkey 2
#     If false: throw to monkey 3
# 
# Monkey 1:
#   Starting items: 54, 65, 75, 74
#   Operation: new = old + 6
#   Test: divisible by 19
#     If true: throw to monkey 2
#     If false: throw to monkey 0
# 
# Monkey 2:
#   Starting items: 79, 60, 97
#   Operation: new = old * old
#   Test: divisible by 13
#     If true: throw to monkey 1
#     If false: throw to monkey 3
# 
# Monkey 3:
#   Starting items: 74
#   Operation: new = old + 3
#   Test: divisible by 17
#     If true: throw to monkey 0
#     If false: throw to monkey 1"

inputs <- unlist(str_split(input, "\n\n"))
listInputs <- lapply(inputs, FUN=function(x) unlist(str_split(x,"\n")))

splitInputMonkey <- function(i){
  monk <- listInputs[i][[1]]
  lstItems <- as.integer(str_split(gsub(" ", "", gsub("  Starting items: ", "",monk[2])), ',')[[1]])
  operation <- gsub("  Operation: new = ", "",monk[3])
  diviseur <- as.integer(gsub("  Test: divisible by ", "",monk[4]))
  nextMonkeyIfTrue <-  as.integer(gsub("    If true: throw to monkey ", "",monk[5]))
  nextMonkeyIfFalse <- as.integer(gsub("    If false: throw to monkey ", "",monk[6]))
  c("IDMonkey"=i-1,
    "diviseur"=diviseur,
    "op"=operation,
    "nextMonkeyIfTrue"=nextMonkeyIfTrue,
    "nextMonkeyIfFalse"=nextMonkeyIfFalse,
    "items"=list(lstItems))
}
dt <- rbindlist(lapply(1:length(listInputs), splitInputMonkey))
dtInit <- unique(dt[,.(IDMonkey, diviseur, op, nextMonkeyIfTrue, nextMonkeyIfFalse)])

getNextMonkeyAndVal <- function(id, val){
  dtEval <- copy(dtInit[IDMonkey==id])
  old <- as.numeric(val)
  op <- as.character(dtEval[, op])
  new <- eval(parse(text = op))
  worryLvl <- new %/% 3
  dtEval[, worryLvl:=worryLvl]
  dtEval[, nextMonkey:=ifelse(worryLvl%%diviseur==0,nextMonkeyIfTrue,nextMonkeyIfFalse)]
  l <-  as.list(dtEval[, .(worryLvl, nextMonkey)])
  l
}

nbMonkeys <- length(listInputs)
lst <- list()
for(i in seq(0,nbMonkeys-1))
  lst[i+1] <- list(dt[IDMonkey==i, items])
lstInspect <- rep(0,nbMonkeys)

for(k in 1:20){
  cat(sprintf("\nBoucle %s : ",k))
  for(i in 1:nbMonkeys){
    for(j in seq_along(lst[[i]])){
      l <- getNextMonkeyAndVal(i-1, lst[[i]][j])
      worry <- l$worryLvl
      next_m <- l$nextMonkey
      lst[[next_m+1]] <- c(lst[[next_m+1]],worry)
      lstInspect[i] <- lstInspect[i]+1
    }
    lst[[i]] <- list()
  }
}

lstInspect <- lstInspect[order(lstInspect, decreasing = T)]
lstInspect[1]*lstInspect[2]


