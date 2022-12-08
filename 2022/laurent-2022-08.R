source("src/functions.R")
input <- read_lines(aoc_get_inputfile(8, 2022, "laurent"))

# input <- read_lines(
# "30373
# 25512
# 65332
# 33549
# 35390")

dt <- as.data.table(str_split(input,""))
dt <- dt[, lapply(.SD, as.integer)]
dt <- as.data.table(t(dt))
colnames(dt) <- row.names(dt)
n <- nrow(dt)

isVisible <- function(i,j) {
  val <- dt[i,..j]
  colsW<-seq(1,j-1)
  colsE<-seq(j+1,n)
  listW <- dt[i,..colsW]
  listE <- dt[i,..colsE]
  listN <- dt[seq(1,i-1),..j]
  listS <- dt[seq(i+1,n),..j]
  res <- val>max(listW) | val>max(listE) | val>max(listN) | val>max(listS)
  #cat(sprintf("%s %s, val %s : %s\n", i,j, val,res))
  res
}

visTrees <- 2*(n+n-2)
for (i in seq(2,n-1))
  for (j in seq(2,n-1))
    if(isVisible(i,j))
      visTrees <- visTrees+1
visTrees


# Section 2 ---------------------------------------------------------------
# input <- read_lines(
# "30373
# 25512
# 65332
# 33549
# 35390")
dt <- as.data.table(str_split(input,""))
dt <- dt[, lapply(.SD, as.integer)]
dt <- as.data.table(t(dt))
colnames(dt) <- row.names(dt)
n <- nrow(dt)


getScenicFromList <- function(x, lst) {
  res <- 0
  i <- 1
  while(i<=length(lst)) {
    if(x<=lst[i]) {
      res <- res+1
      break
    } 
    if (x>lst[i])
      res <- res+1
    i <- i+1
  }
  res
}

getScenic <- function(i,j) {
  val <- dt[i,..j]
  colsW<-seq(1,j-1)
  colsE<-seq(j+1,n)
  listW <- unlist(rev(dt[i,..colsW]))
  listE <- unlist(dt[i,..colsE])
  listN <- unlist(dt[seq(i-1,1),..j])
  listS <- unlist(dt[seq(i+1,n),..j])
  
  res <- 
    getScenicFromList(val,listW)*
    getScenicFromList(val,listE)*
    getScenicFromList(val,listN)*
    getScenicFromList(val,listS)

  res
}

maxScenic <- 0
for (i in seq(2,n-1)) {
  for (j in seq(2,n-1)) {
    a <- getScenic(i,j)
    if(a>maxScenic)
      maxScenic <- a
  }
}

maxScenic

