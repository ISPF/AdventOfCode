source("src/functions.R")
name <- "laurent"
year <- 2022
day <- 4

input <- aoc_get_inputfile(day, year, name)
dt <- data.table(inputs=unlist(str_split(input, "\n")))
dt <- dt[inputs!=""]
dt[, c("assignement1", "assignement2") := tstrsplit(inputs, ",")]
dt[, c("from1", "to1") := tstrsplit(assignement1, "-")]
dt[, c("from2", "to2") := tstrsplit(assignement2, "-")]

isFullyInAssignemnt <- function(from1, to1, from2, to2){
  list1 <- seq(from1, to1)
  list2 <- seq(from2, to2)
  len1 <- length(list1)
  len2 <- length(list2)

  if (len2<len1) {
    res <- setdiff(list2, list1)
  } else {
    res <- setdiff(list1, list2)
  }
  length(res)==0
}

dt[, isContained := mapply(isFullyInAssignemnt, from1, to1, from2, to2)]
dt[,sum(isContained)]

# section 2 ---------------------------------------------------------------

isOverlap <- function(from1, to1, from2, to2){
  list1 <- seq(from1, to1)
  list2 <- seq(from2, to2)
  length(intersect(list1, list2))>0
}

dt[, isOverlap := mapply(isOverlap, from1, to1, from2, to2)]
dt[,sum(isOverlap)]
