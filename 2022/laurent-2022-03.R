source("src/functions.R")
name <- "laurent"
year <- 2022
day <- 3

input <- aoc_get_inputfile(day, year, name)

dt <- data.table(input=unlist(str_split(input, "\n")))
dt <- dt[input!=""]
dt[,compartment1:=substring(input, 1, nchar(input)/2)]
dt[,compartment2:=substring(input, nchar(input)/2+1, nchar(input))]

getIntersect <- function(a, b){
  paste(intersect(strsplit(a, "")[[1]],
                  strsplit(b, "")[[1]]), collapse = '')
}

dt[, inters:=mapply(getIntersect, compartment1, compartment2)]

points <- c(1:52)
names(points) <- c(tolower(LETTERS), LETTERS)

dt[,pts:=points[inters]]
dt[, sum(pts)]

# Section 2 ---------------------------------------------------------------

dt[, grp:=(.I+2) %/% 3]

getIntersect3 <- function(a, b, c){
  intersect(
    intersect(strsplit(a, "")[[1]],
              strsplit(b, "")[[1]]),
    strsplit(c, "")[[1]])
}

res <- lapply(1:100, FUN=function(x) {
  dt2 <- dt[grp==x]
  a <- as.character(dt2[1, input])
  b <- as.character(dt2[2, input])
  c <- as.character(dt2[3, input])
  getIntersect3(a,b,c)
})

res <- unlist(res)
sum(points[res])
