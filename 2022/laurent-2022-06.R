source("src/functions.R")
name <- "laurent"
year <- 2022
day <- 6

input <- aoc_get_inputfile(day, year, name)
input <- gsub("\\n", "", input)
l <- strsplit(input, "")[[1]]

for(i in seq(1,length(l)-4)) {
  if (length(unique(l[seq(i,i+3)]))==4) {
    cat(sprintf("%s\n",i+3))
    break
  }
}

## section 2
for(i in seq(1,length(l)-14)) {
  if (length(unique(l[seq(i,i+13)]))==14){
    cat(sprintf("%s\n",i+13))
    break
    }
}