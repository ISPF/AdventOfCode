source("src/functions.R")
#input <- aoc_get_inputfile(12, 2022, "laurent")
input <- "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]"


input <- unlist(str_split(input, "\n\n"))
listInput <- lapply(input, FUN=function(x) unlist(str_split(x,"\n")))

listInput[[6]][2]
i <- 2
compare <- function(i){
  left <- listInput[[i]][1]
  right <- listInput[[i]][2]
  
  leftItems <- str_split(left, ",")
  rightItems <- str_split(right, ",")
  
  
  l1 <- gsub["["]
  
}
c <- "[1,[2,[3,[4,[5,6,7]]]],8,9]"
stringr::str_extract_all(c, "(?<=\\[).+?(?=\\])")


extractItems <- function(c){
  if (nchar(c)>=2){
    c <- substring(c,2, nchar(c)-1)
    items <- str_split(c, ",")
    
}



