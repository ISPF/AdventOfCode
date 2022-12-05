source("src/functions.R")
input <- aoc_get_inputfile(5, 2022, "laurent")
inputs <- str_split_1(input, "\n")
inputs <- inputs[inputs!=""]

dt <- data.table(inputs=inputs)
moves <- dt[10:nrow(dt)]
#stacks <- dt[1:8]
stacks <- fread("2022/input/laurent-2022-5.stacks.csv")
#fwrite(stacks, "stacks.csv")

moves[,c("nb", "from") := tstrsplit(inputs, " from ")]
moves[,c("from", "to") := tstrsplit(from, " to ")]
moves[,nb:=gsub("move ", "", nb)]
moves[, nb:=as.integer(nb)]
moves[, from:=as.integer(from)]
moves[, to:=as.integer(to)]

getStackAndReverse <- function(stackNumber, n) {
  stack <- get(paste0("stack", stackNumber))
  # etape 1
  #rev(stack[1:n])
  # etape 2
  stack[1:n]

}

stack1 <- stri_remove_empty(stacks[,V1])
stack2 <- stri_remove_empty(stacks[,V2])
stack3 <- stri_remove_empty(stacks[,V3])
stack4 <- stri_remove_empty(stacks[,V4])
stack5 <- stri_remove_empty(stacks[,V5])
stack6 <- stri_remove_empty(stacks[,V6])
stack7 <- stri_remove_empty(stacks[,V7])
stack8 <- stri_remove_empty(stacks[,V8])
stack9 <- stri_remove_empty(stacks[,V9])

for(i in 1:nrow(moves)){
  nb <- as.integer(moves[i, nb])
  from <- moves[i, from]
  to <- moves[i, to]
  stackFrom <- get(paste0("stack", from))
  stackTo <- get(paste0("stack", to))
  
  newStackTo <- c(getStackAndReverse(from, nb), stackTo)
  newStackFrom <- stackFrom[seq(nb+1,length(stackFrom))]
  
  assign(paste0("stack", from), newStackFrom, envir = .GlobalEnv)
  assign(paste0("stack", to), newStackTo, envir = .GlobalEnv)
}

paste(c(
  stack1[1],
  stack2[1],
  stack3[1],
  stack4[1],
  stack5[1],
  stack6[1],
  stack7[1],
  stack8[1],
  stack9[1]), collapse = "")

