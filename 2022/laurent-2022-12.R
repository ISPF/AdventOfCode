source("src/functions.R")
input <- aoc_get_inputfile(12, 2022, "laurent")
input <- unlist(str_split(input, "\n"))
input <- lapply(input, FUN=function(x) str_split(x, '')[[1]])
dt <- as.data.table(input[1:41])
grid <- as.matrix(dt)

ws <- which(grid=="S", arr.ind = T)
we <- which(grid=="E", arr.ind = T)
grid[ws] <- "a"
grid[we] <- "z"

q <- list()
q <- append(q, list(0,ws[1],ws[2]))

vis <- wS

for r, row in enumerate(grid):
  for c, item in enumerate(row):
  if item == "S":
  sr = r
sc = c
grid[r][c] = "a"
if item == "E":
  er = r
ec = c
grid[r][c] = "z"

q = deque()
q.append((0, sr, sc))