source("src/functions.R")
input <- read_lines(aoc_get_inputfile(10, 2022, "laurent"))

#exemple
#input <- read_lines("2022/input/laurent-2022-10.txt")
#input <- read_lines("noop
#addx 3
#addx -5")

lstInput <- str_split(input, ' ')
lstOP <- unlist(lapply(lstInput, FUN=function(x) x[1]))
lstVal <- as.integer(unlist(lapply(lstInput, FUN=function(x) x[2])))

dt <- data.table(op=lstOP, val=lstVal)
dt[, cycles:=ifelse(op=="noop",1,2)]
dt[, to:=cumsum(cycles)+1]
dt[, from:=shift(to, fill = 0)+1]
dt[,x:=cumsum(ifelse(is.na(val),0,val))+1]
dt[,x1:=shift(x, fill=0)]
dt2 <- data.table(from=c(20, seq(60, 220, 40)), to=c(20, seq(60, 220, 40)))
setkey(dt, from, to)
res <- foverlaps(dt2, dt, type="any")

res[, y:=i.from*ifelse(i.from==to, x, x1)]
res[,sum(y)]

# section 2 ---------------------------------------------------------------