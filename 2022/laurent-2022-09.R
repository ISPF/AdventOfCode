source("src/functions.R")
input <- read_lines(aoc_get_inputfile(9, 2022, "laurent"))

# input <- read_lines("R 4
# U 4
# L 3
# D 1
# R 4
# D 1
# L 5
# R 2")

lstInput <- str_split(input, ' ')
lstDirection <- unlist(lapply(lstInput, FUN=function(x) x[1]))
lstN <- as.integer(unlist(lapply(lstInput, FUN=function(x) x[2])))
lstMoves <- list("N"="D", "E"="L", "S"="U", "W"="R")

whereIsTailFromHead <- function() {
  w1 <- which(ropeH=="H",arr.ind = TRUE)
  w2 <- which(ropeT=="T",arr.ind = TRUE)
  i <- w1[1]
  j <- w1[2]
  x <- w2[1]
  y <- w2[2]
  fcase(i==x & j==y-1, "E",
        i==x & j==y+1, "W",
        i==x+1 & j==y, "N",
        i==x-1 & j==y, "S",
        i==x+1 & j==y+1, "NW",
        i==x+1 & j==y-1, "NE",
        i==x-1 & j==y+1, "SW",
        i==x-1 & j==y-1, "SE")
}

move1 <- function(direction){
  w1 <- which(ropeH=="H",arr.ind = TRUE)
  w2 <- which(ropeT=="T",arr.ind = TRUE)
  i <- w1[1]
  j <- w1[2]
  x <- w2[1]
  y <- w2[2]
  z <- whereIsTailFromHead()
  
  ropeH[w1]=""
  if(direction=="U") ropeH[i-1, j]="H"
  if(direction=="D") ropeH[i+1, j]="H"
  if(direction=="L") ropeH[i, j-1]="H"
  if(direction=="R") ropeH[i, j+1]="H"
  
  if((nchar(z)==1 & direction==lstMoves[z]) |
     (nchar(z)==2 & direction %in% c(lstMoves[substring(z,1,1)],lstMoves[substring(z,2,2)])))  {
    #on met T sur l'ancienne position de H sinon, on ne le bouge pas
    ropeT[w2]=""
    ropeT[w1]="T"
    ropeP[w1]="#"
    ropeP[w2]="#"
  }
  ropeH <<- ropeH
  ropeT <<- ropeT
  ropeP <<- ropeP
}

move <- function(direction,nb) {
   i <- 1
   while(i<=nb){
     move1(direction)
     i <- i+1
   }
}

# ropeH
# ropeT
# ropeP
# 
# move1("R")
# move("D",2)
# 
# ropeH
# ropeT
# ropeP

n <- 1200
ropeH <- matrix('',n,n)
ropeH[n/2,n/2] <- "H"
ropeT <- matrix('',n,n)
ropeT[n/2,n/2] <- "T"
ropeP <- matrix('',n,n)
ropeP[n/2,n/2] <- "#"

# Day 1 -------------------------------------------------------------------


for(i in 1:2000) {
  if(i%%10==0){
    cat(sprintf("%s\n",i))
  }
  move(lstDirection[i], lstN[i])
}
###Beaucoup trop long !
sum(ropeP=='#')



# Refactor ----------------------------------------------------------------

input <- data.frame(lstDirection, lstN)
hpos <- tpos <- matrix(0,ncol=2)
for(i in 1:nrow(input)){
  d <- switch(input[i,1],"R"=c(1,0),
              "U"=c(0,1),"L"=c(-1,0),"D"=c(0,-1))
  for(j in 1:input[i,2]){
    hpos <- rbind(hpos,hpos[nrow(hpos),]+d)
  }
}

for(i in 1:nrow(hpos)){
  dif <- hpos[i,]-tail(tpos,1)
  if(any(abs(dif)>1)){
    if(dif[1]>=1&dif[2]==0) m <- c(1,0)
    if(dif[1]>=1&dif[2]>=1) m <- c(1,1)
    if(dif[1]>=1&dif[2]<=-1) m <- c(1,-1)
    if(dif[1]<=-1&dif[2]==0) m <- c(-1,0)
    if(dif[1]<=-1&dif[2]<=-1) m <- c(-1,-1)
    if(dif[1]<=-1&dif[2]>=1) m <- c(-1,1)
    if(dif[1]==0&dif[2]>=1) m <- c(0,1)
    if(dif[1]==0&dif[2]<=-1) m <- c(0,-1)
    tpos <- rbind(tpos, tail(tpos,1)+m)
  }
}
single <- tpos[!duplicated(tpos),]
nrow(single)
#6357

#Now tpos is position of knot 2
for(k in 3:10){
  hpos <- tpos
  tpos <- matrix(0,ncol=2)
  for(i in 1:nrow(hpos)){
    dif <- hpos[i,]-tail(tpos,1)
    if(any(abs(dif)>1)){
      if(dif[1]>=1&dif[2]==0) m <- c(1,0)
      if(dif[1]>=1&dif[2]>=1) m <- c(1,1)
      if(dif[1]>=1&dif[2]<=-1) m <- c(1,-1)
      if(dif[1]<=-1&dif[2]==0) m <- c(-1,0)
      if(dif[1]<=-1&dif[2]<=-1) m <- c(-1,-1)
      if(dif[1]<=-1&dif[2]>=1) m <- c(-1,1)
      if(dif[1]==0&dif[2]>=1) m <- c(0,1)
      if(dif[1]==0&dif[2]<=-1) m <- c(0,-1)
      tpos <- rbind(tpos, tail(tpos,1)+m)
    }
  }
}

single <- tpos[!duplicated(tpos),]
nrow(single)
