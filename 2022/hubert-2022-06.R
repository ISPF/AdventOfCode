source("functions.R")
name <- "Hubert"
year <- 2022
day <- 6
input <- aoc_get_inputfile(day, year, name)

s <- strsplit( input, "")[[ 1 ]]

for( l in seq_len( length( s ) - 3 )){
  window <- s[ l:( l + 3 ) ]
  if( length( window ) == length( unique( window )) ){
    print( l + 3 )
    break
  } 
}

for( l in seq_len( length( s ) - 13 )){
  window <- s[ l:( l + 13 ) ]
  if( length( window ) == length( unique( window )) ){
    print( l + 13 )
    break
  } 
}
