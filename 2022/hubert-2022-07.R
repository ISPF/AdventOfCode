source("functions.R")
name <- "Hubert"
year <- 2022
day <- 7
input <- aoc_get_inputfile(day, year, name)

library(tidyverse)

console <- read_lines(input)
position <- 3 # Start with root folder content

navigate_and_list <- function(dir){
  # 1 - Sum files size
  size <- 0
  instruction <- console[position]
  position <<- position + 1
  while( !grepl("\\$", instruction) && position<=1+length(console)){
    # All before next $ cd is files with length or dir
    size <- size + parse_number( paste(instruction, "0"))
    instruction <- console[position]
    position <<- position + 1
  }
  
  # 2 - Navigate subdirs
  subdir_list <- list()
  while ( instruction!="$ cd .." && position<=1+length(console)) {
    subdir_name <- gsub("\\$ cd ", "", instruction)
    position <<- position + 1 # Ignore $ ls
    subdir_content <- navigate_and_list(subdir_name)
    size <- size + subdir_content[[ 2 ]]
    subdir_list <- c( subdir_list, list( subdir_content ) )

    instruction <- console[position]
    position <<- position + 1
  }
  # return previous level
  return(list(dir, size, subdir_list))
}
my_fs <- navigate_and_list("/")

find_small <- function( dir ){
  sum( unlist( lapply( dir[[ 3 ]], find_small ))) + 
    if( dir[[ 2 ]] <= 100000) dir[[ 2 ]] else 0
}
find_small(my_fs)

find_big <- function( dir ){
  if( dir[[ 2 ]] >= 30000000 - ( 70000000 - my_fs[[ 2 ]] )) {
    pmin( dir[[ 2 ]],
          min( sapply( dir[[ 3 ]], find_big)))
  } else +Inf
}
find_big(my_fs)
