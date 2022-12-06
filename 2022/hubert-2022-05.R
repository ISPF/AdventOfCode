source("functions.R")
name <- "Hubert"
year <- 2022
day <- 5
input <- aoc_get_inputfile(day, year, name)

library(tidyverse)

stacks_df <- read_fwf( file = input, n_max =8,
                       fwf_widths( rep(4, times = 9))) %>% 
  mutate_all( substr, start = 2, stop = 2 ) %>%
  arrange( desc( row_number() ))

moves <- tibble( line = read_lines( input, skip = 10 )) %>%
  separate(line, c(NA, "n", "f", "t"), sep = "[^[:digit:]]+") %>%
  mutate_all(as.integer)

library(rstack)
init_one_stack <- function(s){
  S <- stack$new()
  for(i in seq_along(s))
    if(!is.na(s[[i]]))
      S$push(s[[i]])
  S
}

# Top of stacks
stacks <- map(stacks_df, init_one_stack )

moves %>%
  mutate( res = pmap( list(n, f, t), 
                     function(number, from, to) { 
                       for(i in 1:number) {
                         crane = stacks[[from]]$pop()
                         stacks[[to]]$push(crane)
                       }
                     }))
paste( map( stacks, ~.$peek()), collapse = "")

# Moving several at a time
stacks <- map(stacks_df, init_one_stack )

crane = stack$new()
moves %>%
  mutate( res = pmap( list(n, f, t), 
                      function(number, from, to) { 
                        for( i in 1:number ) 
                          crane$push( stacks[[ from ]]$pop())
                        while( crane$size() )
                          stacks[[ to ]]$push( crane$pop() )
                      }))
paste( map( stacks, ~.$peek()), collapse = "")
