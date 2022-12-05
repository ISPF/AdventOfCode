source("functions.R")
name <- "Hubert"
year <- 2022
day <- 3
input <- aoc_get_inputfile(day, year, name)

library(tidyverse)

# Letters values
v<- setNames( 1:52, c(letters,LETTERS) )

rucksack <- tibble( l = read_lines( input )) %>%
  mutate ( n = nchar(l), 
           l1 = substr(l, 1, n/2), 
           l2 = substr(l, n/2 + 1, n),
           c1 = strsplit( l1 , ""),
           c2 = strsplit( l2 , ""),
           c1 = map(c1, unique),
           c2 = map(c2, unique),
           equals = map2( c1, c2, ~ .x %in% .y),
           pos = map(equals, which),
           common = map2(c1, pos, ~ .x[.y])
           )
rucksack %>%
  mutate(score = map(common, ~v[.]) ,
         score = unlist(score) )%>%
  summarise( total = sum(score))
           
groups <- rucksack %>% select(value = l) %>%
  mutate(name = paste0("elf_", rep(1:3, 100)),
         group = (row_number() -1 ) %/% 3) %>%
  pivot_wider() %>%
  mutate( group = NULL)%>%
  mutate_all( strsplit, "") %>%
  mutate_all(~map(., unique)) %>%
  mutate( dup = pmap(list(elf_1, elf_2, elf_3), 
                     ~ ..1 %in% ..2 & ..1 %in% ..3),
          pos = map(dup, which),
          common = map2(elf_1, pos, ~ .x[.y]))
groups %>%
  mutate(score = map(common, ~v[.]),
         score = unlist(score) ) %>%
  summarise(total = sum( score ))

