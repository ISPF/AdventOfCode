source("functions.R")
name <- "Hubert"
year <- 2022
day <- 4
input <- aoc_get_inputfile(day, year, name)

library(tidyverse)
assignments <- tibble( line = read_lines( input )) %>%
  separate(line, c("elf1", "elf2"), sep = ","  ) %>%
  separate(elf1, c("debut1", "fin1"), sep= "-") %>%
  separate(elf2, c("debut2", "fin2"), sep = "-") %>%
  mutate_all(as.integer)

# Total overlap
assignments %>%
  mutate(overlap = (debut1 >= debut2 & fin1 <= fin2) |
           (debut2 >= debut1 & fin2 <= fin1))%>%
  summarise(sum(overlap))

# Partial overlap
assignments %>%
  mutate(overlap = ( debut1 <= fin2 & fin1 >= debut2 ) |
           ( debut2 <= fin1 & fin2 >= debut1 ))%>%
  summarise(sum(overlap))
