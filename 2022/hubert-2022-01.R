source("functions.R")
name <- "Hubert"
year <- 2022
day <- 1
input <- aoc_get_inputfile(day, year, name)

library(tidyverse)
elves <- tibble( food_calorie = read_lines( input ) ) %>%
  mutate( food_calorie = as.integer(food_calorie) ,
          separator = is.na( food_calorie),
          elf_id = cumsum( separator )) %>%
  group_by( elf_id ) %>% 
  summarise( total_calories = sum( food_calorie, na.rm = TRUE ),
             .groups = "drop" )

elves %>% 
  summarise( maximum = max( total_calories ))

elves %>%
  slice_max( total_calories, n = 3 ) %>%
  summarise( top_3_total = sum( total_calories ) )

  
