source("src/functions.R")
name <- "Thierry"
year <- 2022
day <- 2

input <- aoc_get_inputfile(day, year, name)
inputs <- unlist(str_split(input,"\n"))

total <-  0

for (i in inputs)
{
    score=switch (i,
      "A X" = 4,
      "A Y" = 8,
      "A Z" = 3,
      "B X" = 1,
      "B Y" = 5,
      "B Z" = 9,
      "C X" = 7,
      "C Y" = 2,
      "C Z" = 6
    )
    if(!is.null(score)){
    total <-   total + score}
}

print(total)

total2 <- 0

for (i in inputs)
{
  score=switch (i,
                "A X" = 3,
                "A Y" = 4,
                "A Z" = 8,
                "B X" = 1,
                "B Y" = 5,
                "B Z" = 9,
                "C X" = 2,
                "C Y" = 6,
                "C Z" = 7
  )
  if(!is.null(score)){
    total2 <-   total2 + score}
}

print(total2)

