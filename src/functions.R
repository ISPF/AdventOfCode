library(data.table)
library(curl)
library(DBI)
library(httr)
library(lubridate)
library(openxlsx)
library(rjson)
library(stringdist)
library(stringr)
library(tools)
library(stringi)
library(tidyverse)

# Récupération des inputs  ---------------------------------------------
#https://colin-fraser.net/post/a-quick-tutorial-on-importing-data-from-advent-of-code-into-r/

aoc_get_response <- function(day, year = 2022, session_cookie = rstudioapi::askForSecret("Advent of Code Session Cookie")) {
  aoc_url <- sprintf("adventofcode.com/%s/day/%s/input", year, day)
  cookie <- set_cookies(session = session_cookie)
  response <- GET(aoc_url, cookie)
  rawToChar(response$content)
}

aoc_get_inputfile <- function(day=day, year=year, name=name){
  inputRDSFile <- sprintf("%s/input/%s-%s-%s.rds", year, name, year, day)
  if (!file.exists(inputRDSFile)) {
    cat("Get input file From server\n")
    input <- aoc_get_response(day,year)
    saveRDS(input, inputRDSFile)
  } else {
    cat("Get input file From filesystem\n")
    input <- readRDS(inputRDSFile)
  }
  input
}
