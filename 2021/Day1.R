library(tidyverse)

getwd()

df <- read_lines("2021/essays/aoc_1.txt") %>% 
  tibble()

