# -------------------- DAY 5 --------------------
library(tidyverse)

input <- read_lines("2021/input_data/d5_input_text.txt")

lines <- tibble(input = input) %>% 
  separate(input,
           into = c("x1", "y1", "x2", "y2"),
           convert = TRUE)


# ---------- Part One ----------