# -------------------- DAY 4 --------------------
library(tidyverse)

numbers <- read_lines("2021/input_data/d4_numbers.txt") %>% 
  str_split(",") %>% 
  unlist() %>% 
  as.integer()


input <- read_lines("2021/input_data/d4_boards.txt") %>% 
  tibble() %>% 
  set_names("nn")

boards <- input %>% 
  mutate(nn = str_trim(nn)) %>% 
  separate(nn, into = paste0("v",
                             seq(1:5))) %>% 
  filter(!is.na(v5))

boards <- boards %>% 
  mutate(board = sort(rep(seq(1, 100), 5))) %>% 
  group_by(board) %>% 
  mutate(row = row_number()) %>% 
  ungroup()



# ---------- Part One ----------
