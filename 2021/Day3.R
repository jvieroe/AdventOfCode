# -------------------- DAY 3 --------------------
library(tidyverse)
library(compositions)

input <- read_lines("2021/input_data/d3_input_text.txt") 

input <- str_split_fixed(input, "", 12) %>% 
  as.data.frame() %>% 
  tibble()

# ---------- Part One ----------
df <- input %>% 
  mutate(across(names(.),
                ~ as.numeric(.x))) %>% 
  summarize(across(names(.),
                   ~ mean(.x)))

gamma <- df %>% 
  summarize(across(names(.),
                   ~ round(.x))) %>% 
  unite('.', 1:ncol(.), sep = "") %>% 
  pull() %>% 
  unbinary()

epsilon <- df %>% 
  summarize(across(names(.),
                   ~ round(1 - .x))) %>% 
  unite('.', 1:ncol(.), sep = "") %>% 
  unbinary()

gamma*epsilon

# ---------- Part Two ----------


# input <- read_lines("2021/input_data/d3_input_text.txt") %>% 
#   tibble() %>% 
#   rename(binary = '.')


# input %>% 
#   separate(col = binary,
#            into = paste0("n", seq(1, nchar(input$binary[1]), 1)))


paste0("n", seq(1, nchar(input$binary[1]), 1))
