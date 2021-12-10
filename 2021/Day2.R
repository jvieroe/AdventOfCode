# -------------------- DAY 2 --------------------
library(tidyverse)

input <- read_lines("2021/input_data/d2_input_text.txt") %>% 
  tibble() %>% 
  rename(command = '.')

input <- input %>% 
  mutate(forward = grepl("forward", command),
         down = grepl("down", command),
         up = grepl("up", command)) %>% 
  mutate(across(c(forward, down, up),
                ~ ifelse(.x == TRUE,
                         parse_number(command),
                         NA)))

# ---------- Part One ----------
input %>% 
  summarize(horizontal = sum(forward, na.rm = T),
            vertical = sum(down, na.rm = T) - sum(up, na.rm = T)) %>% 
  mutate(product = horizontal * vertical) %>% 
  pull(product)

# ---------- Part Two ----------
input %>% 
  mutate(across(names(.),
                ~ ifelse(is.na(.x),
                         0,
                         .x))) %>% 
  mutate(horizontal = cumsum(forward),
         aim = cumsum(down) - cumsum(up)) %>% 
  mutate(vertical = cumsum((aim * forward))) %>% 
  mutate(product = horizontal * vertical) %>% 
  slice_tail() %>% 
  pull(product)
