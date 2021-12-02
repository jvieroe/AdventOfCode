# -------------------- DAY 2 --------------------
library(tidyverse)
library(zoo)

input <- read_lines("2021/input_data/d2_input_text.txt") %>% 
  tibble() %>% 
  rename(command = '.')

# ---------- Part One ----------
input <- input %>% 
  mutate(forward = grepl("forward", command),
         down = grepl("down", command),
         up = grepl("up", command)) %>% 
  mutate(across(c(forward, down, up),
                ~ ifelse(.x == TRUE,
                         parse_number(command),
                         NA)))

input %>% 
  summarize(horizontal = sum(forward, na.rm = T),
            vertical = sum(down, na.rm = T) - sum(up, na.rm = T)) %>% 
  mutate(product = horizontal * vertical) %>% 
  pull(product)

# 1459206

# ---------- Part Two ----------
input <- input %>% 
  mutate(aim = NA)

input
  

# mutate(horizontal = sum(forward, na.rm = T),
#          vertical = rollsum(up, 1, fill = NA))
# input
