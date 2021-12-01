# -------------------- DAY 1 --------------------
library(tidyverse)

# ---------- Part One ----------
input <- read_lines("2021/input_data/d1_input_text.txt") %>% 
  tibble() %>% 
  rename(measurement = '.') %>% 
  mutate(measurement = as.numeric(measurement))

input %>% 
  mutate(lag_measurement = lag(measurement, 1)) %>% 
  filter(measurement > lag_measurement) %>% 
  tally()

# ---------- Part Two ----------
input %>% 
  mutate(l1 = lag(measurement, 1),
         l2 = lag(measurement, 2)) %>% 
  rowwise() %>%
  mutate(tw = sum(c(measurement,
                    l1,
                    l2),
                  na.rm = F)) %>% 
  ungroup() %>% 
  mutate(l_tw = lag(tw, 1)) %>% 
  filter(tw > l_tw) %>% 
  tally()
