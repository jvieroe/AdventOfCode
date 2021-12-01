library(tidyverse)

# -------------------- DAY 1 --------------------

# ---------- Part One ----------

input <- read_lines("2021/input_data/d1_input_text.txt") %>% 
  tibble() %>% 
  rename(measurement = '.') %>% 
  mutate(measurement = as.numeric(measurement))

df <- input %>% 
  mutate(lag_measurement = dplyr::lag(measurement, 1))

df <- df %>% 
  mutate(increase = ifelse(measurement > lag_measurement,
                           1,
                           0))

df %>% 
  filter(increase == 1) %>% 
  tally()



# ---------- Part Two ----------
