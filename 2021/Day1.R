library(tidyverse)
library(tidyselect)

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
df <- input %>% 
  mutate(l1 = dplyr::lag(measurement, 1),
         l2 = dplyr::lag(measurement, 2)) %>% 
  rowwise() %>%
  mutate(three_window = sum(measurement,
                            l1,
                            l2)) %>% 
  ungroup()

df <- df %>% 
  mutate(lag_three_window = dplyr::lag(three_window, 1))

df <- df %>% 
  mutate(increase = ifelse(three_window > lag_three_window,
                           1,
                           0))

df %>% 
  filter(increase == 1) %>% 
  tally()
