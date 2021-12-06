# -------------------- DAY 4 --------------------
library(tidyverse)
library(tidyselect)

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
  filter(!is.na(v5)) %>% 
  mutate(across(names(.),
                ~ as.numeric(.x)))

nms <- names(boards)

boards <- boards %>% 
  mutate(board = sort(rep(seq(1, 100), 5))) %>% 
  group_by(board) %>% 
  mutate(row = row_number())

df <- boards

numbers <- numbers[1:10]

# ---------- Part One ----------
for (i in seq_along(numbers)) {
  
  print(i)
  
  #print(numbers[i])
  
  df <- df %>% 
    mutate(across(all_of(nms),
                  ~ ifelse(.x == numbers[i],
                           0,
                           .x))) %>% 
    # mutate(rs = pmap_dbl(select(., nms),
    #                      function(...) mean(c(...))))
    # mutate(rs = rowMeans(subset(., select = c(v1, v2, v3, v4, v5)),
    #                      na.rm = TRUE))
    # mutate(rs = rowMeans(select(., nms)))
    rowwise() %>%
    mutate(is_na = sum(c(nms),
                       na.rm = F)) %>%
    ungroup()
  
}


