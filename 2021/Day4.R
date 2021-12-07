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



numbers <- numbers[1:100]

# ---------- Part One ----------
df <- boards %>% 
  as.data.frame() %>% 
  mutate(rs = NA)



bingo_fun <- function(data, y) {
  
  for (i in seq_along(numbers)) {
    
    df <- df %>% 
      mutate(across(all_of(nms),
                    ~ ifelse(.x == numbers[i],
                             0,
                             .x))) %>% 
      mutate(rs = rowSums(.[1:5])) %>% 
      group_by(board) %>% 
      mutate(across(all_of(nms),
                    ~ sum(.x),
                    .names = "{.col}_cs")) %>% 
      as.data.frame()
    
  }
  
}







for (i in seq_along(numbers)) {
  
  print(i)
  
  if (!0 %in% df$rs) {
    
    df <- df %>% 
      mutate(across(all_of(nms),
                    ~ ifelse(.x == numbers[i],
                             0,
                             .x))) %>% 
      mutate(rs = rowSums(.[1:5])) %>% 
      group_by(board) %>% 
      mutate(across(all_of(nms),
                    ~ sum(.x),
                    .names = "{.col}_cs")) %>% 
      as.data.frame()
    
  } else if (0 %in% df$rs) {
    
    df <- df %>% 
      mutate(turn = i)
    
  }
  
  
}

temp <- df %>% 
  select(ends_with("_cs")) %>% 
  summarise(min(.))






