# -------------------- DAY 4 --------------------
library(tidyverse)
library(tidyselect)

numbers <- read_lines("2021/input_data/d4_numbers.txt") %>% 
  str_split(",") %>% 
  unlist() %>% 
  as.integer()

boards <- read_lines("2021/input_data/d4_boards.txt") %>% 
  tibble() %>% 
  set_names("nn") %>% 
  mutate(nn = str_trim(nn)) %>% 
  separate(nn, into = paste0("v",
                             seq(1:5))) %>% 
  filter(!is.na(v5)) %>% 
  mutate(across(names(.),
                ~ as.numeric(.x)))

nms <- names(boards)

boards <- boards %>%  
  mutate(board = sort(rep(seq(1, 100), 5))) %>% 
  mutate(rs = NA) %>% 
  as.data.frame()

# ---------- Part One ----------
bingo_fun <- function(data) {
  
  exp <- matrix(nrow = length(numbers),
                ncol = 3)
  
  for (i in seq_along(numbers)) {
    
    data <- data %>% 
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
    
    cs <- data %>%
      select(ends_with("_cs")) %>%
      slice_head() %>%
      t()

    mat <- data %>%
      select(rs) %>%
      add_column(cs) %>%
      as.matrix()
    
    if (min(mat) == 0) {
      
      exp[i, 1] <- i
      exp[i, 2] <- data %>% select(all_of(nms)) %>% as.matrix() %>% sum()
      exp[i, 3] <- numbers[i]
      
    }
    
  }
  
  y <- exp %>% 
    as.data.frame()
  
  return(y)

}

blist <- split(boards,
               f = boards$board)

bingo <- map(.x = blist,
             .f = bingo_fun)

bind_rows(bingo) %>% 
  slice(which.min(V1)) %>% 
  mutate(product = V2*V3) %>% 
  pull(product)


# ---------- Part Two ----------
bind_rows(bingo, .id = "board") %>%  
  group_by(board) %>% 
  slice(which.min(V1)) %>% 
  ungroup() %>% 
  slice(which.max(V1)) %>% 
  mutate(product = V2*V3) %>% 
  pull(product)
