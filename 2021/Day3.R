# -------------------- DAY 3 --------------------
library(tidyverse)
library(compositions)
library(janitor)

input <- read_lines("2021/input_data/d3_input_text.txt")

input <- str_split_fixed(input, "", 12) %>% 
  as.data.frame() %>% 
  tibble() %>% 
  mutate(across(names(.),
                ~ as.numeric(.x)))

# ---------- Part One ----------
df <- input %>% 
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

3429254

# ---------- Part Two ----------
nms <- names(input)
oxygen <- input
co2 <- input

for (i in seq_along(nms)) {
  
  if (nrow(oxygen) == 1) {
    
    oxygen <- oxygen
    
  } else if (nrow(oxygen) > 1) {
    
    oxygen <- oxygen %>%
      mutate(var := !!sym(nms[i]))
    
    tmp <- tabyl(oxygen$var) %>% 
      tibble() %>% 
      setNames(c("var", "n", "percent")) %>% 
      arrange(desc(n),
              desc(var)) %>% 
      slice_head() %>% 
      pull(var)
    
    oxygen <- oxygen %>% 
      filter(var == tmp) %>% 
      select(-var)
    
    
  }
  
}


for (i in seq_along(nms)) {
  
  if (nrow(co2) == 1) {
    
    co2 <- co2
    
  } else if (nrow(co2) > 1) {
    
    co2 <- co2 %>%
      mutate(var := !!sym(nms[i]))
    
    tmp <- tabyl(co2$var) %>% 
      tibble() %>% 
      setNames(c("var", "n", "percent")) %>% 
      arrange(desc(n),
              desc(var)) %>% 
      slice_head() %>% 
      pull(var)
    
    co2 <- co2 %>% 
      filter(var != tmp) %>% 
      select(-var)
    
    
  }
  
}

oxygen <- oxygen %>% 
  unite('.', 1:ncol(.), sep = "") %>% 
  pull() %>% 
  unbinary()

co2 <- co2 %>% 
  unite('.', 1:ncol(.), sep = "") %>% 
  pull() %>% 
  unbinary()

oxygen * co2