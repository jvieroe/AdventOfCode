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
df <- input %>% 
  mutate(across(names(.),
                ~ as.numeric(.x)))
  # mutate(across(names(.),
  #               ~ strtoi(.x)))

nms <- names(df)

oxygen <- df
co2 <- df

# for (i in seq_along(nms)) {
#   
#   co2 <- co2 %>%
#     mutate(var := !!sym(nms[i]),
#            var_mean = mean(!!sym(nms[i])))
#   
#   
# }

# custom_round <- function(x, y) {
#   
#   if (x == 1/2) {
#     y <- ceiling(x)
#   } else if (x != 1/2) {
#     y <- round(x)
#   }
#   
#   return(y)
#   
# }
# 
# custom_round <- function (x, digits = 0) {
#   posneg <- sign(x)
#   z <- trunc(abs(x) * 10 ^ (digits + 1)) / 10
#   z <- floor(z * posneg + 0.5) / 10 ^ digits
#   return(z)
# }
# 
# 
# 
# 
# for (i in seq_along(nms)) {
#   
#   if (nrow(oxygen) == 1) {
#     
#     oxygen <- oxygen
#     
#   } else if (nrow(oxygen) > 1) {
#     
#     oxygen <- oxygen %>%
#       mutate(across(all_of(nms),
#                     ~ mean(.x),
#                     .names = "{.col}_mean")) %>% 
#       mutate(across(ends_with("_mean"),
#                     ~ round(.x))) %>% 
#       filter(!!sym(nms[i]) == !!sym(paste0(nms[i], "_mean"))) %>% 
#       select(-ends_with("_mean"))
#     
#   }
#   
# }
# 


for (i in seq_along(nms)) {
  
  if (nrow(oxygen) == 1) {
    
    oxygen <- oxygen
    
  } else if (nrow(oxygen) > 1) {
    
    oxygen <- oxygen %>%
      mutate(var := !!sym(nms[i]),
             var_mean = round(mean(var) + 1e-6))
    
    if (oxygen$var_mean[1] == 0.5) {
      
      oxygen <- oxygen %>% 
        filter(var == 1)
      
    } else if (oxygen$var_mean[1] != 0.5) {
      
      oxygen <- oxygen %>% 
        filter(var == var_mean) %>% 
        select(-c(var,
                  var_mean))
      
    }
    
  }
  
}


for (i in seq_along(nms)) {
  
  if (nrow(co2) == 1) {
    
    co2 <- co2
    
  } else if (nrow(co2) > 1) {
    
    # co2 <- co2 %>%
    #   mutate(across(all_of(nms),
    #                 ~ mean(.x),
    #                 .names = "{.col}_mean")) %>% 
    #   mutate(across(ends_with("_mean"),
    #                 ~ round(.x))) %>% 
    #   filter(!!sym(nms[i]) != !!sym(paste0(nms[i], "_mean"))) %>% 
    #   select(-ends_with("_mean"))
    
    co2 <- co2 %>%
      mutate(var := !!sym(nms[i]),
             var_mean = round(mean(var))) %>% 
      filter(var != var_mean) %>% 
      select(-c(var,
                var_mean))
    
    
  }
  
}

oxygen
co2

oxygen %>% 
  unite('.', 1:ncol(.), sep = "") %>% 
  pull() %>% 
  unbinary()

co2 %>% 
  unite('.', 1:ncol(.), sep = "") %>% 
  pull() %>% 
  unbinary()

1465*3787
oxygen * co2

2981085/3787






df <- input %>%
  mutate(across(names(.),
                ~ as.numeric(.x))) %>%
  mutate(across(names(.),
                ~ mean(.x),
                .names = "{.col}_mean")) %>%
  mutate(across(names(.),
                ~ round(mean(.x)),
                .names = "{.col}_mean_round"))

df <- input %>%
  mutate(across(all_of(nms),
                ~ as.numeric(.x))) %>%
  mutate(across(all_of(nms),
                ~ mean(.x),
                .names = "{.col}_mean")) %>%
  mutate(across(all_of(nms),
                ~ round(mean(.x)),
                .names = "{.col}_mean_round"))


ogr <- df
co2sr <- df

while (nrow(ogr) > 1) {
  
  for (i in 1:12) {
    
    ogr <- ogr %>% 
      filter(!!sym(paste0("V", i)) == !!sym(paste0("V", i, "_mean_round")))
    
  }
  
}

ogr %>% 
  unite('.', 1:ncol(.), sep = "") %>% 
  unbinary()


while (nrow(co2sr) > 1) {
  
  for (i in 1:12) {
    
    co2sr <- co2sr %>% 
      filter(!!sym(paste0("V", i)) != !!sym(paste0("V", i, "_mean")))
    
  }
  
}







# input <- read_lines("2021/input_data/d3_input_text.txt") %>% 
#   tibble() %>% 
#   rename(binary = '.')


# input %>% 
#   separate(col = binary,
#            into = paste0("n", seq(1, nchar(input$binary[1]), 1)))


paste0("n", seq(1, nchar(input$binary[1]), 1))



###############################

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
oxygen <- input %>% 
  mutate(across(names(.),
                ~ as.numeric(.x)))

nms <- names(oxygen)
nms

while (nrow(oxygen) > 1) {
  
  for (i in 1:12) {
    
    oxygen <- oxygen %>% 
      mutate(across(all_of(nms),
                    ~ ifelse(.x == 0.5,
                             ceiling(.x),
                             round(.x)),
                    .names = "{.col}_rnd"))%>% 
      filter(!!sym(paste0("v", i) == !!sym(paste0("v", i, "_rnd"))))
    
  }
  
}



