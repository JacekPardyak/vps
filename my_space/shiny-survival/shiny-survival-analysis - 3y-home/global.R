library(survival)
library(tidyverse)
library(tidymodels)

df <- read_csv("./out_3y.csv") %>% filter(PG_2 != '?') 

prod_all <- df %>% select(all_of("PROD")) %>% distinct() %>% pull()

