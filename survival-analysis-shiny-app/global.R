library(shiny)
library(tidyverse)
library(KMsurv)
library(survival)

n = 10000
df <- data.frame(id = c(1:n)) %>% 
  mutate(type = sample(c(1, 2), n, replace=TRUE)) %>%
  mutate(type = as.factor(type)) %>%
  mutate(delta = sample(c(0, 1), n, replace=TRUE)) %>%
  mutate(time = sample(c(1:365), n, replace=TRUE)) %>%
  mutate(gender = sample(c("f","m"), n, replace=TRUE)) %>% 
  mutate(gender = as.factor(gender)) %>%
  mutate(category = sample(c("a","b","c"), n, replace=TRUE)) %>% 
  mutate(category = as.factor(category)) %>%
  mutate(product = sample(LETTERS, n, replace=TRUE)) %>% 
  mutate(product = as.factor(product)) 


