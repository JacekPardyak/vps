library(tidyverse)
Contract <- read_csv("recommender/Contract.csv") %>% 
  mutate(customerId = as.character(customerId)) %>% 
  mutate(productId = as.character(productId))
newContract <- read_csv("recommender/newContract.csv") %>% 
  mutate(customerId = as.character(customerId))%>% 
  mutate(productId = as.character(productId))


View(Contract)