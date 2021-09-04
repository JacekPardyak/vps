# txt2json
library(tidyverse)
library(rjson)

read_csv("./data/vps_churn_data.txt") %>% toJSON() %>% write("./data/vps_churn_data.json")
read_csv("./data/vps_test_data.txt") %>% toJSON() %>% write("./data/vps_test_data.json")
# test
# result <- fromJSON(file = "./data/vps_churn_data.json") %>% data.frame()
