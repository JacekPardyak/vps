# txt2json
library(tidyverse)
library(jsonlite)

read_csv("./data/vps_churn_data.txt") %>% data.frame() %>% write_json("./data/vps_churn_data.json")
read_csv("./data/vps_test_data.txt") %>% data.frame() %>% write_json("./data/vps_test_data.json")
# test
vps <- fromJSON("./data/vps_churn_data.json") #%>% as.data.frame
pro <- fromJSON("./data/vps_test_data.json") # %>% as.data.frame

class(vps)
