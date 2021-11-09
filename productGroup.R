library(tidyverse)
ProductGroup <- read_csv("data/ProductGroup.csv", 
                         col_types = cols(...1 = col_skip(), id = col_character(), 
                                          parentCategoryId = col_character()))
View(ProductGroup)

tbl_1 <- ProductGroup %>% select(c("name", "parentCategoryId", "depth")) %>% 
  rename(from = name, id = parentCategoryId)
head(tbl_1)
tbl_2 <- ProductGroup %>% select(c( "id", "name")) %>% 
  rename(to = name)
head(tbl_2)
routes <-  tbl_1 %>% left_join(tbl_2)
head(routes)
routes <- routes %>% arrange(depth)
