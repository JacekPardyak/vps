# apriori in R
library(tidyverse)
library(arules)
vps <- read_csv("./data/vps_churn_data.txt")
vps <- vps %>% mutate(is_churn = as.factor(is_churn))

vars = setdiff(names(vps), c('id', 'is_churn'))
for (var in vars) {
vps <- vps %>% mutate(!!sym(var) := cut_interval(!!sym(var),
                                                 n = 3,
                                                 breaks = unique(
                                                   quantile(!!sym(var), probs = seq.int(0, 1, by=1/3))),
                                                 labels = c("low", "medium", "high")))
}

vps <- vps %>%
  pivot_longer(., cols = !one_of('id'), names_to = "Variable", values_to = "Value") %>%
  rowwise() %>% mutate(Item = paste(Variable, Value, sep = "=")) %>% ungroup() %>%
  select(one_of('id', "Item")) %>% rename("Transaction" = id) #`colnames<-`(c("Transaction", "Item"))

vps %>% count(Item, sort = TRUE) %>% ggplot(aes(x = reorder(Item, -n), y = n, fill = Item))  + 
  geom_bar(stat = "identity") + coord_flip() + theme(legend.position = "none")

vps %>% write_csv("./data/vps_transactions.csv")

vps <- read.transactions(file = "./data/vps_transactions.csv",
                           format="single", cols=c(1,2), sep=",", rm.duplicates=TRUE)

churn_all <- apriori(vps, 
                     parameter = list(sup = 0.1,
                                      conf = 0.8,
                                      target="rules",
                                      minlen = 1,
                                      maxlen = 2
                             )) %>% DATAFRAME()

churn_1 <- apriori(vps, 
                 parameter = list(sup = 0.001, 
                                  conf = 0.6,
                                  target="rules",
                                  minlen = 1,
                                  maxlen = 2
                 ), appearance = list(rhs = c("is_churn=1"))) %>% DATAFRAME()

churn_0 <- apriori(vps, 
                 parameter = list(sup = 0.1, 
                                  conf = 0.6,
                                  target="rules",
                                  minlen = 1,
                                  maxlen = 2
                 ), appearance = list(rhs = c("is_churn=0"))) %>% DATAFRAME()


