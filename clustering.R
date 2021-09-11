# apriori in R
library(tidyverse)

vps <- read_csv("./data/vps_churn_data.txt") 

wineTrain <- vps %>% select(! one_of(c('id', 'is_churn')))

wineK3 <- kmeans(x=wineTrain, centers=2)
wineK3

library(useful)

plot(wineK3, data=wineTrain)

plot(wineK3, data=vps, class="is_churn")

wineK3N25 <- kmeans(wineTrain, centers=10, nstart=25) # see the cluster sizes with 1 start
  
wineK3$size

wineBest <- FitKMeans(wineTrain, max.clusters=10, nstart=25, seed=278613) 
wineBest
PlotHartigan(wineBest)
table(vps$is_churn, wineK3N25$cluster)

wineH <- hclust(d=dist(wineTrain)) 
plot(wineH)
