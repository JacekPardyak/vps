# devtools::install_github("OpenIntroStat/OIsurv")
library(OIsurv) # for data
library(survival)
library(KMsurv)
library(tidyverse)
# Attach the data
data(tongue)
n = 1000
df <- data.frame(id = c(1:n)) %>% 
  mutate(type = sample(c(1, 2), n, replace=TRUE)) %>%
  mutate(type = as.factor(type)) %>%
  mutate(delta = sample(c(0, 1), n, replace=TRUE)) %>%
  mutate(time = sample(c(1:365), n, replace=TRUE)) %>%
  mutate(gender = sample(c("f","m"), n, replace=TRUE)) %>% 
  mutate(gender = as.factor(gender)) %>%
  mutate(category = sample(c("a","b","c"), n, replace=TRUE)) %>% 
  mutate(category = as.factor(category)) 

sur_var = "category"
selectedData <- df %>% select(all_of(sur_var)) %>% distinct() %>% pull()



formula = as.formula(paste("Surv(time,delta) ~ ", sur_var))
runSur <- survfit(formula = formula, data=df)

plot(runSur, col = c(1:length(levels(selectedData))), xscale=365.25, lwd=2, mark.time=TRUE,
     xlab="Years since study entry", ylab="Survival")
legend("bottomleft", levels(selectedData),
         col = c(1:length(levels(selectedData))), lwd=2, bty='n')

length(levels(selectedData))


summary(df)


sample(c(1:365), 80, replace = T)
