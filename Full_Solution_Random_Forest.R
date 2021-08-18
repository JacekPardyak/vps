
# plan
# keep calm
# 0. data preparation
# 1. split data
# 2. train model on train
# 3. validate on test data
# 4. make predictions for 'production' data
# dzisiaj tylko demonstracja z : https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/
# jutro, czwartek - inne modele
# further steps 
# demonstration how to use it with spark


library(tidymodels)

# read churn historicla data
vps <- read_csv("./data/vps_churn_data.txt") %>%
  mutate(is_churn = factor(ifelse(is_churn == 0, "No", "Yes")))

# check proportions
vps %>% 
  count(is_churn) %>% 
  mutate(prop = n/sum(n))

# split data into train and test
vps_split <- initial_split(vps, prop = 0.6, strata = is_churn)
vps_split

# check the observations for training
vps_split %>%
  training() %>%
  glimpse()

# check proportions in training split
vps_split %>%
  training() %>% 
  count(is_churn) %>% 
  mutate(prop = n/sum(n))

# check proportions in training split
vps_split %>%
  testing() %>% 
  count(is_churn) %>% 
  mutate(prop = n/sum(n))

# prepare recipe 
vps_recipe <- training(vps_split) %>% # on which data
  recipe(is_churn ~.) %>%
  step_rm(id) %>% # remove id
  step_corr(all_predictors()) %>% # remove variables highly correlated with other vars
  step_center(all_predictors(), -all_outcomes()) %>% # make vars to be of mean zero
  step_scale(all_predictors(), -all_outcomes()) %>% # make vars standard dev of 1
  prep() # execute transformations

vps_recipe

# Execute the pre-processing
# prepare testing data
vps_testing <- vps_recipe %>%
  bake(testing(vps_split))

glimpse(vps_testing)

# prepare training data
vps_training <- juice(vps_recipe)

glimpse(vps_training)

# train model using random forest with 2 different engines
vps_ranger <- rand_forest(trees = 100, mode = "classification") %>%
  set_engine("ranger") %>%
  fit(is_churn ~ ., data = vps_training)

vps_rf <-  rand_forest(trees = 100, mode = "classification") %>%
  set_engine("randomForest") %>%
  fit(is_churn ~ ., data = vps_training)

# making predictions

predict(vps_ranger, vps_testing)

# truth and predicted values in one table
vps_ranger %>%
  predict(vps_testing) %>%
  bind_cols(vps_testing) %>%
  glimpse()

# model validation

vps_ranger %>%
  predict(vps_testing) %>%
  bind_cols(vps_testing) %>%
  metrics(truth = is_churn, estimate = .pred_class)

vps_rf %>%
  predict(vps_testing) %>%
  bind_cols(vps_testing) %>%
  metrics(truth = is_churn, estimate = .pred_class) # this is worse


# use trained model to make predictions on production data
production <- read_csv("./data/vps_test_data.txt")

vps_production <- vps_recipe %>%
  bake(production)

tmp <- predict(vps_ranger, vps_production) %>% rename(is_churn = .pred_class)

tmp %>% 
  count(is_churn) %>% 
  mutate(prop = n/sum(n))

# Per classifier metrics
vps_ranger %>%
  predict(vps_testing, type = "prob") %>%
  glimpse()

vps_probs <- vps_ranger %>%
  predict(vps_testing, type = "prob") %>%
  bind_cols(vps_testing)

glimpse(vps_probs)

# calculate curve methods. In this case we are using gain_curve().

vps_probs %>%
  gain_curve(is_churn, .pred_No) %>%
  glimpse()

vps_probs %>%
  gain_curve(is_churn, .pred_No) %>%
  autoplot()

vps_probs%>%
  roc_curve(is_churn, .pred_No) %>%
  autoplot()



# combine two prediction modes
predict(vps_ranger, vps_testing, type = "prob") %>%
  bind_cols(predict(vps_ranger, vps_testing)) %>%
  bind_cols(select(vps_testing, is_churn)) %>%
  glimpse()


predict(vps_ranger, vps_testing, type = "prob") %>%
  bind_cols(predict(vps_ranger, vps_testing)) %>%
  bind_cols(select(vps_testing, is_churn)) %>%
  metrics(is_churn, .pred_No, estimate = .pred_class)




