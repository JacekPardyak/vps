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
# tune parameters

library(tidyverse)
library(tidymodels)

# read churn historical data
vps <- read_csv("./data/vps_churn_data.txt") %>%
  mutate(is_churn = factor(ifelse(is_churn == 0, "No", "Yes")))

vps %>% write_csv("./data/vps_churn_data.csv")

# check proportions
vps %>% 
  count(is_churn) %>% 
  mutate(prop = n/sum(n))

# check missing values
is.na(vps) %>% colSums()

# show table in Viewer
library(gt)
vps %>% 
  slice_head(n = 4) %>% 
  gt()

# calculate and display correlation matrix
library(corrplot)
read_csv("./data/vps_churn_data.txt") %>% cor() %>% corrplot(method = 'color')

# data overview
library(skimr)
vps %>% skim()

# plot histograms
library(GGally)

read_csv("./data/vps_churn_data.txt") %>% select(c(1:10, 23)) %>%
  ggscatmat(alpha = 0.2)

read_csv("./data/vps_churn_data.txt") %>% select(c(11:23)) %>%
  ggscatmat(alpha = 0.2)

# plot whiskers 
vps %>% 
  select(c(1:5, 23)) %>% 
  ggpairs()
vps %>% 
  select(c(6:10, 23)) %>% 
  ggpairs()
vps %>% 
  select(c(11:15, 23)) %>% 
  ggpairs()
vps %>% 
  select(c(16: 23)) %>% 
  ggpairs()



# explore the data
# strong positive correlation
vps %>% names()
vps %>% 
  ggplot(aes(x = disk_ops_read_mean_m_3, y = disk_octets_read_mean_m_3)) +
  geom_point(color = "cornflowerblue") + labs(title = "The two most correlated variables")



vps %>% 
  ggplot(aes(x = cpu_load_max_gradient, y = cpu_load_monthly_mean_delta)) +
  geom_point(color = "cornflowerblue")

# strong negative correlation
vps %>% 
  ggplot(aes(x = disk_octets_write_mean_m_3, y = network_rx_max_gradient)) +
  geom_point(color = "cornflowerblue")

vps %>% 
  ggplot(aes(x = disk_octets_write_mean_m_3, y = network_rx_max_gradient)) +
  geom_point(aes(color = is_churn), 
             alpha = 0.4)

vps %>% 
  ggscatmat(columns = c(2:7),
            color = 'is_churn', 
            corMethod = "spearman",
            alpha=0.2)

?ggscatmat


# split data into train and test
set.seed(1234)
vps_split <- initial_split(vps, prop = 3/4, strata = is_churn)
vps_split

# Create data frames for the two sets:
train_data <- training(vps_split) 
test_data <- testing(vps_split)


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

vps_recipe <-
  recipe(is_churn ~.,
         data = train_data) %>%
  update_role(id, new_role = "ID") %>% 
  step_log(all_nominal(), -all_outcomes()) %>% 
  step_naomit(everything(), skip = TRUE) %>% 
  step_novel(all_nominal(), -all_outcomes()) %>%
  step_normalize(all_numeric(), -all_outcomes(), -id) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_numeric(), -all_outcomes(), -id) %>%
  step_corr(all_predictors(), threshold = 0.7, method = "spearman") 

summary(vps_recipe)

prepped_data <- 
  vps_recipe %>% # use the recipe object
  prep() %>% # perform the recipe on training data
  juice() # extract only the preprocessed dataframe 

# Take a look at the data structure:
glimpse(prepped_data)

prepped_data %>% 
    ggscatmat(corMethod = "spearman",
              alpha=0.2)


#vps_recipe <- training(vps_split) %>% # on which data
#  recipe(is_churn ~.) %>%
#  step_rm(id) %>% # remove id
#  step_corr(all_predictors()) %>% # remove variables highly correlated with other vars
#  step_center(all_predictors(), -all_outcomes()) %>% # make vars to be of mean zero
#  step_scale(all_predictors(), -all_outcomes()) %>% # make vars standard dev of 1
#  prep() # execute transformations

vps_recipe

# cross validation
set.seed(100)
cv_folds <-
  vfold_cv(train_data, 
           v = 5, 
           strata = is_churn)

# train models
# Logistic regression
log_spec <- # your model specification
  logistic_reg() %>%  # model type
  set_engine(engine = "glm") %>%  # model engine
  set_mode("classification") # model mode

# Show your model specification
log_spec

log_wflow <- # new workflow object
  workflow() %>% # use workflow function
  add_recipe(vps_recipe) %>%   # use the new recipe
  add_model(log_spec)   # add your model spec

# show object
log_wflow

library(yardstick)

log_res <- 
  log_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc), # , sens, spec
    control = control_resamples(
      save_pred = TRUE)
  )


# save model coefficients for a fitted model object from a workflow

get_model <- function(x) {
  extract_fit_parsnip(x) %>% tidy()
}

# same as before with one exception
log_res_2 <- 
  log_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc), # , sens, spec
    control = control_resamples(
      save_pred = TRUE,
      extract = get_model) # use extract and our new function
  )

log_res_2$.extracts[[1]]

log_res_2$.extracts[[1]][[1]]


all_coef <- map_dfr(log_res_2$.extracts, ~ .x[[1]][[1]])
# Show all of the resample coefficients for a single predictor:
filter(all_coef, term == "cpu_load_mean_m_3")

log_res %>%  collect_metrics(summarize = TRUE)


log_pred <- 
  log_res %>%
  collect_predictions()

log_pred %>% 
  conf_mat(is_churn, .pred_class)


log_pred %>% 
  group_by(id) %>% # id contains our folds
  roc_curve(is_churn, .pred_Yes) %>% 
  autoplot()


log_pred %>% 
  ggplot() +
  geom_density(aes(x = .pred_Yes, 
                   fill = is_churn), 
               alpha = 0.5)


###  
# random forest
library(ranger)

rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

rf_wflow <-
  workflow() %>%
  add_recipe(vps_recipe) %>% 
  add_model(rf_spec) 


rf_res <-
  rf_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens),
    control = control_resamples(save_pred = TRUE)
  ) 

rf_res %>%  collect_metrics(summarize = TRUE)


## xgboost
library(xgboost)

xgb_spec <- 
  boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xgb_wflow <-
  workflow() %>%
  add_recipe(vps_recipe) %>% 
  add_model(xgb_spec)


xgb_res <- 
  xgb_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens),
    control = control_resamples(save_pred = TRUE)
  ) 

xgb_res %>% collect_metrics(summarize = TRUE)

# 4.2.4 K-nearest neighbor
knn_spec <- 
  nearest_neighbor(neighbors = 4) %>% # we can adjust the number of neighbors 
  set_engine("kknn") %>% 
  set_mode("classification") 

knn_wflow <-
  workflow() %>%
  add_recipe(vps_recipe) %>% 
  add_model(knn_spec)

knn_res <- 
  knn_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens),
    control = control_resamples(save_pred = TRUE)
  ) 

knn_res %>% collect_metrics(summarize = TRUE)

# Neural network

library(keras)

nnet_spec <-
  mlp() %>%
  set_mode("classification") %>% 
  set_engine("keras", verbose = 0) 

nnet_wflow <-
  workflow() %>%
  add_recipe(housing_rec) %>% 
  add_model(nnet_spec)

nnet_res <- 
  nnet_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      recall, precision, f_meas, 
      accuracy, kap,
      roc_auc, sens),
    control = control_resamples(save_pred = TRUE)
  ) 

nnet_res %>% collect_metrics(summarize = TRUE)

# compare models

log_metrics <- 
  log_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Logistic Regression") # add the name of the model to every row

rf_metrics <- 
  rf_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Random Forest")

xgb_metrics <- 
  xgb_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "XGBoost")

knn_metrics <- 
  knn_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Knn")

nnet_metrics <- 
  nnet_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Neural Net")

# create dataframe with all models
model_compare <- bind_rows(
  log_metrics,
  rf_metrics,
  xgb_metrics,
  knn_metrics,
  nnet_metrics
) 

# change data structure
model_comp <- 
  model_compare %>% 
  select(model, .metric, mean, std_err) %>% 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) 

# show mean F1-Score for every model
model_comp %>% 
  arrange(mean_f_meas) %>% 
  mutate(model = fct_reorder(model, mean_f_meas)) %>% # order results
  ggplot(aes(model, mean_f_meas, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") +
  geom_text(
    size = 3,
    aes(label = round(mean_f_meas, 2), y = mean_f_meas + 0.08),
    vjust = 1
  )

# show mean area under the curve (auc) per model
model_comp %>% 
  arrange(mean_roc_auc) %>% 
  mutate(model = fct_reorder(model, mean_roc_auc)) %>%
  ggplot(aes(model, mean_roc_auc, fill=model)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Blues") + 
  geom_text(
    size = 3,
    aes(label = round(mean_roc_auc, 2), y = mean_roc_auc + 0.08),
    vjust = 1
  )

model_comp %>% slice_max(mean_f_meas)

last_fit_rf <- last_fit(rf_wflow, 
                        split = vps_split,
                        metrics = metric_set(
                          recall, precision, f_meas, 
                          accuracy, kap,
                          roc_auc, sens)
)


last_fit_rf %>% collect_metrics(summarize = TRUE)

library(vip)

last_fit_rf %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 10)


last_fit_rf %>%
    collect_predictions() %>% 
  conf_mat(is_churn, .pred_class) %>% 
  autoplot(type = "heatmap")

last_fit_rf %>% 
  collect_predictions() %>% 
  roc_curve(is_churn, .pred_No) %>% 
  autoplot()


### make predictions
production <- read_csv("./data/vps_test_data.txt")
vps_production <- vps_recipe %>%
  bake(production)


last_fit_rf %>% predictions(production)

fit_workflow <- fit(rf_wflow, train_data)

# This will automatically `bake()` the recipe on `testing`,
# applying the log step to `disp`, and then fit the regression.
predict(fit_workflow, production)





tmp <- predict(vps_rf, vps_production) %>% 
  rename(is_churn = .pred_class) %>%
  mutate(is_churn = ifelse(is_churn == "Yes", 1, 0 ))

production %>% select(! one_of('is_churn')) %>%
  bind_cols(tmp) %>%
  write_csv("./data/vps_test_data_pred.txt")





# Execute the pre-processing
# prepare testing data
vps_testing <- vps_recipe %>%
  bake(testing(vps_split))

glimpse(vps_testing)

# prepare training data
vps_training <- juice(vps_recipe)

glimpse(vps_training)

# train model using random forest with 2 different engines

vps_svm <- svm_poly(mode = "classification") %>% 
  set_engine("kernlab") %>%
  fit(is_churn ~ ., data = vps_training)

vps_lreg <- 
  logistic_reg(mode = "classification",
               engine = "glm") %>% 
  fit(is_churn ~ ., data = vps_training)

install.packages('keras')

library(keras)

nnet_spec <-
  mlp() %>%
  set_mode("classification") %>% 
  set_engine("keras", verbose = 0)  %>% 
  fit(is_churn ~ ., data = vps_training)

?logistic_reg

# making predictions
predict(xgb_spec, vps_testing, type = "prob") %>%
  bind_cols(predict(xgb_spec, vps_testing)) %>%
  bind_cols(select(vps_testing, is_churn)) %>%
  metrics(is_churn, .pred_No, estimate = .pred_class)


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

vps_probs <- vps_lreg %>%
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




