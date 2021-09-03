library(tidyverse)
library(tidymodels)

# read churn historical data
vps <- read_csv("./data/vps_churn_data.txt") %>%
  mutate(is_churn = factor(ifelse(is_churn == 0, "No", "Yes")))

# check outiers
vars <- vps %>% names()
vars

disk_ops <- vars[agrep('disk_ops', vars)] # 6
disk_octets <- vars[agrep('disk_octets', vars)] # 6
cpu <- vars[agrep('cpu_', vars)] # 3
network <- vars[agrep('network', vars)] # 6

vps_longer <- vps %>%
  pivot_longer(., cols = !one_of('id', 'is_churn'), names_to = "Variable", values_to = "Value")

vps_longer  %>% filter(Variable %in% disk_ops) %>%
    ggplot(aes(x = Variable, y = Value, fill = is_churn)) +
    geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4)
  
vps_longer  %>% filter(Variable %in% disk_octets) %>%
  ggplot(aes(x = Variable, y = Value, fill = is_churn)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4)

vps_longer  %>% filter(Variable %in% cpu) %>%
  ggplot(aes(x = Variable, y = Value, fill = is_churn)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4)

vps_longer  %>% filter(Variable %in% network) %>%
  ggplot(aes(x = Variable, y = Value, fill = is_churn)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 8, outlier.size = 4)

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


vps %>% 
  ggplot(aes(x = disk_octets_write_mean_m_3, y = network_rx_max_gradient)) +
  geom_point(aes(color = is_churn), 
             alpha = 0.4)

### Start tuning parameters
# split data into train and test
set.seed(123)
vps_split <- initial_split(vps, strata = is_churn)
vps_train <- training(vps_split)
vps_test <- testing(vps_split)

# prepare recipe 
vps_rec <- recipe(is_churn ~ ., data = vps_train) %>%
  update_role(id, new_role = "ID") %>%
  step_corr(all_predictors()) %>% 
  step_normalize(all_numeric(), -all_outcomes(), -id)
#  step_other(species, caretaker, threshold = 0.01) %>%
#  step_other(site_info, threshold = 0.005) %>%
#  step_dummy(all_nominal(), -all_outcomes()) %>%
#  step_date(date, features = c("year")) %>%
#  step_rm(date) %>%
#  step_downsample(legal_status)

vps_prep <- vps_rec %>% prep()
#juiced <- juice(vps_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")


tune_wf <- workflow() %>%
  add_recipe(vps_rec) %>%
  add_model(tune_spec)

set.seed(234)
vps_folds <- vfold_cv(vps_train)

doParallel::registerDoParallel()

set.seed(345)
tune_res <- tune_grid(
  tune_wf,
  resamples = vps_folds,
  grid = 20
)

tune_res

tune_res %>%
collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")


rf_grid <- grid_regular(
  mtry(range = c(1, 3)),
  min_n(range = c(1, 5)),
  levels = 5
)

rf_grid

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = vps_folds,
  grid = rf_grid
)

regular_res

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")


best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf

library(vip)

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(is_churn ~ .,
      data = juice(vps_prep) %>% select(-id)
  ) %>%
  vip(geom = "point")

final_wf <- workflow() %>%
  add_recipe(vps_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(vps_split)

final_res %>%
  collect_metrics()

final_res %>%
  collect_predictions() %>% 
  conf_mat(is_churn, .pred_class) %>% 
  autoplot(type = "heatmap")



final_res %>%
  collect_predictions() %>%
  mutate(correct = case_when(
    is_churn == .pred_class ~ "Correct",
    TRUE ~ "Incorrect"
  )) #%>%
  bind_cols(vps_test) %>%
  ggplot(aes(disk_octets_write_mean_m_3, y = network_rx_max_gradient, color = correct)) + #network_tx_mean_m_3
  geom_point(size = 0.5, alpha = 0.5) +
  labs(color = NULL) +
  scale_color_manual(values = c("gray80", "darkred"))

### 

production <- read_csv("./data/vps_test_data.txt")
vps_production <- vps_rec %>% prep() %>% 
  bake(production)
vps_production %>% glimpse()

tmp <- final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(is_churn ~ ., data = juice(vps_prep)) %>% predict(vps_production) %>% 
  rename(is_churn = .pred_class) %>%
  mutate(is_churn = ifelse(is_churn == "Yes", 1, 0 ))

production %>% select(! one_of('is_churn')) %>%
  bind_cols(tmp) %>%
  write_csv("./data/vps_test_data_pred_part_3.txt")

table(tmp$is_churn)


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
  boost_vps() %>% 
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




