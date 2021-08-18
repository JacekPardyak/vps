library(tidyverse)

data <- read_csv("./data/vps_churn_data.txt") %>%
  mutate(is_churn = factor(ifelse(is_churn == 0, "No", "Yes")))

production <- read_csv("./data/vps_test_data.txt")

# 

# problem understanding

# data understanding

glimpse(data)

# (check missing)
data %>% is.na() %>% colSums()

# (... check if first var could be a good predictor... )
data %>%
  ggplot(aes(x = cpu_load_mean_m_3, fill = is_churn)) +
  geom_density(alpha = 0.3) #+ 
#  labs(x= "Salary",

# (check correlations)

# (...)

data %>% 
  count(is_churn) %>% 
  mutate(prop = n/sum(n))

# (check if last, categorized var could be a good predictor)
library(ggmosaic)
data %>% 
  mutate(network_tx_max_gradient = cut(network_tx_max_gradient, breaks=c(-Inf, 0, Inf),
                                       labels=c("negative","positive"))) %>%
  ggplot()  +
  geom_mosaic(aes(x = product( is_churn , network_tx_max_gradient), fill = is_churn))

# data preparation
library(tidymodels)
set.seed(123)
splits      <- initial_split(data, strata = is_churn)

telco_other <- training(splits)
telco_test  <- testing(splits)

# training set proportions by children
telco_other %>% 
  count(is_churn) %>% 
  mutate(prop = n/sum(n))

# test set proportions by children
telco_test  %>% 
  count(is_churn) %>% 
  mutate(prop = n/sum(n))

set.seed(234)
val_set <- validation_split(telco_other, 
                            strata = is_churn, 
                            prop = 0.80)
val_set




# building model
# (Null model)
null_model() %>% 
  set_engine("parsnip") %>% 
  set_mode("classification") %>% 
  translate()

nm_model <- null_model(mode = "classification") %>% 
  set_engine("parsnip")
nm_model %>%    
  parameters()

nm_recipe <- 
  recipe(is_churn ~ ., data = telco_other) %>% 
  step_rm(id) %>% 
  step_normalize(all_predictors()) 

    step_discretize(tenure) %>%
  step_discretize(MonthlyCharges) %>%
  step_mutate(SeniorCitizen = factor(ifelse(SeniorCitizen == 0, "No", "Yes"))) %>% 
  step_mutate(MultipleLines = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
  step_mutate(OnlineSecurity = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
  step_mutate(OnlineBackup = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
  step_mutate(DeviceProtection = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
  step_mutate(TechSupport = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
  step_mutate(StreamingTV = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
  step_mutate(StreamingMovies = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
  step_rm(TotalCharges) %>% 
  step_rm(customerID) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# cookie
nm_cookie <- bake(nm_recipe %>% prep(), new_data = NULL)

# workflow
nm_workflow <- 
  workflow() %>% 
  add_model(nm_model) %>% 
  add_recipe(nm_recipe)
nm_workflow

nm_res <- 
  nm_workflow %>% 
  tune_grid(val_set,
            grid = 12,
            control = control_grid(save_pred = TRUE, verbose = TRUE),
            metrics = metric_set(roc_auc))
nm_res



nm_res %>% 
  show_best(metric = "roc_auc")



nm_best <- 
  nm_res %>% 
  collect_metrics()
nm_best

nm_auc <- 
  nm_res %>% 
  collect_predictions(parameters = nm_best) %>% 
  roc_curve(is_churn, .pred_No) %>% 
  mutate(model = "Null model")

autoplot(nm_auc)


# ( ... svm model ... )
svm_poly() %>% 
  set_engine("kernlab") %>% 
  set_mode("classification") %>% 
  translate()

sv_model <- svm_poly(mode = "classification") %>% 
  set_engine("kernlab")
sv_model %>%    
  parameters()

sv_recipe <- 
  recipe(is_churn ~ ., data = telco_other) %>% 
  step_rm(id) %>% 
  step_normalize(all_predictors()) 

#  recipe(Churn ~ ., data = telco_other) %>% 
#  step_discretize(tenure) %>%
#  step_discretize(MonthlyCharges) %>%
#  step_mutate(SeniorCitizen = factor(ifelse(SeniorCitizen == 0, "No", "Yes"))) %>% 
#  step_mutate(MultipleLines = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(OnlineSecurity = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(OnlineBackup = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(DeviceProtection = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(TechSupport = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(StreamingTV = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(StreamingMovies = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_rm(TotalCharges) %>% 
#  step_rm(customerID) %>% 
#  step_dummy(all_nominal(), -all_outcomes()) %>% 
#  step_zv(all_predictors()) %>% 
#  step_normalize(all_predictors())

# cookie
sv_cookie <- bake(sv_recipe %>% prep(), new_data = NULL)

# workflow
sv_workflow <- 
  workflow() %>% 
  add_model(sv_model) %>% 
  add_recipe(sv_recipe)
sv_workflow

sv_res <- 
  sv_workflow %>% 
  tune_grid(val_set,
            grid = 12,
            control = control_grid(save_pred = TRUE, verbose = TRUE),
            metrics = metric_set(roc_auc))
sv_res

sv_res %>% 
  show_best(metric = "roc_auc")



sv_best <- 
  sv_res %>% 
  collect_metrics()
sv_best

sv_auc <- 
  sv_res %>% 
  collect_predictions(parameters = sv_best) %>% 
  roc_curve(is_churn, .pred_No) %>% 
  mutate(model = "SVM model")

autoplot(sv_auc)


# ( ... ... )

# logistic regression
lr_model <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

# pre proces with recipies
lr_recipe <- 
  recipe(is_churn ~ ., data = telco_other) %>% 
  step_rm(id) %>% 
  step_normalize(all_predictors()) 

#  recipe(Churn ~ ., data = telco_other) %>% 
#  step_discretize(tenure) %>%
#  step_discretize(MonthlyCharges) %>%
#  step_mutate(SeniorCitizen = factor(ifelse(SeniorCitizen == 0, "No", "Yes"))) %>% 
#  step_mutate(MultipleLines = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(OnlineSecurity = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(OnlineBackup = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(DeviceProtection = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(TechSupport = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(StreamingTV = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(StreamingMovies = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_rm(TotalCharges) %>% 
#  step_rm(customerID) %>%
#  step_dummy(all_nominal(), -all_outcomes()) %>% 
#  step_zv(all_predictors()) %>% 
#  step_normalize(all_predictors())

# cookie
lr_cookie <- bake(lr_recipe %>% prep(), new_data = NULL)

lr_workflow <- 
  workflow() %>% 
  add_model(lr_model) %>% 
  add_recipe(lr_recipe)

lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

lr_reg_grid %>% top_n(-5) # lowest penalty values
lr_reg_grid %>% top_n(5)  # highest penalty values

lr_res <- 
  lr_workflow %>% 
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE, verbose = TRUE),
            metrics = metric_set(roc_auc))

lr_res
class(lr_res)

lr_plot <- 
  lr_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_plot 

top_models <-
  lr_res %>% 
  show_best("roc_auc", n = 15) %>% 
  arrange(penalty) 
top_models

lr_best <- 
  lr_res %>% 
  collect_metrics() %>% 
  arrange(penalty) %>% 
  slice(12)
lr_best






lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(is_churn, .pred_No) %>% 
  mutate(model = "Logistic Regression")

autoplot(lr_auc)

# ( ... random forest ...)

cores <- parallel::detectCores()
cores

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("classification")

rf_recipe <- 
  recipe(is_churn ~ ., data = telco_other) %>% 
  step_rm(id) %>% 
  step_normalize(all_predictors()) 

#  recipe(Churn ~ ., data = telco_other) %>% 
#  step_discretize(tenure) %>%
#  step_discretize(MonthlyCharges) %>%
#  step_mutate(SeniorCitizen = factor(ifelse(SeniorCitizen == 0, "No", "Yes"))) %>% 
#  step_mutate(MultipleLines = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(OnlineSecurity = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(OnlineBackup = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(DeviceProtection = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(TechSupport = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(StreamingTV = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_mutate(StreamingMovies = factor(ifelse(MultipleLines == "Yes", "Yes", "No"))) %>%
#  step_rm(TotalCharges) %>% 
#  step_rm(customerID) %>% 
#  step_dummy(all_nominal(), -all_outcomes()) %>% 
#  step_zv(all_predictors()) %>% 
#  step_normalize(all_predictors())

rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)

rf_mod

# show what will be tuned
rf_mod %>%    
  parameters()

set.seed(345)
rf_res <- 
  rf_workflow %>% 
  tune_grid(val_set,
            grid = 12,
            control = control_grid(save_pred = TRUE, verbose = TRUE),
            metrics = metric_set(roc_auc))
rf_res

rf_res %>% 
  show_best(metric = "roc_auc")

autoplot(rf_res)

rf_best <- 
  rf_res %>% 
  select_best(metric = "roc_auc")
rf_best

rf_res %>% 
  collect_predictions()

rf_auc <- 
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(is_churn, .pred_No) %>% 
  mutate(model = "Random Forest")

rf_auc

bind_rows(nm_auc, rf_auc, lr_auc, sv_auc) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_viridis_d(option = "plasma", end = .6)





# evaluating model

# deploying model

# cookie

rf_workflow 

rf_best %>%
  fit(is_churn ~ ., data = telco_other)



production <- read_csv("./data/vps_test_data.txt")

iris_production <- iris_recipe %>%
  bake(production)

predict(iris_ranger, iris_production)


cookie <- bake(rf_recipe %>% prep(), new_data = production)

predict(rf_best, new_data = cookie)



library(dplyr)

lm_model <-
  linear_reg() %>%
  set_engine("lm") %>%
  fit(mpg ~ ., data = mtcars %>% dplyr::slice(11:32))

pred_cars <-
  mtcars %>%
  dplyr::slice(1:10) %>%
  dplyr::select(-mpg)

predict(lm_model, pred_cars)

lm_model

rf_best

lm_model <-
  linear_reg() %>%
  set_engine("lm") %>%
  fit(mpg ~ ., data = mtcars %>% dplyr::slice(11:32))


#### ----------- 
library(vip)         # for variable importance plots

# the last model
last_rf_mod <- 
  rand_forest(mtry = 8, min_n = 7, trees = 1000) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("classification")

# the last workflow
last_rf_workflow <- 
  rf_workflow %>% 
  update_model(last_rf_mod)

# the last fit
set.seed(345)
last_rf_fit <- 
  last_rf_workflow %>% 
  last_fit(splits)

last_rf_fit

last_rf_fit %>% 
  collect_metrics()

last_rf_fit %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>% 
  vip(num_features = 20)

last_rf_fit %>% 
  collect_predictions() %>% 
  roc_curve(is_churn, .pred_No) %>% 
  autoplot()


?collect_predictions





