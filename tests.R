library(tidyverse)
library(sparklyr)
#spark_install(version = "2.1.0")

sc <- spark_connect(master = "local")

vps <- read_csv("./data/vps_churn_data.txt") # %>% mutate(is_churn = factor(ifelse(is_churn == 0, "No", "Yes")))


numericData <- vps

# Calculate correlation matrix
descrCor <- cor(numericData)

# Print correlation matrix and look at max correlation
print(descrCor)
summary(descrCor[upper.tri(descrCor)])

# Check Correlation Plot
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(descrCor, cutoff=0.7)

# print indexes of highly correlated attributes
print(highlyCorrelated)

# Indentifying Variable Names of Highly Correlated Variables
highlyCorCol <- colnames(numericData)[highlyCorrelated]

# Print highly correlated attributes
highlyCorCol

# Remove highly correlated variables and create a new dataset
dat3 <- dat2[, -which(colnames(dat2) %in% highlyCorCol)]
dim(dat3)






vps_tbl <- copy_to(sc, vps, overwrite = TRUE)

# random forest , evaluation on training data
rf_model <- vps_tbl %>%
  ml_random_forest(is_churn ~ network_tx_mean_m_3 + cpu_load_mean_m_3, type = "classification")
## * No rows dropped by 'na.omit' call
rf_predict <- ml_predict(rf_model, vps_tbl) %>%
  ft_string_indexer("is_churn", "is_churn_idx") %>%
  collect

table(rf_predict$is_churn_idx, rf_predict$prediction)

# random forest , evaluation on test data
partitions <- tbl(sc, "vps") %>%
  sdf_random_split(training = 0.6, test = 0.4, seed = 1099)

rf_model <- partitions$training %>%
  ml_random_forest(is_churn ~ network_tx_mean_m_3 + cpu_load_mean_m_3, type = "classification")

rf_predict <- ml_predict(rf_model, partitions$test) %>%
  ft_string_indexer("is_churn", "is_churn_idx") %>%
  collect
table(rf_predict$is_churn_idx, rf_predict$prediction)

# random forest , evaluation on test data, full formula
partitions <- tbl(sc, "vps") %>%
  sdf_random_split(training = 0.6, test = 0.4, seed = 1099)

rf_model <- partitions$training %>%
  ml_random_forest(is_churn ~ ., type = "classification")

rf_predict <- ml_predict(rf_model, partitions$test) %>%
  ft_string_indexer("is_churn", "is_churn_idx") %>%
  collect
table(rf_predict$is_churn_idx, rf_predict$prediction)

# random forest , evaluation on test data, full formula
partitions <- tbl(sc, "vps") %>%
  sdf_random_split(training = 0.6, test = 0.4, seed = 1099)

rf_model <- partitions$training %>%
  ml_random_forest(is_churn ~ ., type = "classification")

rf_predict <- ml_predict(rf_model, partitions$test) %>%
  ft_string_indexer("is_churn", "is_churn_idx") %>%
  collect
table(rf_predict$is_churn_idx, rf_predict$prediction)


# random forest , evaluation on test data, drop highly correlated vars
library(caret)

vars <- vps %>% names()
ind <- vps %>% cor() #%>% findCorrelation(cutoff=0.6)
ind %>% tibble() %>% summarise_if(is.numeric, min)

vars <- setdiff(vars, vars[ind])
vars

vps %>% cor() %>% corrplot(order = "FPC", method = "color", type = "lower",
                           tl.cex = 0.7, tl.col = rgb(0, 0, 0))
vps %>% cor() %>% findCorrelation(cutoff=0.6) -> idx
vars[idx]

formula <- formula(paste("is_churn ~ ", paste(vars[c(-1,-length(vars))], collapse = " + ")))
formula

partitions <- tbl(sc, "vps") %>% 
  mutate(cpu_load_mean_m_3 = ft_standard_scaler(cpu_load_mean_m_3)) %>%
  sdf_random_split(training = 0.6, test = 0.4, seed = 888)

rf_model <- partitions$training %>%
  ml_random_forest(formula, type = "classification")

rf_predict <- ml_predict(rf_model, partitions$test) %>%
  ft_string_indexer("is_churn", "is_churn_idx") %>%
  collect
table(rf_predict$is_churn_idx, rf_predict$prediction)


features <- c("cpu_load_mean_m_3", "disk_ops_read_mean_m_3")

tbl(sc, "vps")  %>%
  ft_standard_scaler(
    input_col = "cpu_load_mean_m_3",
    output_col = "cpu_load_mean_m_3_temp",
    with_mean = TRUE
  )
  
  
  
tbl(sc, "vps") %>%
  ft_vector_assembler(
    input_col = "cpu_load_mean_m_3",
    output_col = "cpu_load_mean_m_3_temp") %>%
  ft_standard_scaler(
    input_col = "cpu_load_mean_m_3_temp",
    output_col = "cpu_load_mean_m_3_t",
    with_mean = TRUE
  )

  
sc <- spark_connect(master = "local")
iris_tbl <- sdf_copy_to(sc, iris, name = "iris_tbl", overwrite = TRUE)

features <- c("Sepal_Length", "Sepal_Width", "Petal_Length", "Petal_Width")

iris_tbl %>%
  ft_vector_assembler(
    input_col = features,
    output_col = "features_temp"
  ) %>%
  ft_standard_scaler(
    input_col = "features_temp",
    output_col = "features",
    with_mean = TRUE
  )  
  
iris_tbl 
  
  ft_vector_assembler(
    input_col = features,
    output_col = "features_temp"
  ) %>%
  ft_standard_scaler(
    input_col = "features_temp",
    output_col = "features",
    with_mean = TRUE
  ) %>%
  collect





tbl(sc, "vps")  %>%
  ft_vector_assembler(
    input_col = features,
    output_col = "features_temp"
  ) 




tbl(sc, "vps")

?ft_standard_scaler


dplyr::src_tbls(sc)

spark_disconnect(sc)




library("nycflights13")
library("Lahman")

library(tidyverse)
iris_tbl <- copy_to(sc, iris)
flights_tbl <- copy_to(sc, nycflights13::flights, "flights")
batting_tbl <- copy_to(sc, Lahman::Batting, "batting")
dplyr::src_tbls(sc)

# filter by departure delay and print the first few records
flights_tbl %>% filter(dep_delay == 2)


delay <- flights_tbl %>%
  group_by(tailnum) %>%
  summarise(count = n(), dist = mean(distance), delay = mean(arr_delay)) %>%
  filter(count > 20, dist < 2000, !is.na(delay)) %>%
  collect

# plot delays
library(ggplot2)
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area(max_size = 2)

batting_tbl %>%
  select(playerID, yearID, teamID, G, AB:H) %>%
  arrange(playerID, yearID, teamID) %>%
  group_by(playerID) %>%
  filter(min_rank(desc(H)) <= 2 & H > 0)


library(DBI)
iris_preview <- dbGetQuery(sc, "SELECT * FROM iris LIMIT 10")
iris_preview

# copy mtcars into spark
mtcars_tbl <- copy_to(sc, mtcars)

# transform our data set, and then partition into 'training', 'test'
partitions <- mtcars_tbl %>%
  filter(hp >= 100) %>%
  mutate(cyl8 = cyl == 8) %>%
  sdf_random_split(training = 0.5, test = 0.5, seed = 1099)

# fit a linear model to the training dataset
fit <- partitions$training %>%
  ml_linear_regression(response = "mpg", features = c("wt", "cyl"))
fit


summary(fit)

## Reading and Writing Data

temp_csv <- tempfile(fileext = ".csv")
temp_parquet <- tempfile(fileext = ".parquet")
temp_json <- tempfile(fileext = ".json")

spark_write_csv(iris_tbl, temp_csv)
iris_csv_tbl <- spark_read_csv(sc, "iris_csv", temp_csv)

spark_write_parquet(iris_tbl, temp_parquet)
iris_parquet_tbl <- spark_read_parquet(sc, "iris_parquet", temp_parquet)

spark_write_json(iris_tbl, temp_json)
iris_json_tbl <- spark_read_json(sc, "iris_json", temp_json)

dplyr::src_tbls(sc)


spark_web(sc)

spark_disconnect(sc)

## Using H2O
# rsparkling is a CRAN package from H2O that extends sparklyr to provide an interface into Sparkling Water. For instance, the following example installs, configures and runs h2o.glm:
  
options(rsparkling.sparklingwater.version = "2.1.14")

library(rsparkling)
library(sparklyr)
library(dplyr)
library(h2o)
h2o.init()
sc <- spark_connect(master = "local", version = "2.1.0")
mtcars_tbl <- copy_to(sc, mtcars, "mtcars", overwrite = TRUE)

mtcars_h2o <- h2o::as.h2o(mtcars_tbl)

?as.h2o.data.frame
mtcars_glm <- h2o.glm(x = c("wt", "cyl"),
                      y = "mpg",
                      training_frame = mtcars_h2o,
                      lambda_search = TRUE)
mtcars_glm

## --------------------

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


#### -----------------------------------------------
spark_disconnect(sc)
sc <- spark_connect(master = "local")
src_tbls(sc)

### 

data("iris") 
iris %>% rename_with(~ tolower(gsub(".", "_", .x, fixed = TRUE))) %>% write_csv("./data/IRIS.csv")

