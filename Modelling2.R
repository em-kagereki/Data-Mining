
# By default this separates into 75:25

# https://www.kirenz.com/post/2021-02-17-r-classification-tidymodels/#boosted-tree-xgboost

# https://www.tidymodels.org/start/case-study/

source("Step3_featureEngineering.R")
data2$EXPIRE_FLAG<-as.factor(data2$EXPIRE_FLAG) 
data2<-data2%>% 
  select(-FreqDISCHARGE_LOCATION)
splits<- initial_split(data2%>% select(-HADM_ID),
                       prop =3/4,
                       strata = EXPIRE_FLAG)

data_training <- training(splits)
data_test  <- testing(splits)

data_training %>% 
  count(EXPIRE_FLAG) %>% 
  mutate(prop = n/sum(n))

data_test  %>% 
  count(EXPIRE_FLAG) %>% 
  mutate(prop = n/sum(n))


val_set <- validation_split(data_training, 
                            strata = EXPIRE_FLAG, 
                            prop = 0.80)
val_set




lr_recipe <- 
  recipe(EXPIRE_FLAG ~ ., data = data_training) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors()) %>%
  step_corr(all_predictors(), threshold = 0.7, method = "spearman")


## CV folds:

cv_folds <-
  vfold_cv(data_training, 
           v = 5, 
           strata = EXPIRE_FLAG) 

# Logistic regression:
log_spec <- # your model specification
  logistic_reg() %>%  # model type
  set_engine(engine = "glm") %>%  # model engine
  set_mode("classification") # model mode

## Logistic Workflow :
log_wflow <- 
  workflow() %>% 
  add_recipe(lr_recipe) %>%   
  add_model(log_spec)   

# Fit model
log_res <- 
  log_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(
      save_pred = TRUE)
  ) 
log_res %>%  collect_metrics(summarize = FALSE)

log_pred <- 
  log_res %>%
  collect_predictions()

log_auc <- 
  log_res %>% 
  collect_predictions(parameters = log_res) %>% 
  roc_curve(EXPIRE_FLAG, .pred_0) %>% 
  mutate(model = "Logistic regression")


# Random forest
rf_spec <- 
  rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

rf_wflow <-
  workflow() %>%
  add_recipe(lr_recipe) %>% 
  add_model(rf_spec) 
## Fit model

rf_res <-
  rf_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  ) 

rf_res %>%  collect_metrics(summarize = TRUE)


rf_pred <- 
  rf_res %>%
  collect_predictions()

rf_auc <- 
  rf_res %>% 
  collect_predictions(parameters = rf_res) %>% 
  roc_curve(EXPIRE_FLAG, .pred_0) %>% 
  mutate(model = "Logistic regression")

# Xgboost
xgb_spec <- 
  boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

xgb_wflow <-
  workflow() %>%
  add_recipe(lr_recipe) %>% 
  add_model(xgb_spec)

xgb_res <- 
  xgb_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  ) 

xgb_res %>% collect_metrics(summarize = TRUE)


xgb_pred <- 
  xgb_res %>%
  collect_predictions()

xgb_auc <- 
  xgb_res %>% 
  collect_predictions(parameters = xgb_res) %>% 
  roc_curve(EXPIRE_FLAG, .pred_0) %>% 
  mutate(model = "Logistic regression")

## KNN
knn_spec <- 
  nearest_neighbor(neighbors = 4) %>% # we can adjust the number of neighbors 
  set_engine("kknn") %>% 
  set_mode("classification")

## Knn workflow
knn_wflow <-
  workflow() %>%
  add_recipe(lr_recipe) %>% 
  add_model(knn_spec)


##KNN model
knn_res <- 
  knn_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)
  ) 
knn_res %>% collect_metrics(summarize = TRUE)

knn_pred <- 
  knn_res %>%
  collect_predictions()

knn_auc <- 
  knn_res %>% 
  collect_predictions(parameters = knn_res) %>% 
  roc_curve(EXPIRE_FLAG, .pred_0) %>% 
  mutate(model = "Logistic regression")


#nnet_spec <-
 # mlp() %>%
 # set_mode("classification") %>% 
 # set_engine("keras", verbose = 0) 

## ANN workflow:
#nnet_wflow <-
#  workflow() %>%
#  add_recipe(lr_recipe) %>% 
#  add_model(nnet_spec)

#nnet_res <- 
#  nnet_wflow %>% 
#  fit_resamples(
#    resamples = cv_folds, 
 #   metrics = metric_set(
 #     f_meas, 
 #     accuracy, kap,
 #     roc_auc, sens, spec),
 #   control = control_resamples(save_pred = TRUE)
 # ) 



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

# nnet_metrics <- 
#   nnet_res %>% 
#   collect_metrics(summarise = TRUE) %>%
#   mutate(model = "Neural Net")

# create dataframe with all models
model_compare <- bind_rows(
  log_metrics,
  rf_metrics,
  xgb_metrics,
  knn_metrics,
  # nnet_metrics
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


## Model tuning

rf_best_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("classification")
rf_best_workflow <- 
  workflow() %>% 
  add_model(rf_best_mod) %>% 
  add_recipe(lr_recipe)

cores <- parallel::detectCores()
cores
set.seed(345)
rf_best_res <- 
  rf_best_workflow %>% 
  tune_grid(val_set,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

rf_best <- 
  rf_best_res %>% 
  select_best(metric = "roc_auc")

rf_best_res %>% 
  collect_predictions()

rf_auc_best <- 
  rf_best_res %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(EXPIRE_FLAG, .pred_0) %>% 
  mutate(model = "Tuned Random Forest")

All_Models<-bind_rows(rf_auc,knn_auc,rf_auc_best) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_viridis_d(option = "plasma", end = .6)
