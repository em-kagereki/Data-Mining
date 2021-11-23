
# By default this separates into 75:25

# https://www.kirenz.com/post/2021-02-17-r-classification-tidymodels/#boosted-tree-xgboost

# https://www.tidymodels.org/start/case-study/

setwd("E:/school/data mining/project/mimic-iii-clinical-database-1.4/mimic-iii-clinical-database-1.4/codes")

source("Step3_featureEngineering.R")
data2$EXPIRE_FLAG<-as.factor(data2$EXPIRE_FLAG) 
data2<-data2%>% 
  select(-FreqDISCHARGE_LOCATION)
data2$ADMITTIME <-ymd_hms(data2$ADMITTIME)

splits_data<- initial_split(data2%>% select(-HADM_ID),
                       prop =8/10)

data_training <- training(splits_data)
data_test  <- testing(splits_data)

data_training %>% 
  count(EXPIRE_FLAG) %>% 
  mutate(prop = n/sum(n))

data_test  %>% 
  count(EXPIRE_FLAG) %>% 
  mutate(prop = n/sum(n))


val_set <- validation_split(data_training, 
                            strata = EXPIRE_FLAG, 
                            prop = 0.80)

lr_recipe <- 
  recipe(EXPIRE_FLAG ~ ., data = data_training) %>% 
  step_zv(all_predictors()) %>% 
  step_date(ADMITTIME) %>% 
  step_rm(ADMITTIME) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_normalize(all_predictors()) %>%
  step_corr(all_predictors(), threshold = 0.7, method = "spearman") %>% 
  step_pca(all_numeric())


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
    control = control_resamples(save_pred = TRUE)) 
knn_res %>% collect_metrics(summarize = TRUE)


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
  )+
  theme_economist() 


## Model tuning




set.seed(345)
cores <- parallel::detectCores()

rf_best_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 128) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("classification")

rf_best_workflow <- 
  workflow() %>% 
  add_model(rf_best_mod) %>% 
  add_recipe(lr_recipe)

rf_best_res <- 
  rf_best_workflow %>% 
  tune_grid(val_set,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(f_meas))

rf_best <- 
  rf_best_res %>% 
  select_best(metric = "f_meas")


rf_best_metrics <- 
  rf_best_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Tuned Random Forest")


## By calling rf_best, we are collecting the parametres for the best model

rf_best_res %>% 
  collect_predictions() 


mean<-mean(rf_best_metrics$mean)

model_comp<-model_comp %>% 
  select(model,mean_f_meas) %>% 
  add_row(model = "Tuned Random Forest", mean_f_meas = mean)

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
  )+
  #scale_color_viridis_d(option = "plasma", end = .6)+ 
  theme_economist() 


## The model with the best estimat
## We have to repeat this code to get the new metrics

last_rf_mod <- 
  rand_forest(mtry = rf_best$mtry, min_n = rf_best$min_n, trees = 128) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("classification")

last_rf_workflow <- 
  rf_wflow %>% 
  update_model(last_rf_mod)


last_rf_fit <- 
  last_rf_workflow %>% 
  last_fit(split=splits_data)

Predictions<-last_rf_fit %>% 
  collect_predictions()


last_rf_fit %>% 
  collect_metrics()



Predictions$EXPIRE_FLAG<-as.numeric(Predictions$EXPIRE_FLAG)
Predictions$.pred_class<-as.numeric(Predictions$.pred_class)

library(cutpointr)


cp <- cutpointr(Predictions, .pred_class, EXPIRE_FLAG, 
                method = maximize_metric, metric = sum_sens_spec)

F1Score <- cutpointr(Predictions, .pred_class, EXPIRE_FLAG, 
                method = maximize_metric, metric = F1_score)

summary(cp)

plot(cp)
#opt_cut_b <- cutpointr(Predictions, .pred_class, EXPIRE_FLAG, boot_runs = 1000)


last_rf_fit %>% 
  pluck(".workflow", 1) %>%   
  extract_fit_parsnip() %>% 
  vip(num_features = 20)
# THreshold of the F1
#https://arxiv.org/abs/1402.1892
