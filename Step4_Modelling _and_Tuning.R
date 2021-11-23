
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
log_res %>%  collect_metrics(summarize = TRUE)

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


 qda_spec <- discrim_quad() %>%
set_mode("classification") %>%
  set_engine("MASS")

qda_wflow <- 
  workflow() %>% 
  add_model(qda_spec) %>% 
  add_recipe(lr_recipe)

qda_res <- 
  qda_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)) 

 qda_metrics<- qda_res %>% collect_metrics(summarize = TRUE)


## KNN
knn_spec <- 
  nearest_neighbor()%>% # we can adjust the number of neighbors 
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
#  set_mode("classification") %>% 
#  set_engine("keras", verbose = 0) 

## ANN workflow:
#nnet_wflow <-
#  workflow() %>%
#  add_recipe(lr_recipe) %>% 
#  add_model(nnet_spec)

#nnet_res <- 
 # nnet_wflow %>% 
#  fit_resamples(
 #   resamples = cv_folds, 
#    metrics = metric_set(
 #     f_meas, 
  #    accuracy, kap,
   #   roc_auc, sens, spec),
  #  control = control_resamples(save_pred = TRUE)
  #) 

#nnet_metrics <- 
 # nnet_res %>% 
  #collect_metrics(summarise = TRUE) %>%
  #mutate(model = "Logistic Regression") # add the name of the model to every row


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

qda_metrics <- 
  qda_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "QDA")

# nnet_metrics <- 
#   nnet_res %>% 
#   collect_metrics(summarise = TRUE) %>%
#   mutate(model = "Neural Net")

# create dataframe with all models
model_compare <- bind_rows(
  log_metrics,
  rf_metrics,
  qda_metrics,
  knn_metrics,
  # nnet_metrics
) 

# change data structure
model_comp <- 
  model_compare %>% 
  select(model,.metric,mean, std_err) %>% 
  pivot_wider(names_from = .metric, values_from = c(mean, std_err)) 


## Then compare the linear models

## SVM
svm_spec <-
  svm_rbf() %>%
  set_mode("classification") %>%
  set_engine("kernlab")

svm_wflow <- 
  workflow() %>% 
  add_model(svm_spec) %>% 
  add_recipe(lr_recipe)

svm_res <- 
  svm_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)) 

svm_res %>% collect_metrics(summarize = TRUE)

svm_metrics <- 
  svm_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "SVM")


## NAive Bayes
nb_spec <- naive_Bayes() %>% 
  set_mode("classification") %>% 
  set_engine("klaR") %>% 
  set_args(usekernel = FALSE)  

nb_wflow <- 
  workflow() %>% 
  add_model(nb_spec) %>% 
  add_recipe(lr_recipe)

nb_res <- 
  nb_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)) 

nb_metrics <- 
  nb_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "Naive bayes")


lda_spec <- discrim_linear() %>%
  set_mode("classification") %>%
  set_engine("MASS")

lda_wflow <- 
  workflow() %>% 
  add_model(lda_spec) %>% 
  add_recipe(lr_recipe)

lda_res <- 
  lda_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(save_pred = TRUE)) 


lda_metrics <- 
  lda_res %>% 
  collect_metrics(summarise = TRUE) %>%
  mutate(model = "LDA")


# create dataframe with all models
model_compare <- bind_rows(
  log_metrics,
  rf_metrics,
  qda_metrics,
  knn_metrics,
  lda_metrics,
  nb_metrics,
  svm_metrics
  # nnet_metrics
) 

write.csv(model_compare,"model_compare.csv")

## Tune the top two models:

set.seed(345)
cores <- parallel::detectCores()
rf_best_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
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
            metrics = metric_set(f_meas, 
                                 accuracy, kap,
                                 roc_auc, sens, spec))
rf_best <- 
  rf_best_res %>% 
  select_best(metric = "f_meas")

rf_best_metrics  <- rf_best_res %>% 
  collect_metrics(summarize = TRUE)%>%
  mutate(model = "Random Forest(Tuned)")


# Tuned Logistic regression:
library(glmnet)
library(themis)

set.seed(19)

mod <- logistic_reg(penalty = tune(),
                    mixture = tune()) %>%
  set_engine("glmnet")


wfl <- workflow() %>%
  add_recipe(lr_recipe) %>%
  add_model(mod)

glmn_set <- parameters(penalty(range = c(-5,1), trans = log10_trans()),
                       mixture())

glmn_grid <- 
  grid_regular(glmn_set, levels = c(7, 5))
ctrl <- control_grid(save_pred = TRUE, verbose = TRUE)


glmn_tune <- 
  tune_grid(wfl,
            resamples = cv_folds,
            grid = glmn_grid,
            control = ctrl,
            metrics = metric_set(f_meas, 
                                 accuracy,kap,
                                 roc_auc,sens,spec))


best_log <- select_best(glmn_tune, metric = "f_meas")

best_log_metrics<-glmn_tune %>% 
  collect_metrics(summarize = TRUE)%>%
  mutate(model = "Logistic regression(Tuned)")


## Tune SVM

Tune_svm_mod <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

svm_tune_workflow <- 
  workflow() %>% 
  add_model(Tune_svm_mod) %>% 
  add_recipe(lr_recipe)

set.seed(19)

svm_tune<-tune_grid(svm_tune_workflow,
          resamples = cv_folds,
          grid = 25,
          metrics = metric_set(f_meas, 
                               accuracy,kap,
                               sens,spec),
                    control = control_resamples(save_pred = TRUE))


best_svm_metrics<-svm_tune %>% 
  collect_metrics(summarize = TRUE)%>%
  mutate(model = "SVM(Tuned)")


### xgboost
tuned_xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")


xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), data_training),
  learn_rate(),
  size = 25
)

tuned_xgb_wflow <-
  workflow() %>%
  add_recipe(lr_recipe) %>% 
  add_model(tuned_xgb_spec)

doParallel::registerDoParallel()

set.seed(234)
tuned_xgb_res <- tune_grid(
  tuned_xgb_wflow,
  resamples = cv_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

tuned_xgb_metrics<-tuned_xgb_res %>% 
  collect_metrics(summarize = TRUE)%>%
  mutate(model = "XGBOOST(Tuned)")

### Tune LDA

tune_lda_spec <- discrim_linear(penalty = tune()) %>%
  set_mode("classification") %>%
  set_engine("MASS")

tune_lda_wflow <- 
  workflow() %>% 
  add_model(tune_lda_spec) %>% 
  add_recipe(lr_recipe)

lda_set <- parameters(penalty())

lda_grid <- 
  grid_regular(lda_set, levels = c(25))

set.seed(234)
tuned_lda_res <- tune_grid(
  tune_lda_wflow,
  resamples = cv_folds,
  grid = lda_grid,
  metrics = metric_set(f_meas, 
                       accuracy,kap,
                       sens,spec),
  control = ctrl
)


#write.csv(xgb_metrics, "best_xgboost_metrics.csv")

# create dataframe with all models
model_compare <- bind_rows(
  log_metrics,
  rf_metrics,
  qda_metrics,
  knn_metrics,
  lda_metrics,
  nb_metrics,
  svm_metrics,
  best_log_metrics,
  rf_best_metrics,
  xgb_metrics,
  best_svm_metrics,
  tuned_xgb_metrics
  #nnet_metrics
) 
write.csv(model_compare, "final_metrics.csv")















#### UPDATE THE BEST MODEL

Final_BEST_log_spec <- # your model specification
  logistic_reg(penalty=0.01,mixture = 0.5) %>%  # model type
  set_engine(engine = "glm") %>%  # model engine
  set_mode("classification") # model mode

## Logistic Workflow :
Final_Best_log_wflow <- 
  workflow() %>% 
  add_recipe(lr_recipe) %>%   
  add_model(Final_BEST_log_spec)   

# Fit model
Best_FInal_log_res <- 
  Final_Best_log_wflow %>% 
  fit_resamples(
    resamples = cv_folds, 
    metrics = metric_set(
      f_meas, 
      accuracy, kap,
      roc_auc, sens, spec),
    control = control_resamples(
      save_pred = TRUE)
  ) 
Best_FInal_log_res %>%  collect_metrics(summarize = TRUE)

log_auc <- 
  Best_FInal_log_res %>% 
  collect_predictions(parameters = log_res) %>% 
  roc_curve(EXPIRE_FLAG, .pred_0) %>% 
  mutate(model = "Logistic regression")

log_pr <- 
  Best_FInal_log_res %>% 
  collect_predictions(parameters = log_res) %>% 
  pr_curve(EXPIRE_FLAG, .pred_0) %>% 
  mutate(model = "Logistic regression")


last_log_fit <- 
  Final_Best_log_wflow %>% 
  last_fit(split=splits_data)

last_log_fit %>%  collect_metrics(summarize = TRUE)


Predictions<-last_log_fit %>% 
  collect_predictions()

pr_curve(Predictions, EXPIRE_FLAG, .pred_class)

library(ggplot2)
library(dplyr)
bind_rows(log_auc) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) + 
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal()

finalMetrics <- read.csv("final_metrics.csv", h=T, sep=',') %>% 
  filter(.metric == "f_meas")

ggplot(finalMetrics, aes(x = factor(model), y = mean)) +
  geom_point(width = 0.9, fill = "white") +
  geom_jitter(aes(color = model, shape = model)) +
  labs(x = NULL)   # Remove x axis label


ggplot(finalMetrics) +
  geom_point(aes(x = mean, y = model,color = model))
