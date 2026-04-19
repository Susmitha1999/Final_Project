install.packages("xgboost") #new pacakage
install.packages("doParallel") #new pacakage
library(tidymodels)   # Core framework for modeling (includes recipes, workflows, parsnip, etc.)
library(finetune)     # Additional tuning strategies (e.g., racing, ANOVA-based tuning)
library(vip)          # For plotting variable importance from fitted models
library(xgboost)      # XGBoost implementation in R
library(ranger)       # Fast implementation of Random Forests
library(tidyverse)    # Data wrangling and visualization
library(doParallel)   # For parallel computing (useful during resampling/tuning)
library(caret)       # Other great library for Machine Learning 

training <- read.csv("../data/training/training_fe.csv") %>%
  clean_names()


set.seed(931)
training_split <- initial_split(training, 
  prop = .7, 
  strata = yield_mg_ha  # Stratify by target variable
  )

training_split

data_train <- training(training_split)  # 70% of data
data_train #Training data frame

data_test <- testing(training_split)  # 30% of data
data_test #Testing data frame

ggplot() +
  geom_density(data = data_train, 
               aes(x = yield_mg_ha),
               color = "red") +
  geom_density(data = data_test, 
               aes(x = yield_mg_ha),
               color = "blue")

xgb_recipe <- recipe(y ~ ., data = data_train) %>%
step_rm(
    year,       # Remove year identifier
    site,       # Remove site identifier
)

xgb_recipe


# Prep the recipe to estimate any required statistics
xgp_prep <- xgp_recipe %>% 
  prep()

# Examine preprocessing steps
xgb_prep

xgb_model <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  min_n = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")  

xgb_model

set.seed(250)
# Create 10-fold cross-validation resampling object from training data
resampling_foldcv <- vfold_cv(traindata, 
                              v = 10)

resampling_foldcv
resampling_foldcv$splits[[1]]

xgb_grid <- grid_latin_hypercube(
  tree_depth(), 
  min_n(),
  learn_rate(),
  trees(),
  size = 100 
)

xgb_grid

ggplot(data = xgb_grid,
       aes(x = tree_depth, 
           y = min_n)) +
  geom_point(aes(color = factor(learn_rate),
                 size = trees),
             alpha = .5,
             show.legend = FALSE)

set.seed(7644)
registerDoParallel(cores = parallel::detectCores() -1)

xgb_result <- tune_race_anova(object = xgb_model,
                      preprocessor = xgb_recipe,
                      resamples = resampling_foldcv,
                      grid = xgb_grid,
                      control = control_race(save_pred = TRUE))

stopImplicitCluster()


beepr::beep()
xgb_result$.metrics[[2]]

# Based on lowest RMSE
best_rmse <- xgb_result %>% 
  select_best(metric = "rmse")%>% 
  mutate(source = "best_rmse")

best_rmse
