knitr::opts_chunk$set(echo = TRUE)


library(tidyverse)
library(tidymodels)
library(kernlab)
library(doParallel)


training <- read_csv("../data/training/training_fe.csv")


training_model <- training %>%
  mutate(
    previous_crop = as.factor(previous_crop),
    year          = as.integer(year)
  ) %>%
  drop_na()

cat("Total rows for modeling:", nrow(training_model), "\n")


set.seed(931)
yield_split <- initial_split(training_model, prop = 0.70, strata = yield_mg_ha)

train_data <- training(yield_split)
test_data  <- testing(yield_split)

cat("Train rows:", nrow(train_data), "\n")
cat("Test rows:",  nrow(test_data),  "\n")



svm_recipe <- recipe(yield_mg_ha ~ ., data = train_data) %>%
  step_rm(hybrid, date_planted, date_harvested) %>%
  step_novel(site, previous_crop) %>%
  step_other(previous_crop, threshold = 0.05) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())

svm_recipe



svm_spec <- svm_rbf(
  cost      = tune(),
  rbf_sigma = tune()
) %>%
  set_engine("kernlab") %>%
  set_mode("regression")



svm_wf <- workflow() %>%
  add_recipe(svm_recipe) %>%
  add_model(svm_spec)


set.seed(123)
folds <- vfold_cv(train_data, v = 10, strata = yield_mg_ha)



svm_grid <- grid_regular(
  cost(range      = c(-2, 5)),
  rbf_sigma(range = c(-5, 0)),
  levels = 5
)

cat("Grid size:", nrow(svm_grid), "combinations\n")



n_cores <- parallel::detectCores() - 1
cat("Using", n_cores, "cores\n")
registerDoParallel(cores = n_cores)



cat("Starting tuning...\n")

svm_tune <- tune_grid(
  svm_wf,
  resamples = folds,
  grid      = svm_grid,
  metrics   = metric_set(rmse, rsq),
  control   = control_grid(verbose = TRUE, save_pred = TRUE)
)

stopImplicitCluster()



cat("Tuning complete. Best results:\n")
show_best(svm_tune, metric = "rmse")

autoplot(svm_tune)
ggsave("outputs/svm_tuning_plot.png", width = 8, height = 5, dpi = 300)



best_params <- select_best(svm_tune, metric = "rmse")
cat("Best parameters:\n")
print(best_params)

final_svm_wf <- finalize_workflow(svm_wf, best_params)

cat("Fitting final model on full training set...\n")
final_svm_fit <- fit(final_svm_wf, data = train_data)


test_preds <- predict(final_svm_fit, test_data) %>%
  bind_cols(test_data %>% select(yield_mg_ha))

test_metrics <- test_preds %>%
  metrics(truth = yield_mg_ha, estimate = .pred)

cat("Holdout Test Performance:\n")
print(test_metrics)


test_preds %>%
  ggplot(aes(x = yield_mg_ha, y = .pred)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, color = "darkred", linetype = "dashed") +
  labs(title = "SVM: Predicted vs Actual Yield",
       x = "Actual Yield (Mg/ha)",
       y = "Predicted Yield (Mg/ha)") +
  theme_minimal()

ggsave("outputs/svm_predicted_vs_actual.png", width = 8, height = 5, dpi = 300)


saveRDS(final_svm_fit,  "outputs/svm_final_model.rds")
saveRDS(svm_tune,       "outputs/svm_tune_results.rds")
write_csv(test_metrics, "outputs/svm_test_metrics.csv")

cat("SVM script complete\n")
