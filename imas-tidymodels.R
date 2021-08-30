## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)


## ----install-packages, include = FALSE---------------------------------------------------------------------------------------------------------------------
source('./R/install_required_packages.R') #run this line to install the required packages


## ----load-packages, warning=FALSE, message=FALSE-----------------------------------------------------------------------------------------------------------
library(palmerpenguins) #penguins data
library(tidyverse) #wrangling
library(tidymodels) #modeling and machine learning
library(vip) #variable importance
library(fastshap) #SHapley Additive exPlanations
library(janitor) #cleaning variable names
library(lubridate) #manipulating dates
library(tictoc) #timing computations
library(ggcorrplot) #correlation plots


## ----penguins, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------
penguins_raw <- palmerpenguins::penguins_raw


## ----glimpse-penguins--------------------------------------------------------------------------------------------------------------------------------------
dplyr::glimpse(penguins_raw)


## ----tidy-penguins-----------------------------------------------------------------------------------------------------------------------------------------
penguins_prepared <- penguins_raw %>%
  janitor::clean_names() %>%
  dplyr::select(c(species, island, date_egg, culmen_length_mm:delta_13_c_o_oo)) %>%
  dplyr::mutate(species = case_when(species == "Adelie Penguin (Pygoscelis adeliae)" ~ "adelie",
                             species == "Gentoo penguin (Pygoscelis papua)" ~ "gentoo",
                             species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "chinstrap")) %>%
  dplyr::mutate(species = factor(species),
                island = factor(island),
                sex = factor(sex),
                year = lubridate::year(date_egg)) %>%
  dplyr::rename(bill_length_mm = culmen_length_mm,
                bill_depth_mm = culmen_depth_mm,
                delta_15_n = delta_15_n_o_oo,
                delta_13_c = delta_13_c_o_oo) %>%
  tidyr::drop_na()



## ----visualise-bodies, out.width="100%"--------------------------------------------------------------------------------------------------------------------

#Long format is great for summarising and plotting
penguins_prepared_long <- penguins_prepared %>%
  tidyr::pivot_longer(., -c(species, island, date_egg, sex , year), names_to = "variable", values_to = "value")

ggplot(data = penguins_prepared_long %>%
         dplyr::filter(variable == "bill_length_mm" |
                         variable == "bill_depth_mm" |
                         variable == "flipper_length_mm" |
                         variable == "body_mass_g"), 
       aes(x = sex, y = value, fill = species)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), width = 0.75) +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  facet_wrap(~variable, scales = "free_y") +
  theme_bw() +
  labs(y = NULL,
       x = "Penguin sex",
       fill = "Penguin species")



## ----visualise-isotopes, echo=FALSE, out.width="100%"------------------------------------------------------------------------------------------------------
ggplot(data = penguins_prepared_long %>%
         dplyr::filter(variable == "delta_15_n" |
                         variable == "delta_13_c"), 
       aes(x = sex, y = value, fill = species)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), width = 0.75) +
  scale_fill_manual(values = c("darkorange","purple","cyan4")) +
  facet_grid(variable ~ year, scales = "free") +
    theme_bw() +
  labs(y = NULL,
       x = "Penguin sex",
       fill = "Penguin species")


## ----split-sets--------------------------------------------------------------------------------------------------------------------------------------------
set.seed(222)
penguins_split <- rsample::initial_split(penguins_prepared, prop = 2/3, strata = species)
penguins_train <- rsample::training(penguins_split) #extract the training set from our split
penguins_test <- rsample::testing(penguins_split) #extract the testing set from our split


## ----define-recipe-----------------------------------------------------------------------------------------------------------------------------------------
penguins_recipe <- recipes::recipe(delta_13_c ~ 
                                     bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g + #numeric features
                                     sex + species + year + island, #nominal features (except year..)
                                   data = penguins_train) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors())


## ----create-workflow---------------------------------------------------------------------------------------------------------------------------------------
penguins_workflow <- workflows::workflow() %>%
  workflows::add_recipe(penguins_recipe)


## ----model-types-------------------------------------------------------------------------------------------------------------------------------------------
model_specifications <- list()

model_specifications$linear_regression_specification <- parsnip::linear_reg() %>%
  set_mode("regression") %>%
  set_engine("lm")

model_specifications$decision_tree_specification <- parsnip::decision_tree() %>%
  set_mode("regression") %>%
  set_engine("rpart")

model_specifications$random_forest_specification <- parsnip::rand_forest() %>%
  set_mode("regression") %>%
  set_engine("ranger", importance = "permutation") #we'll discuss this later.

model_specifications$svm_poly_specification <- parsnip::svm_poly() %>%
  set_mode("regression") %>%
  set_engine("kernlab")

model_specifications$mars_specification <- parsnip::mars() %>%
  set_mode("regression") %>%
  set_engine("earth")

###BONUS###
#model_specifications$boosted_tree_specification <- parsnip::????() %>%
#  set_mode("regression") %>%
#  set_engine("????")


## ----define-fit--------------------------------------------------------------------------------------------------------------------------------------------
fit_my_models <- function(model_specification){
  
  fitted_model <- penguins_workflow %>%
    workflows::add_model(model_specification) %>%
    parsnip::fit(data = penguins_train)
  
  fitted_model
  
}


## ----do-fit, message=FALSE---------------------------------------------------------------------------------------------------------------------------------
fitted_models <- purrr::map(.x = model_specifications, .f = fit_my_models)


## ----predict-test------------------------------------------------------------------------------------------------------------------------------------------
predict_with_my_models <- function(fitted_model){
  
  #extract a string with the model engine
  model_engine <- fitted_model$fit$fit$spec$engine %>%
    unlist()
  
  #make our prediction and add it to the test set dataframe
  predicted_test <- predict(fitted_model, new_data = penguins_test) %>%
    dplyr::bind_cols(penguins_test, .) %>%
    dplyr::mutate(engine = model_engine) #adds a column containing the model engine
  
  #output dataframe of the test set with the prediction
  predicted_test
  
}

#Map over list of fitted models, predict and combine into dataframe by rows
predicted_tests <- purrr::map_dfr(fitted_models, predict_with_my_models)


## ----glimpse-predicted-------------------------------------------------------------------------------------------------------------------------------------
glimpse(predicted_tests)


## ----collect-metrics---------------------------------------------------------------------------------------------------------------------------------------
predicted_test_metrics <- predicted_tests %>%
  dplyr::group_by(engine) %>%
  yardstick::metrics(., truth = delta_13_c, estimate = .pred)


## ----plot-metrics, out.width="100%"------------------------------------------------------------------------------------------------------------------------
ggplot(data = predicted_test_metrics, aes(x = engine, y = .estimate, colour = engine)) +
  geom_point(size = 3) +
  facet_wrap(~.metric, scales = "free", nrow = 3) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2")


## ----scatter-metrics, out.width="100%"---------------------------------------------------------------------------------------------------------------------
ggplot(data = predicted_tests, aes(x = .pred, y = delta_13_c, colour = engine)) +
  geom_point() +
  geom_abline(slope = 1, linetype = 2) +
  facet_wrap(~engine) +
  theme_bw() +
  guides(color = "none") +
  scale_color_brewer(palette = "Dark2")


## ---- echo=FALSE, out.width="100%"-------------------------------------------------------------------------------------------------------------------------
knitr::include_graphics("./figs/delta_13_c_decision_tree_example.jpeg")


## ----tune-spec---------------------------------------------------------------------------------------------------------------------------------------------
tune_specification_random_forest <- parsnip::rand_forest(
  trees = 200,
  mtry = tune(),
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger", importance = "permutation")


## ----tune-workflow-----------------------------------------------------------------------------------------------------------------------------------------
tuning_workflow_random_forest <- workflow() %>%
  add_recipe(penguins_recipe) %>%
  add_model(tune_specification_random_forest)


## ----hyperparameter-grid-----------------------------------------------------------------------------------------------------------------------------------
hyperparameter_grid_random_forest <- dials::grid_regular(
  mtry(range = c(1,11)),
  min_n(range = c(1,30)),
  #trees(range = c(50, 2000)),
  levels = 3
)

glimpse(hyperparameter_grid_random_forest)


## ----create-vfolds-----------------------------------------------------------------------------------------------------------------------------------------
set.seed(222)
penguins_train_folds <- rsample::vfold_cv(penguins_train, v = 3, repeats = 3)


## ----tune-vfolds-------------------------------------------------------------------------------------------------------------------------------------------
# set.seed(333)
# tictoc::tic()
# tuning_results_random_forest <- tune::tune_grid(
#   object = tuning_workflow_random_forest,
#   resamples = penguins_train_folds,
#   grid = hyperparameter_grid_random_forest,
#   metrics = yardstick::metric_set(rsq, rmse)
# )
# tictoc::toc()

#For a much larger number of hyperparameter combinations, run the following:
tuning_results_random_forest <- readRDS('./data/processed/penguins_tuning_results_random_forest_extra.rds')


## ----tune-metrics, out.width = "100%"----------------------------------------------------------------------------------------------------------------------
collected_metrics_random_forest <- tuning_results_random_forest  %>%
  collect_metrics() %>%
  tidyr::pivot_longer(-c(.metric:.config),
               values_to = "value",
               names_to = "parameter"
  )

ggplot(data = collected_metrics_random_forest, aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  geom_errorbar(data = collected_metrics_random_forest, aes(x = value, 
                                                            ymin = mean - std_err,
                                                            ymax = mean + std_err, 
                                                            color = parameter),
                show.legend = FALSE) + 
  facet_grid(.metric~parameter, scales = "free") +
  labs(x = NULL, y = "estimate") +
  theme_bw() +
  scale_color_brewer(palette = "Set1")


## ----finalise-model----------------------------------------------------------------------------------------------------------------------------------------
best_rmse <- tune::select_best(tuning_results_random_forest, "rmse")

my_tuned_model_random_forest <- tune::finalize_model(
  tune_specification_random_forest,
  best_rmse
)

my_fitted_model_random_forest <- workflow() %>%
  add_recipe(penguins_recipe) %>%
  add_model(my_tuned_model_random_forest) %>%
  parsnip::fit(penguins_train)

my_predicted_test_random_forest <- predict(my_fitted_model_random_forest, new_data = penguins_test) %>%
  bind_cols(penguins_test, .)


## ----metrics-best------------------------------------------------------------------------------------------------------------------------------------------
yardstick::metrics(my_predicted_test_random_forest, truth = delta_13_c, estimate = .pred)



## ----metrics-default---------------------------------------------------------------------------------------------------------------------------------------
yardstick::metrics(predicted_tests %>% filter(engine == "ranger"), truth = delta_13_c, estimate = .pred)


## ----vip, out.width = "100%"-------------------------------------------------------------------------------------------------------------------------------
my_fitted_model_random_forest %>%
  extract_fit_parsnip() %>%
  vip::vip() +
  theme_bw()


## ----corrplot, out.width = "100%"--------------------------------------------------------------------------------------------------------------------------
penguins_recipe_juiced <- penguins_recipe %>%
                   recipes::prep() %>%
                   recipes::juice()

ggcorrplot::ggcorrplot(penguins_recipe_juiced %>% 
                         cor() %>%
                         round(., 1),
                       hc.order = TRUE, lab = TRUE)



## ----shapmatrix--------------------------------------------------------------------------------------------------------------------------------------------
matrix_for_shap <- penguins_recipe %>%
  recipes::prep() %>%
  recipes::juice() %>%
  dplyr::select(-delta_13_c) %>%
  as.matrix()


## ----shap-prep---------------------------------------------------------------------------------------------------------------------------------------------
pfun <- function(object, newdata) {
  predict(object, data = newdata)$predictions
}

fitted_model_object <- my_fitted_model_random_forest %>%
  extract_fit_parsnip()



## ----calculate-shap----------------------------------------------------------------------------------------------------------------------------------------
tictoc::tic()
shap_n100 <- fastshap::explain(fitted_model_object$fit, 
                             X = matrix_for_shap, 
                             pred_wrapper = pfun, 
                             nsim = 100)
tictoc::toc()


## ----plot-shap-global, out.width = "100%"------------------------------------------------------------------------------------------------------------------
autoplot(shap_n100) +
  theme_bw()


## ----plot-shap-single, out.width = "100%"------------------------------------------------------------------------------------------------------------------
autoplot(shap_n100, type ="contribution", row_num = 1) +
  theme_bw()


## ----shap-dependence, out.width = "100%"-------------------------------------------------------------------------------------------------------------------
autoplot(shap_n100, 
         type = "dependence", 
         feature = "flipper_length_mm", 
         X = penguins_recipe_juiced,
         smooth = TRUE) +
  theme_bw()


## ----flipper-delta13c, out.width="100%"--------------------------------------------------------------------------------------------------------------------
ggplot(data = penguins_train, aes(x = flipper_length_mm, y = delta_13_c)) +
  geom_point() +
  theme_bw()

