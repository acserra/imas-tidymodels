library(rpart.plot)

specification_decision_tree <- parsnip::decision_tree(
  tree_depth = 4,
  cost_complexity = 0.005,
  min_n = 4
) %>%
  set_mode("regression") %>%
  set_engine("rpart")

my_fitted_model_decision_tree <- parsnip::fit(specification_decision_tree, delta_13_c~
                                                bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g + 
                                                #delta_15_n +  + 
                                                sex + species + year + island,
                                              data = penguins_train)

rpart.plot::rpart.plot(my_fitted_model_decision_tree$fit)
rpart.plot::prp(my_fitted_model_decision_tree$fit)
