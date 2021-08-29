list.of.packages <- c("tidyverse", "tidymodels", "vip", "janitor", "palmerpenguins",
                      "lubridate", "fastshap", "tictoc", "ggcorrplot", 
                      "ranger", "xgboost", "earth", "kernlab")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)