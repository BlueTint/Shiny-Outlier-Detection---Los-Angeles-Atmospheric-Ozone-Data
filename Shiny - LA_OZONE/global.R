### global.R ###

# Load libraries
library(shiny)
library(shinyjs)
library(recipes)
library(DT)
library(dbscan)
library(fpc)
library(ggplot2)
library(factoextra)
library(shinythemes)
library(dplyr)
library(gss)
# library(Cairo)
library(quantregForest)
library(ggrepel)
library(e1071)
library(shinyBS)
library(bsplus)
library(shinyWidgets)
library(ggbeeswarm)
library(ggstatsplot)
library(rlang)
library(heatmaply)
library(tibble)
library(tidyverse)

# Load 'Ozone' data
data(ozone)
data <- ozone

# Sample and split data into training and test data
sample_size = nrow(data) * 0.7
set.seed(101)
data_id  <- sample(seq_len(nrow(data)), size = sample_size)
data_tr <- data[data_id, ]
data_te <- data[-data_id, ]

# Apply yeo-johnson transform
yj_estimates <- recipe(upo3 ~ ., data = data_tr) %>%
  update_role(day, new_role = "ID variable") %>%
  step_naomit(everything()) %>%
  step_nzv(all_predictors()) %>% 
  step_lincomb(all_predictors()) %>%
  step_YeoJohnson(all_predictors()) %>%
  prep(data = data_tr)
yj_te <- bake(yj_estimates, data_te)[,c(9,1,2,3,4,5,6,7,8,10)] 

# Non-applied yeo-johnson transform
without_yj_estimates <- recipe(upo3 ~ ., data = data_tr) %>%
  update_role(day, new_role = "ID variable") %>%
  step_naomit(everything()) %>%
  step_nzv(all_predictors()) %>% 
  step_lincomb(all_predictors()) %>%
  prep(data = data_tr)

# Test data with transform
without_yj_te <- bake(without_yj_estimates, data_te)[,c(9,1,2,3,4,5,6,7,8,10)]  

# Train data with transform
without_yj_tr <- juice(without_yj_estimates)[,c(9,1,2,3,4,5,6,7,8,10)] # original training data
x = without_yj_tr[,2:9]
y = without_yj_tr$upo3