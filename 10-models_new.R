library(ggplot2)
library(tidyverse)

#Reading Data
model1data <- readRDS("objects/model1data.rds")
model2data <- readRDS("objects/model2data.rds")
model3data <- readRDS("objects/model3data.rds")

model1data_j <- readRDS("objects/model1data_j.rds")
model2data_j <- readRDS("objects/model2data_j.rds")
model3data_j <- readRDS("objects/model3data_j.rds")

#Models new data
(model1 <- lm(diffdiff ~ deltarep, data = model1data))
(model2 <- lm(diffdiff ~ deltarep + deltarep*state_big + factor(year), data = model2data)) 
(model3 <- lm(diffdiff ~ deltarep + deltarep*change_indicator2, data = model3data))

#Models joint

(model1_j <- lm(diffdiff ~ deltarep + new_dataset, data = model1data_j))
(model2_j <- lm(diffdiff ~ deltarep + deltarep*state_big + factor(year) + new_dataset, data = model2data_j)) 
(model3_j <- lm(diffdiff ~ deltarep + deltarep*change_indicator2 + new_dataset, data = model3data_j))

