library(ggplot2)
library(tidyverse)

#Reading Data
model1data_og <- readRDS("objects/model1data_og.rds")
model2data_og <- readRDS("objects/model2data_og.rds")
model3data_og <- readRDS("objects/model3data_og.rds")

#Models
(model1 <- lm(diffdiff ~ deltarep, data = model1data_og))
(model2 <- lm(diffdiff ~ deltarep + deltarep*state_big + factor(year), data = model2data_og)) 
(model3 <- lm(diffdiff ~ deltarep + deltarep*change_indicator2, data = model3data_og))
