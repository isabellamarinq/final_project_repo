library(ggplot2)
library(tidyverse)

#Reading Data
model1data <- readRDS("objects/model1data.rds")
model2data <- readRDS("objects/model2data.rds")
model3data <- readRDS("objects/model3data.rds")

#Models
(model1 <- lm(diffdiff ~ deltarep, data = model1data))
(model2 <- lm(diffdiff ~ deltarep + deltarep*state_big + factor(year), data = model2data)) 
(model3 <- lm(diffdiff ~ deltarep + deltarep*change_indicator2, data = model3data))
