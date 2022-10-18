library(ggplot2)
library(tidyverse)
library(modelsummary)

test <- model1data_j |>
  filter(year < 2007)

lm(diffdiff ~ deltarep + + factor(year), data = test)

#Reading Data
model1data_og <- readRDS("objects/model1data_og.rds")
model2data_og <- readRDS("objects/model2data_og.rds")
model3data_og <- readRDS("objects/model3data_og.rds")

model1data <- readRDS("objects/model1data.rds")
model2data <- readRDS("objects/model2data.rds")
model3data <- readRDS("objects/model3data.rds")

model1data_j <- readRDS("objects/model1data_j.rds")
model2data_j <- readRDS("objects/model2data_j.rds")
model3data_j <- readRDS("objects/model3data_j.rds")

#Models
(model1_og <- lm(diffdiff ~ deltarep + factor(year), data = model1data_og))
(model2_og <- lm(diffdiff ~ deltarep + deltarep:state_big + factor(year), data = model2data_og)) 
(model3_og <- lm(diffdiff ~ deltarep + deltarep:change_indicator2 + factor(year), data = model3data_og))

#Models new data
(model1 <- lm(diffdiff ~ deltarep + factor(year), data = model1data))
(model2 <- lm(diffdiff ~ deltarep + deltarep:state_big + factor(year), data = model2data)) 
(model3 <- lm(diffdiff ~ deltarep + deltarep:change_indicator2 + factor(year), data = model3data))

#Models joint
(model1_j <- lm(diffdiff ~ deltarep + new_dataset + factor(year), data = model1data_j))
(model2_j <- lm(diffdiff ~ deltarep + deltarep:state_big + factor(year) + new_dataset, data = model2data_j)) 
(model3_j <- lm(diffdiff ~ deltarep + deltarep:change_indicator2 + new_dataset + factor(year), data = model3data_j))

models <- list(
  "Share Outlays OLS Replication" = model1_og, 
  "Share Outlays OLS Replication w/ Size" = model2_og,
  "Share Outlays OLS Replication w/ Change Direction" = model3_og,
  "Federal State Revenue OLS" = model1,
  "Federal State Revenue OLS w/ Size" = model2,
  "Federal State Revenue OLS w/ Change Direction" = model3,
  "Joint Federal Funds OLS" = model1_j,
  "Joint Federal Funds OLS w/ Size" = model2_j,
  "Joint Federal Funds OLS w/ Change Direction" = model3_j
  )

modelsummary(
  models, 
  fmt = 3,  
  gof_map = c("nobs"),
  coef_rename = c("deltarep" = "Change in Representatives", 
                  "new_dataset" = "New Data Set",
                  "factor(year)1994" = "1994 Fixed Effects",
                  "factor(year)2004" = "2004 Fixed Effects",
                  "deltarep:state_big" = "Delta Rep X State Size",
                  "deltarep:change_indicator2" = "Delta Rep X Lose Seats",
                  "factor(year)2014" = "2014 Fixed Effects"),
  statistic = "{std.error} ({p.value})",
  stars = TRUE,
  coef_omit = "(Intercept) | Big State | 1994 Fixed Effects | 2004 Fixed Effects | Direction of Change | 2014 Fixed Effects"
  )
