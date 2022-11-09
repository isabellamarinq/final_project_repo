library(ggplot2)
library(tidyverse)
library(modelsummary)
library(fixest)

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

#Old Models - IGNORE
model1_og2 <- lm(diffdiff ~ deltarep + factor(year), data = model1data_og)
model2_og2 <- lm(diffdiff ~ deltarep + deltarep:state_big + factor(year), data = model2data_og)
model3_og2 <- lm(diffdiff ~ deltarep + deltarep:change_indicator2 + factor(year), data = model3data_og)

model1_2 <- lm(diffdiff ~ deltarep + factor(year), data = model1data)
model2_2 <- lm(diffdiff ~ deltarep + deltarep:state_big + factor(year), data = model2data)
model3_2 <- lm(diffdiff ~ deltarep + deltarep:change_indicator2 + factor(year), data = model3data)

model1_j2 <- lm(diffdiff ~ deltarep + new_dataset + factor(year), data = model1data_j)
model2_j2 <- lm(diffdiff ~ deltarep + deltarep:state_big + factor(year) + new_dataset, data = model2data_j)
model3_j2 <- lm(diffdiff ~ deltarep + deltarep:change_indicator2 + new_dataset + factor(year), data = model3data_j)

#Models
model1_og <- fixest::feols(diffdiff ~ deltarep | year, data = model1data_og, vcov = ~ state)
model2_og <- fixest::feols(diffdiff ~ deltarep + deltarep:state_big | year, data = model2data_og, vcov = ~ state)
model3_og <- fixest::feols(diffdiff ~ deltarep + deltarep:change_indicator2 | year, data = model3data_og, vcov = ~ state)

#Models new data
model1 <- fixest::feols(diffdiff ~ deltarep | year, data = model1data, vcov = ~ state)
model2 <- fixest::feols(diffdiff ~ deltarep + deltarep:state_big | year, data = model2data, vcov = ~ state)
model3 <- fixest::feols(diffdiff ~ deltarep + deltarep:change_indicator2 | year, data = model3data, vcov = ~ state)

#Models joint
model1_j <- fixest::feols(diffdiff ~ deltarep + new_dataset | year, data = model1data_j, vcov = ~ state)
model2_j <- fixest::feols(diffdiff ~ deltarep + deltarep:state_big + new_dataset | year, data = model2data_j, vcov = ~ state)
model3_j <- fixest::feols(diffdiff ~ deltarep + deltarep:change_indicator2 + new_dataset | year, data = model3data_j, vcov = ~ state)

#Model Lists

models_new <- list(
  "Federal State Revenue OLS" = model1,
  "Federal State Revenue OLS w/ Size" = model2,
  "Federal State Revenue OLS w/ Seats Loss" = model3
  )

models_old <- list(
  "Share Outlays OLS Replication" = model1_og, 
  "Share Outlays OLS Replication w/ Size" = model2_og,
  "Share Outlays OLS Replication w/ Seats Loss" = model3_og
  )

models_joint <- list(
  "Joint Federal Funds OLS" = model1_j,
  "Joint Federal Funds OLS w/ Size" = model2_j,
  "Joint Federal Funds OLS w/ Seats Loss" = model3_j
  )

#Model Summaries

modelsummary(
  models_new, 
  fmt = 3,  
  gof_map = NA,
  coef_map = c("deltarep" = "Change in Representatives", 
               "deltarep:state_big" = "Change in Rep X Large State",
               "deltarep:change_indicator2" = "Change in Rep X Lost Seats"),
  statistic = NULL,
  estimate = "{estimate} ({std.error})",
  notes = list('n = 100', 'Control variables not included on the table are year fixed effects.', 'Estimates, with standard errors in parnetheses. Outcome is measured in logged millions of dollars units and the Change in Representatives variable is measured in logged Congressional seats. Standard Error is clustered by state.'),
  title = "Effects of Changes in Logged Congressional Representation on First Difference in Changes in Logged State Revenue from the Federal Government Shares"
)

modelsummary(
  models_old, 
  fmt = 3,  
  gof_map = NA,
  statistic = NULL,
  coef_map = c("deltarep" = "Change in Representatives", 
               "deltarep:state_big" = "Change in Rep X State Size",
               "deltarep:change_indicator2" = "Change in Rep X Lost Seats"),
  estimate = "{estimate} ({std.error})",
  notes = list('n = 149', 'Control variables not included on the table are year fixed effects', 'Estimates, with standard errors in parnetheses. Outcome is measured in logged millions of dollars units and the Change in Representatives variable is measured in logged Congressional seats. Standard Error is clustered by state.'),
  title = "Effects of Changes in Logged Congressional Representation on First Difference in Changes in Logged Outlay Shares"
)

modelsummary(
  models_joint, 
  fmt = 3,  
  gof_map = NA,
  coef_map = c("deltarep" = "Change in Representatives", 
               "deltarep:state_big" = "Change in Rep X State Size",
               "deltarep:change_indicator2" = "Change in Rep X Lost Seats"),
  statistic = NULL,
  estimate = "{estimate} ({std.error})",
  notes = list('n = 199', 'Control variables not included on the table are year fixed effects and an indicator variable of whether the data point is from new data.', 'Estimates, with standard errors in parnetheses. Outcome is measured in logged millions of dollars units and the Change in Representatives variable is measured in logged Congressional seats. Standard Error is clustered by state.'),
  title = "Effects of Changes in Logged Congressional Representation on First Difference in Changes in Logged Federal Funds"
)
