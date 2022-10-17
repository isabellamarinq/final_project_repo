library(ggplot2)
library(tidyverse)
library(patchwork)

#Reading Data
graph1data_og <- readRDS("objects/graph1data_og.rds")
graph2data_og <- readRDS("objects/graph2data_og.rds")
graph3data_og <- readRDS("objects/graph3data_og.rds")

#Graphs

g1_og <- graph1data_og |>
  ggplot(aes(x = year, y = share_seats, group = sign_change)) +
  geom_line(aes(linetype = as.factor(sign_change))) +
  labs(x = "Year", y = "Share of House of Representatives") 

g2_og <- graph2data_og |>
  ggplot(aes(x = year, y = share_pop, group = sign_change)) +
  geom_line(aes(linetype = as.factor(sign_change))) +
  labs(x = "Year", y = "Share of Total Population")

g3_og <- graph3data_og |>
  ggplot(aes(x = year, y = share_outlays, group = sign_change)) +
  geom_line(aes(linetype = as.factor(sign_change))) +
  labs(x = "Fiscal Year", y = "Share of Federal Outlays")

#Compiling Graphs

g1_og + g2_og + g3_og + plot_layout(nrow = 3)
