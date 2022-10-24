library(ggplot2)
library(tidyverse)

#Reading Data
fig5_data <- readRDS("objects/funds_comp_data.rds")

#Making Graph
fig5_data |>
  ggplot(aes(x = year)) +
  geom_line(aes(y = outlays_percapita)) +
  geom_line(aes(y = revenue_percapita)) +
  geom_line(aes(y = expenditures_percapita)) +
  geom_point(aes(y = outlays_percapita)) +
  geom_point(aes(y = revenue_percapita)) +
  geom_point(aes(y = expenditures_percapita)) +
  labs(title = "Apportionment Cycles as Natural Experiments", x = "Fiscal Year", y = "Dollars")

ggsave("fig5.pdf", device = "pdf", path = "figures")