library(ggplot2)
library(tidyverse)
library(geomtextpath)
library(ggrepel)

#Reading Data
fig5_data <- readRDS("objects/funds_comp_data.rds")

#Manipulating Data
fig5_data_long <- fig5_data |>
  rename('Federal Outlays Data' = outlays_percapita, 'Revenue from the Federal Government Data' = revenue_percapita, 'Federal Expenditures on States Data' = expenditures_percapita) |>
  select(year, 'Federal Outlays Data', 'Revenue from the Federal Government Data', 'Federal Expenditures on States Data') |> 
  pivot_longer(-year)

max_points <- fig5_data_long |>
  group_by(name) |>
  summarize(value = max(value, na.rm = TRUE), year = max(year, na.rm = TRUE)) |>
  mutate(
    year = ifelse(row_number() == 1, "2010", year),
    year = ifelse(row_number() == 2, "2006", year)) |>
  mutate(year = as.numeric(year))

#Making Graph
fig5_data_long |> 
  ggplot(aes(x = year, y = value, color = name)) +
  geom_line() +
  geom_point() +
  geom_text_repel(aes(label = name), data = max_points, nudge_y = 100, size = 3) +
  labs(title = "Different Measurements of Federal Funding to States", x = "Fiscal Year", y = "Dollars per Capita", colour = "Data") +
  theme(legend.position = "none")

ggsave("fig5.png", device = "png", path = "figures", width = 10, height = 8, units = 'in')
