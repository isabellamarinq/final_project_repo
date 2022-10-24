library(ggplot2)
library(tidyverse)

#Reading Data
tab2data <- readRDS("objects/lnrri_table_data.rds")

#Forming Table

summary <- rep(NA, 2)

for (i in c(2012, 2013)) {
  output3 <- tab2data |>
    filter(year == i) |>
    summarize(
      median(lnrri), 
      min(lnrri), 
      max(lnrri), 
      quantile(lnrri, .1),
      quantile(lnrri, .9)
    )
  
  summary = rbind(summary, output3) 
}

summary <- summary[-1, ]

summary <- summary |>
  mutate(range = `max(lnrri)` - `min(lnrri)`, year = c(2012, 2013))

summary |>
  select(year, `median(lnrri)`, range, `quantile(lnrri, 0.1)`, `quantile(lnrri, 0.9)`) |>
  mutate_if(is.numeric, round, digits = 2)
