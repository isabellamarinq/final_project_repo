library(ggplot2)
library(tidyverse)

#Reading Data

tab2data_full <- readRDS("objects/lnrri_table_data_full.rds")

#Forming Table

summary_old <- rep(NA, 10)

for (i in c(1972, 1973, 1982, 1983, 1992, 1993, 2002, 2003, 2012, 2013)) {
  output3 <- tab2data_full |>
    filter(year == i) |>
    summarize(
      median(lnrri), 
      min(lnrri), 
      max(lnrri), 
      quantile(lnrri, .1),
      quantile(lnrri, .9)
    )
  
  summary_old = rbind(summary_old, output3) 
}

summary_old <- summary_old[ -1, ]

summary_old <- summary_old |>
  mutate(range = `max(lnrri)` - `min(lnrri)`, year = c(1972, 1973, 1982, 1983, 1992, 1993, 2002, 2003, 2012, 2013))

summary_old |>
  select(year, `median(lnrri)`, range, `quantile(lnrri, 0.1)`, `quantile(lnrri, 0.9)`) |>
  mutate_if(is.numeric, round, digits = 2)
