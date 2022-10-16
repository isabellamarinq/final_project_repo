library(ggplot2)
library(usmap)

rep2012_map_data <- readRDS("objects/rep_2012.rds")
rep2013_map_data <- readRDS("objects/rep_2013.rds")
ppr2012_map_data <- readRDS("objects/people_per_rep_2012.rds")
ppr2013_map_data <- readRDS("objects/people_per_rep_2013.rds")
change_map_data <- readRDS("data_signs.rds")
change_map_data_old <- readRDS("data_signs_og.rds")
  
plot_usmap(data = rep2012_map_data, values = "Rep", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "Representatives in the House (2012)", 
    label = scales::comma) + 
  theme(legend.position = "right") +
  labs(title = "Seats in the House of Representatives by State (2012)")

plot_usmap(data = rep2013_map_data, values = "Rep", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "Representatives in the House (2013)", 
    label = scales::comma) + 
  theme(legend.position = "right") +
  labs(title = "Seats in the House of Representatives by State (2013)")

plot_usmap(data = ppr2012_map_data, values = "people_per_rep", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "People per Representative in the House (2012)", 
    label = scales::comma) + 
  theme(legend.position = "right") +
  labs(title = "People per Seat by State (2012)")

plot_usmap(data = ppr2013_map_data, values = "people_per_rep", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "People per Representative in the House (2013)", 
    label = scales::comma) + 
  theme(legend.position = "right") +
  labs(title = "People per Seat by State (2013)")

plot_usmap(data = change_map_data, values = "sign_change", color = "black") + 
  scale_fill_manual(
    values=c("white", "red", "pink"),
    name = "Change in Representatives Indicator (1992-1994)") + 
  theme(legend.position = "right") +
  labs(title = "States with Changes in Number of Federal Representatives (2012-2014)")

plot_usmap(data = change_map_data_old, values = "sign_change", color = "black") + 
  scale_fill_manual(
    values=c("white", "red", "pink"),
    name = "Change in Representatives Indicator (1992-1994)") +
  theme(legend.position = "right") +
  labs(title = "States with Changes in Number of Federal Representatives (1992-1994)")
