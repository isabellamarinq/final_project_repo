library(ggplot2)
library(usmap)
library(here)

rep2012_map_data <- readRDS("objects/rep_2012.rds")
rep2013_map_data <- readRDS("objects/rep_2013.rds")
ppr2012_map_data <- readRDS("objects/people_per_rep_2012.rds")
ppr2013_map_data <- readRDS("objects/people_per_rep_2013.rds")
change_map_data <- readRDS("objects/data_signs.rds")
change_map_data_old <- readRDS("objects/data_signs_og.rds")
lnrri2012_map_data <- readRDS("objects/lnrri_2012.rds")
lnrri2013_map_data <- readRDS("objects/lnrri_2013.rds")
  
plot_usmap(data = rep2012_map_data, values = "Rep", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "Representatives in the House (2012)", 
    label = scales::comma) + 
  theme(legend.position = "right") +
  labs(title = "Seats in the House of Representatives by State (2012)")

ggsave("seats2012_map.pdf", device = "pdf", path = "figures")

plot_usmap(data = rep2013_map_data, values = "Rep", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "Representatives in the House (2013)", 
    label = scales::comma) + 
  theme(legend.position = "right") +
  labs(title = "Seats in the House of Representatives by State (2013)")

ggsave("seats2013_map.pdf", device = "pdf", path = "figures")

plot_usmap(data = ppr2012_map_data, values = "people_per_rep", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "People per Representative in the House (2012)", 
    label = scales::comma) + 
  theme(legend.position = "right") +
  labs(title = "People per Seat by State (2012)")

ggsave("peopleperseat_2012_map.pdf", device = "pdf", path = "figures")

plot_usmap(data = ppr2013_map_data, values = "people_per_rep", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "People per Representative in the House (2013)", 
    label = scales::comma) + 
  theme(legend.position = "right") +
  labs(title = "People per Seat by State (2013)")

ggsave("peopleperseat_2013_map.pdf", device = "pdf", path = "figures")

plot_usmap(data = lnrri2012_map_data, values = "lnrri", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "ln(RRI) (2012)", 
    label = scales::comma) + 
  theme(legend.position = "right") +
  labs(title = "Log RRI per State (2012)")

ggsave("lnrri_2012_map.pdf", device = "pdf", path = "figures")

plot_usmap(data = lnrri2013_map_data, values = "lnrri", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "ln(RRI) (2013)", 
    label = scales::comma) + 
  theme(legend.position = "right") +
  labs(title = "Log RRI per State (2013)")

ggsave("lnrri_2013_map.pdf", device = "pdf", path = "figures")

plot_usmap(data = change_map_data, values = "sign_change", color = "black") + 
  scale_fill_manual(
    values=c("white", "red", "pink"),
    name = "Change in Representatives Indicator (2012-2014)") + 
  theme(legend.position = "right") +
  labs(title = "States with Changes in Number of Federal Representatives (2012-2014)")

ggsave("change_new_map.pdf", device = "pdf", path = "figures")

plot_usmap(data = change_map_data_old, values = "sign_change", color = "black") + 
  scale_fill_manual(
    values=c("white", "red", "pink"),
    name = "Change in Representatives Indicator (1992-1994)") +
  theme(legend.position = "right") +
  labs(title = "States with Changes in Number of Federal Representatives (1992-1994)")

ggsave("change_old_map.pdf", device = "pdf", path = "figures")
