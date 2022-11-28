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
lnrri2003_map_data <- readRDS("objects/lnrri_2003.rds")
lnrri1993_map_data <- readRDS("objects/lnrri_1993.rds")
lnrri1983_map_data <- readRDS("objects/lnrri_1983.rds")
lnrri1973_map_data <- readRDS("objects/lnrri_1973.rds")
lnroi2013_map_data <- readRDS("objects/lnroi_2013.rds")
lnroi2003_map_data <- readRDS("objects/lnroi_2003.rds")
lnroi1993_map_data <- readRDS("objects/lnroi_1993.rds")
lnroi1983_map_data <- readRDS("objects/lnroi_1983.rds")
lnroi1973_map_data <- readRDS("objects/lnroi_1973.rds")

#Composite lnROI map
lnroimaps <- lnroi2003_map_data |>
  right_join(lnroi2013_map_data, by = "state")

lnroimaps <- lnroi1993_map_data |>
  right_join(lnroimaps, by = "state")

lnroimaps <- lnroi1983_map_data |>
  right_join(lnroimaps, by = "state")

lnroimaps <- lnroi1973_map_data |>
  right_join(lnroimaps, by = "state")

lnroimaps_final <- cbind(lnroimaps[1], stack(lnroimaps[2:6]))

plot_usmap(data = lnroimaps_final, values = "values", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "ln(RRI)", 
    label = scales::comma) + 
  facet_wrap(~ ind, nrow = 3) +
  theme(legend.position = "right") +
  labs(title = "Log ROI per State (1973-2013)")

ggsave("maps.png", device = "png", path = "figures", width = 10, height = 16, units = 'in')

#Composite lnRRI map
lnrrimaps <- lnrri2003_map_data |>
  right_join(lnrri2013_map_data, by = "state")

lnrrimaps <- lnrri1993_map_data |>
  right_join(lnrrimaps, by = "state")

lnrrimaps <- lnrri1983_map_data |>
  right_join(lnrrimaps, by = "state")

lnrrimaps <- lnrri1973_map_data |>
  right_join(lnrrimaps, by = "state")

lnrrimaps_final <- cbind(lnrrimaps[1], stack(lnrrimaps[2:6]))

plot_usmap(data = lnrrimaps_final, values = "values", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "ln(RRI)", 
    label = scales::comma) + 
  facet_wrap(~ ind, nrow = 3) +
  theme(legend.position = "right") +
  labs(title = "Log RRI per State (1973-2013)")

ggsave("maps.png", device = "png", path = "figures", width = 10, height = 16, units = 'in')
  
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

m2013 <- plot_usmap(data = lnrri2013_map_data, values = "lnrri", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "ln(RRI) (2013)", 
    label = scales::comma) + 
  theme(legend.position = "right") +
  labs(title = "Log RRI per State (2013)")

ggsave("lnrri_2013_map.pdf", device = "pdf", path = "figures")

m2003 <- plot_usmap(data = lnrri2003_map_data, values = "lnrri", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "ln(RRI) (2003)", 
    label = scales::comma) + 
  theme(legend.position = "right") +
  labs(title = "Log RRI per State (2003)")

ggsave("lnrri_2003_map.pdf", device = "pdf", path = "figures")

m1993 <- plot_usmap(data = lnrri1993_map_data, values = "lnrri", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "ln(RRI) (1993)", 
    label = scales::comma) + 
  theme(legend.position = "right") +
  labs(title = "Log RRI per State (1993)")

ggsave("lnrri_1993_map.pdf", device = "pdf", path = "figures")

m1983 <- plot_usmap(data = lnrri1983_map_data, values = "lnrri", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "ln(RRI) (1983)", 
    label = scales::comma) + 
  theme(legend.position = "right") +
  labs(title = "Log RRI per State (1983)")

m1973 <- plot_usmap(data = lnrri1973_map_data, values = "lnrri", color = "black") + 
  scale_fill_continuous(
    low = "white", 
    high = "darkblue",
    name = "ln(RRI) (1973)", 
    label = scales::comma) + 
  theme(legend.position = "right") +
  labs(title = "Log RRI per State (1973)")

ggsave("lnrri_1973_map.pdf", device = "pdf", path = "figures")

m2013 + m2003 + m1993 + m1983 + m1973

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
