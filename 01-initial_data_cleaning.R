library(haven)
library(tidyverse)
library(readxl)
library(tibble)

data <- read_xlsx("data/ElisReplicationData.xlsx")
old_data <- read_dta("data/ReplicationData.dta")

data <- data[ , c(1, 2, 3, 4, 5, 6)]   #getting rid of weird columns at the end

data <- data |>
  rename("year" = "Year", "state" = "State") |>
  filter(!state == "District of Columbia")

states_key <- tibble::tribble(
  ~state, ~fips,
  "Alabama",    1,
  "Alaska",    2,
  "Arizona",    4,
  "Arkansas",    5,
  "California",    6,
  "Colorado",    8,
  "Connecticut",    9,
  "Delaware",   10,
  "Florida",   12,
  "Georgia",   13,
  "Hawaii",   15,
  "Idaho",   16,
  "Illinois",   17,
  "Indiana",   18,
  "Iowa",   19,
  "Kansas",   20,
  "Kentucky",   21,
  "Louisiana",   22,
  "Maine",   23,
  "Maryland",   24,
  "Massachusetts",   25,
  "Michigan",   26,
  "Minnesota",   27,
  "Mississippi",   28,
  "Missouri",   29,
  "Montana",   30,
  "Nebraska",   31,
  "Nevada",   32,
  "New Hampshire",   33,
  "New Jersey",   34,
  "New Mexico",   35,
  "New York",   36,
  "North Carolina",   37,
  "North Dakota",   38,
  "Ohio",   39,
  "Oklahoma",   40,
  "Oregon",   41,
  "Pennsylvania",   42,
  "Rhode Island",   44,
  "South Carolina",   45,
  "South Dakota",   46,
  "Tennessee",   47,
  "Texas",   48,
  "Utah",   49,
  "Vermont",   50,
  "Virginia",   51,
  "Washington",   53,
  "West Virginia",   54,
  "Wisconsin",   55,
  "Wyoming",   56
)

old_data <- states_key |>
  left_join(old_data, by = "fips")

years <- c(1965:2020)
states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",  "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

states_rep <- rep(states, 56)
years_rep <- rep(years, 50)

years_df <- data.frame(years_rep)

years_df <- years_df |>
  arrange(years_rep)

years_order <- apply(years_df, 2, as.list)

years_order <- years_order$years_rep

years_states_df <- data.frame(unlist(years_order), unlist(states_rep))

years_states_df <- years_states_df |>
  rename("state" = unlist.states_rep., "year" = unlist.years_order.)

joint_data <- years_states_df |>
  left_join(data, by = c("year", "state"))

joint_data <- joint_data |>
  left_join(old_data, by = c("year", "state"))

joint_data <- joint_data |>
  unite("Pop_joint", c("Pop", "pop_census"), na.rm = TRUE, remove = FALSE)

joint_data <- joint_data |>
  unite("Rep_joint", c("Rep", "seats_budget"), na.rm = TRUE, remove = FALSE)

joint_data$Pop_joint <- sub(".*\\_","",joint_data$Pop_joint)

joint_data$Rep_joint <- sub(".*\\_","",joint_data$Rep_joint)

joint_data <- joint_data |>
  mutate("Pop_joint" = as.numeric(Pop_joint))

joint_data <- joint_data |>
  mutate("Rep_joint" = as.numeric(Rep_joint))

saveRDS(data, "objects/data.rds")
saveRDS(old_data, "objects/old_data.rds")
saveRDS(years, "objects/years.rds")
saveRDS(joint_data, "objects/joint_data.rds")
