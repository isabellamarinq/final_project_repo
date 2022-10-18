library(haven)
library(tidyverse)
library(readxl)

data <- readRDS("objects/data.rds")
old_data <- readRDS("objects/old_data.rds")
joint_data <- readRDS("objects/joint_data.rds")

##data for funds comparison plot

new_funds <- data |>
  select(year, state_local_revenue_dollars, fed_expenditures_dollars, Pop) |>
  group_by(year) |>
  summarise_all(sum) |>
  mutate(
    revenue_percapita = state_local_revenue_dollars / Pop,
    expenditures_percapita = fed_expenditures_dollars / Pop
    )

old_funds <- old_data |>
  select(year, fedex_dollars, pop_census) |>
  group_by(year) |>
  summarise_all(sum) |>
  mutate(outlays_percapita = fedex_dollars / pop_census)

funds_comp_data <- readRDS("objects/years.rds")

funds_comp_data <- as.data.frame(funds_comp_data)

funds_comp_data <- funds_comp_data |>
  rename("year" = "funds_comp_data")

funds_comp_data <- funds_comp_data |>
  left_join(old_funds, by = "year")

funds_comp_data <- funds_comp_data |>
  left_join(new_funds, by = "year")

funds_comp_data <- funds_comp_data[ , c(1, 4, 8, 9)]

saveRDS(funds_comp_data, "objects/funds_comp_data.rds")

##table 2 with new data

#just new data
data <- data |>
  group_by(state) |>
  arrange(year) |>
  mutate(seats_budget_nplus1 = lead(Rep)) |>
  ungroup()

nat_pop <- new_funds |>
  select(year, Pop)

data_with_natpop <- data |>
  left_join(nat_pop, by = "year")

data_with_natpop <- data_with_natpop |>
  rename("state_pop" = Pop.x, "national_pop" = Pop.y)

data_with_natpop <- data_with_natpop |>  #RRI Equation, adding RRI variable
  mutate(lnrri = log((seats_budget_nplus1/state_pop)/(435/national_pop)))

saveRDS(data_with_natpop, "objects/lnrri_table_data.rds")

#joint data

joint_data <- joint_data |>
  group_by(state) |>
  arrange(year) |>
  mutate(seats_budget_nplus1 = lead(Rep_joint)) |>
  ungroup()

nat_pop_full <- joint_data |>
  select(year, Pop_joint) |>
  group_by(year) |>
  summarise_all(sum) 

data_with_natpop_full <- joint_data |>
  left_join(nat_pop_full, by = "year")

data_with_natpop_full <- data_with_natpop_full |>
  rename("state_pop" = Pop_joint.x, "national_pop" = Pop_joint.y)

data_with_natpop_full <- data_with_natpop_full |>  #RRI Equation, adding RRI variable
  mutate(lnrri = log((seats_budget_nplus1/state_pop)/(435/national_pop)))

saveRDS(data_with_natpop_full, "objects/lnrri_table_data_full.rds")

data_with_natpop_full <- data_with_natpop_full

##fig 3 replication

data_signs_og <- old_data |>   #data5 holds information about whether the number of seats changed from 1993 to 1994 for each state
  filter(year %in% c(1993:1994)) |>
  group_by(fips) |>
  arrange(year) |>
  pivot_wider(id_cols = fips, names_from = year, values_from = seats_budget) |>
  mutate(sign_change = sign(`1994` - `1993`)) 

old_data_change_ind <- old_data |>
  filter(year %in% c(1987:2001))

old_data_change_ind <- data_signs_og |>
  select(fips, sign_change) |>
  left_join(old_data_change_ind, by = "fips")

#necessary changes to old data to include lagged seats budget and national population

old_data_change_ind <- old_data_change_ind |>
  group_by(state) |>
  arrange(year) |>
  mutate(seats_budget_nplus1 = lead(seats_budget)) |>
  ungroup()

nat_pop_old <- old_data_change_ind |>
  select(year, pop_census) |>
  group_by(year) |>
  summarise_all(sum)

data_with_natpop_changeind_old <- old_data_change_ind |>
  left_join(nat_pop_old, by = "year")

data_with_natpop_changeind_old <- data_with_natpop_changeind_old |>
  rename("state_pop" = pop_census.x, "national_pop" = pop_census.y)

data_with_natpop_changeind_old <- data_with_natpop_changeind_old |>  #RRI Equation, adding RRI variable
  mutate(lnrri = log((seats_budget_nplus1/state_pop)/(435/national_pop)))

#data for fig3 graph1

graph1_og <- data_with_natpop_changeind_old |>
  group_by(sign_change, year) |>
  summarize(tot_seats = sum(seats_budget_nplus1)) |>
  mutate(share_seats = tot_seats/435)

subset_nat_pop <- data_with_natpop_changeind_old |>
  filter(fips == 1) |>
  select(year, national_pop)

subset_nat_pop <- subset_nat_pop[, -1]

graph1_og <- graph1_og[ , c(2, 1, 3, 4)]

saveRDS(graph1_og, "objects/graph1data_og.rds")

#data for fig3 graph2

tot_pop_og <- data_with_natpop_changeind_old |>
  group_by(sign_change, year) |>
  summarize(tot_pop = sum(state_pop))

graph2_og <- data_with_natpop_changeind_old|>
  left_join(tot_pop_og, by = c("year", "sign_change"))

graph2_og <- graph2_og |>
  mutate(share_pop = tot_pop/national_pop)

graph2_og <- graph2_og[ , c(3, 2, 4, 15)]

saveRDS(graph2_og, "objects/graph2data_og.rds")

#data for fig3 graph3

graph3_og <- data_with_natpop_changeind_old |>
  group_by(sign_change, year) |>
  summarize(tot_outlays = sum(fedex90)) 

year_outlays <- graph3_og |>
  group_by(year) |>
  summarize(yearly_outlays = sum(tot_outlays))

graph3_og <- year_outlays |>
  right_join(graph3_og, by = "year")

graph3_og <- graph3_og |>
  mutate(share_outlays = tot_outlays / yearly_outlays)

graph3_og <- graph3_og[ , c(1, 3, 2, 4, 5)]

saveRDS(graph3_og, "objects/graph3data_og.rds")

##fig 3 new

data_signs <- data_with_natpop |>   #data5 holds information about whether the number of seats changed from 2012 to 2014 for each state
  filter(year %in% c(2012, 2014)) |>
  group_by(state) |>
  arrange(year) |>
  pivot_wider(id_cols = state, names_from = year, values_from = Rep) |>
  mutate(sign_change = sign(`2014` - `2012`))

data_change_ind <- data_with_natpop |>
  filter(year %in% c(2006:2020))

data_change_ind <- data_signs |>
  select(state, sign_change) |>
  right_join(data_change_ind, by = "state")

#data for fig3 graph1

graph1 <- data_change_ind |>
  group_by(sign_change, year) |>
  summarize(tot_seats = sum(seats_budget_nplus1)) |>
  mutate(share_seats = tot_seats/435)

saveRDS(graph1, "objects/graph1data.rds")

#data for fig3 graph2

tot_pop <- data_change_ind |>
  group_by(sign_change, year) |>
  summarize(tot_pop = sum(state_pop)) 

subset_nat_pop <- data_change_ind |>
  filter(state == "Alabama") |>
  select(year, national_pop)

subset_nat_pop <- subset_nat_pop[ , -1]

graph2 <- subset_nat_pop |>
  left_join(tot_pop, by = c("year"))

graph2 <- graph2 |>
  mutate(share_pop = tot_pop/national_pop)

graph2 <- graph2[ , c(1, 3, 2, 4, 5)]

saveRDS(graph2, "objects/graph2data.rds")

#data for fig3 graph3

graph3 <- data_change_ind |>
  group_by(sign_change, year) |>
  summarize(tot_revenue = sum(state_local_revenue_dollars)) 

year_revenue <- graph3 |>
  group_by(year) |>
  summarize(yearly_revenue = sum(tot_revenue))

graph3 <- year_revenue |>
  right_join(graph3, by = "year")

graph3 <- graph3|>
  mutate(share_outlays = tot_revenue / yearly_revenue)

graph3 <- graph3[ , c(1, 3, 2, 4, 5)]

saveRDS(graph3, "objects/graph3data.rds")

##models Replication

#data for model 1

data_t3_og <- old_data |>  #getting lag to create deltarep
  group_by(state) |>
  mutate(seats_budget_lag = lag(seats_budget, n = 2))

data_t3_og <- data_t3_og |>  #Delta Rep (Independent Variable)
  mutate(deltarep = log(seats_budget) - log(seats_budget_lag))

data_t3_og <- data_t3_og |>  #Lagged Outlays (for Dependent Variable)
  group_by(state) |>
  arrange(year) |>
  mutate(share_outlays_lag2 = lag(fedex, n = 2),
         share_outlays_lag4 = lag(fedex, n = 4))

data_t3_og <- data_t3_og |>   #adding variable Diff
  filter(year %in% c(1974, 1984, 1994, 2004)) |>
  mutate(diff = log(fedex) - log(share_outlays_lag2))

data_t3_og <- data_t3_og |>   #adding variable LagDiff
  filter((year == 1974 & fips != 41) | year == 1984 | year == 1994 | year == 2004) |>
  mutate(lagdiff = log(share_outlays_lag2)  - log(share_outlays_lag4)) 

data_t3_final_og <- data_t3_og |>   #adding variable DiffDiff
  mutate(diffdiff = diff - lagdiff) |>
  filter(year %in% c(1974, 1994, 2004))

saveRDS(data_t3_final_og, "objects/model1data_og.rds")

#data for Model 2

data_t3_final_size_og <- data_t3_final_og |>  #adding size variable
  mutate(state_big = state) |>
  mutate(state_big = case_when(
    state_big == "California" ~ 1,
    state_big == "Florida" ~ 1,
    state_big == "Illinois" ~ 1,
    state_big == "Michigan" ~ 1, 
    state_big == "New York" ~ 1,
    state_big == "Ohio" ~ 1,
    state_big == "Pennsylvania" ~ 1,
    state_big == "Texas" ~ 1)) 

data_t3_final_size_og <- data_t3_final_size_og |>  #cleaning size variable
  mutate_at('state_big', ~replace_na(.,0))

saveRDS(data_t3_final_size_og, "objects/model2data_og.rds")

#data for Model 3

data_t3_final_ind_og <- data_t3_final_og |>  #creating change indicator variable
  mutate(change_indicator = sign(deltarep)) 

data_t3_final_ind_og <- data_t3_final_ind_og |>  #clearning change indicator variable
  mutate(change_indicator2 = case_when(
    change_indicator == 0 ~ 0,
    change_indicator == -1 ~ 1,
    change_indicator == 1 ~ 0)) 

saveRDS(data_t3_final_ind_og, "objects/model3data_og.rds")

##models new

#data for Model 1

data_t3 <- data |>  #getting lag to create deltarep
  group_by(state) |>
  mutate(seats_budget_lag = lag(Rep, n = 2))

data_t3 <- data_t3 |>  #Delta Rep (Independent Variable)
  mutate(deltarep = log(Rep) - log(seats_budget_lag))

data_t3 <- data_t3 |>  #Lagged Outlays (for Dependent Variable)
  group_by(state) |>
  arrange(year) |>
  mutate(share_outlays_lag2 = lag(state_local_revenue_dollars, n = 2),
         share_outlays_lag4 = lag(state_local_revenue_dollars, n = 4))

data_t3 <- data_t3 |>   #adding variable Diff
  filter(year %in% c(2004, 2014)) |>
  mutate(diff = log(state_local_revenue_dollars) - log(share_outlays_lag2))

data_t3 <- data_t3 |>   #adding variable LagDiff
  filter(year == 2004 | year == 2014) |>
  mutate(lagdiff = log(share_outlays_lag2)  - log(share_outlays_lag4)) 

data_t3_final <- data_t3 |>   #adding variable DiffDiff
  mutate(diffdiff = diff - lagdiff) |>
  filter(year %in% c(2004, 2014))

saveRDS(data_t3_final, "objects/model1data.rds")

#data for Model 2

data_t3_final_size <- data_t3_final |>  #adding size variable
  mutate(state_big = state) |>
  mutate(state_big = case_when(
    state_big == "California" ~ 1,
    state_big == "Florida" ~ 1,
    state_big == "Illinois" ~ 1,
    state_big == "Michigan" ~ 1, 
    state_big == "New York" ~ 1,
    state_big == "Ohio" ~ 1,
    state_big == "Pennsylvania" ~ 1,
    state_big == "Texas" ~ 1)) 

data_t3_final_size <- data_t3_final_size |>  #cleaning size variable
  mutate_at('state_big', ~replace_na(.,0))

saveRDS(data_t3_final_size, "objects/model2data.rds")

#data for Model 3

data_t3_final_ind <- data_t3_final |>  #creating change indicator variable
  mutate(change_indicator = sign(deltarep)) 

data_t3_final_ind <- data_t3_final_ind |>  #clearning change indicator variable
  mutate(change_indicator2 = case_when(
    change_indicator == 0 ~ 0,
    change_indicator == -1 ~ 1,
    change_indicator == 1 ~ 0)) 

saveRDS(data_t3_final_ind, "objects/model3data.rds")

##models joint

#data set indicator

joint_data <- joint_data |>
  mutate(new_dataset = year) |>
  mutate(
    new_dataset = case_when(
      new_dataset < 2007 ~ "0",
      new_dataset > 2006 ~ "1")
  ) |>
  mutate(new_dataset = as.numeric(new_dataset))

#data for Model 1

data_t3_j <- joint_data |>  #getting lag to create deltarep
  group_by(state) |>
  mutate(seats_budget_lag = lag(Rep_joint, n = 2))

data_t3_j <- data_t3_j |>  #Delta Rep (Independent Variable)
  mutate(deltarep = log(Rep_joint) - log(seats_budget_lag))

data_t3_j <- data_t3_j |>  #Lagged Outlays (for Dependent Variable)
  group_by(state) |>
  arrange(year) |>
  mutate(share_outlays_lag2 = lag(Funds_joint, n = 2),
         share_outlays_lag4 = lag(Funds_joint, n = 4))

data_t3_j <- data_t3_j |>   #adding variable Diff
  filter(year %in% c(1974, 1984, 1994, 2004, 2014)) |>
  mutate(diff = log(Funds_joint) - log(share_outlays_lag2))

data_t3_j <- data_t3_j |>   #adding variable LagDiff
  filter((year == 1974 & fips != 41) | year == 1984 | year == 1994 |year == 2004 | year == 2014) |>
  mutate(lagdiff = log(share_outlays_lag2)  - log(share_outlays_lag4)) 

data_t3_final_j <- data_t3_j |>   #adding variable DiffDiff
  mutate(diffdiff = diff - lagdiff) |>
  filter(year %in% c(1974, 1994, 2004, 2014))

saveRDS(data_t3_final_j, "objects/model1data_j.rds")

#data for Model 2

data_t3_final_size_j <- data_t3_final_j |>  #adding size variable
  mutate(state_big = state) |>
  mutate(state_big = case_when(
    state_big == "California" ~ 1,
    state_big == "Florida" ~ 1,
    state_big == "Illinois" ~ 1,
    state_big == "Michigan" ~ 1, 
    state_big == "New York" ~ 1,
    state_big == "Ohio" ~ 1,
    state_big == "Pennsylvania" ~ 1,
    state_big == "Texas" ~ 1)) 

data_t3_final_size_j <- data_t3_final_size_j |>  #cleaning size variable
  mutate_at('state_big', ~replace_na(.,0))

saveRDS(data_t3_final_size_j, "objects/model2data_j.rds")

#data for Model 3

data_t3_final_ind_j <- data_t3_final_j |>  #creating change indicator variable
  mutate(change_indicator = sign(deltarep)) 

data_t3_final_ind_j <- data_t3_final_ind_j |>  #clearning change indicator variable
  mutate(change_indicator2 = case_when(
    change_indicator == 0 ~ 0,
    change_indicator == -1 ~ 1,
    change_indicator == 1 ~ 0)) 

saveRDS(data_t3_final_ind_j, "objects/model3data_j.rds")

##maps

#maps with seat values

rep_2012 <- data |>
  filter(year == 2012) |>
  select(state, Rep)

rep_2013 <- data |>
  filter(year == 2013) |>
  select(state, Rep)

saveRDS(rep_2012, "objects/rep_2012.rds")
saveRDS(rep_2013, "objects/rep_2013.rds")

#maps with citizens per seat values

people_per_rep_2012 <- data |>
  filter(year == 2012) |>
  select(state, people_per_rep)

people_per_rep_2013 <- data |>
  filter(year == 2013) |>
  select(state, people_per_rep)

data_with_natpop_2012 <- data_with_natpop_full |>
  filter(year == 2012) |>
  select(state,lnrri)

data_with_natpop_2013 <- data_with_natpop_full |>
  filter(year == 2013) |>
  select(state,lnrri)

data_signs_og <- data_signs_og |>
  mutate(sign_change = as.factor(sign_change))

data_signs <- data_signs |>
  mutate(sign_change = as.factor(sign_change))

saveRDS(people_per_rep_2012, "objects/people_per_rep_2012.rds")
saveRDS(people_per_rep_2013, "objects/people_per_rep_2013.rds")
saveRDS(data_with_natpop_2012, "objects/lnrri_2012.rds")
saveRDS(data_with_natpop_2013, "objects/lnrri_2013.rds")
saveRDS(data_signs_og, "objects/data_signs_og.rds")
saveRDS(data_signs, "objects/data_signs.rds")
