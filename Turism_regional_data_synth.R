# Clear environment
rm(list = ls())
library(haven)
library(ggplot2)
library("dplyr")
library(gridExtra)
library("purrr")
library(tibble)
library("tidyr")
library(readxl)
library(tidysynth)

arrivals_overnight_stays_hotel_sector <- read_excel("~/Documents/Turism/data/raw_data/canton_or_region_data/Turism/arrivals_overnight_stays.xlsx", 
                                                    skip = 2)

names <- c(code_canton = "...1", canton = "...2", code_country_origin = "...3", country_origin = "...4", year = "...5", year_2 = "...6", month_number = "...7", month = "...8")
arrivals_overnight_stays_hotel_sector <- arrivals_overnight_stays_hotel_sector %>% rename(all_of(names))%>% 
                                                                                   select(-(year_2)) %>% 
                                                                                   fill(code_canton,canton, code_country_origin, country_origin, year, .direction = "down") %>% drop_na() %>% filter(canton!="Switzerland")
supply_and_demand_stays_hotel_sector <- read_excel("~/Documents/Turism/data/raw_data/canton_or_region_data/Turism/supply_and_demand.xlsx", 
                                                   skip = 2)
names <- c( year = "...1", year_2 = "...2", month_number = "...3", month = "...4", code_canton = "...5", canton = "...6")
supply_and_demand_stays_hotel_sector <- supply_and_demand_stays_hotel_sector %>% rename(all_of(names)) %>% rename( stays = `Overnight stays`, room_nights = `Room nights`, room_occupancy = `Room occupancy`, bed_occupancy = `Bed occupancy`)%>%
  select(-(year_2)) %>% 
  fill(month_number,month, year, .direction = "down") %>% drop_na() %>% filter(canton!="Switzerland")


library(dplyr)
library(zoo)

supply_and_demand_stays_hotel_sector <- supply_and_demand_stays_hotel_sector %>%
  mutate(
    # clean year and month strings
    year_num  = as.numeric(trimws(year)),
    month_num = as.numeric(trimws(month_number)),
    
    # make a Date first to be safe
    time_date = as.Date(paste(year_num, month_num, "01", sep = "-")),
    
    # convert to numeric yearmon index
    time = as.numeric(as.yearmon(time_date))
  ) %>%
  select(-c(year, month, month_number, year_num, month_num))

supply_and_demand <- supply_and_demand_stays_hotel_sector %>%
  synthetic_control(
    outcome = Arrivals,
    unit = code_canton,
    time = time,
    i_unit = 10,
    i_time = as.numeric(as.yearmon("2020-01")),
    generate_placebos = TRUE
  ) %>%
  generate_predictor(
    time_window = as.numeric(as.yearmon("2005-01")):as.numeric(as.yearmon("2019-12")),
    Establishments  = mean(Establishments, na.rm = TRUE),
    Rooms           = mean(Rooms, na.rm = TRUE),
    Beds            = mean(Beds, na.rm = TRUE),
    room_nights     = mean(room_nights, na.rm = TRUE),
    room_occupancy     = mean(room_occupancy, na.rm = TRUE),
    bed_occupancy   = mean(bed_occupancy, na.rm = TRUE),
    stays        = mean(stays, na.rm = TRUE)
  ) %>%
  generate_predictor(
    time_window = as.numeric(as.yearmon("2005-01")):as.numeric(as.yearmon("2017-12")),
    Overnight_stays_2018 = mean(Arrivals, na.rm = TRUE)
  ) %>%
  generate_predictor(
    time_window = as.numeric(as.yearmon("2018-01")):as.numeric(as.yearmon("2018-12")),
    Overnight_stays_2019_first_half = mean(Arrivals, na.rm = TRUE)
  ) %>%
  generate_predictor(
    time_window = as.numeric(as.yearmon("2019-01")):as.numeric(as.yearmon("2019-12")),
    Overnight_stays_2019_second_half = mean(Arrivals, na.rm = TRUE)
  ) %>%
  generate_weights(
    optimization_window = as.numeric(as.yearmon("2005-01")):as.numeric(as.yearmon("2020-01"))
  ) %>%
  generate_control() %>% generate_weights()
#### Output and plots ####
# Predictor and unit weights
supply_and_demand %>% grab_predictor_weights()
supply_and_demand %>% grab_unit_weights()

# Balance table (treated vs synthetic vs donor average)
supply_and_demand %>% grab_balance_table()

# Plots
supply_and_demand %>% plot_trends(time_window = as.numeric(as.yearmon("2005-01")):as.numeric(as.yearmon("2025-06")))
supply_and_demand %>% plot_differences(time_window = as.numeric(as.yearmon("2005-01")):as.numeric(as.yearmon("2025-06")))

supply_and_demand%>% plot_weights()
