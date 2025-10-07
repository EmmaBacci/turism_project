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
library(naniar)
library(lubridate)
library(zoo)  # <- this gives you as.yearmon

library(ggplot2)

supply_demand <- read.csv("~/Documents/Project ideas/Turism/data/raw_data/municipality_or_district_data/Turism/supply_demand_communes.csv") 


supply_demand_wide <- supply_demand %>%  pivot_wider(names_from = Indicator, values_from = DATA) %>% replace_with_na_all(condition = ~.x == "\"...\"") %>%
  rename( stays = `Overnight stays`, room_nights = `Room nights`, room_occupancy = `Room occupancy`, bed_occupancy = `Bed occupancy`, name_commune = Commune) %>% 
  filter(Month!="Total of the year")
library(readxl)
commune_canton <- read_excel("~/Documents/Project ideas/Turism/data/raw_data/municipality_or_district_data/Turism/municipality_included_code_canton_district.xlsx")



supply_demand_canton <- supply_demand_wide %>% left_join(commune_canton, by = c("name_commune"))

supply_demand_Fribourg <- supply_demand_canton %>% filter(canton_code == "FR")
df_long <- supply_demand_Fribourg %>%
  pivot_longer(
    cols = c(Establishments, Rooms, Beds, Arrivals, stays, room_nights, room_occupancy, bed_occupancy),
    names_to = "Variable",
    values_to = "Value"
  )

# Create a 'date' variable for better time axis (optional, uses year + month)
df_long <- df_long %>%mutate(Month = recode(Month, "January" = 1, "February" = 2, "March" = 3, "April" = 4,"May" =5, "June"=6, "July" =7, "August"=8, "September"=9, "October"=10, "November"=11, "December"=12))%>% 
  mutate(
    # clean year and month strings
    year_num  = as.numeric(trimws(Year)),
    month_num = as.numeric(trimws(Month)),
    
    # make a Date first to be safe
    time_date = as.Date(paste(year_num, month_num, "01", sep = "-")),
    
    # convert to numeric yearmon index
    time = as.numeric(as.yearmon(time_date))
  )  %>% mutate_at("Value", as.numeric)
# Plot

library(ggplot2)
library(ggplot2)
ggplot(df_long, aes(x = time_date, y = Value, color = name_commune, group = name_commune)) +
  geom_line() +
  facet_wrap(~Variable, scales = "free_y") +
  theme_minimal() +
  labs(title = "Evolution of Tourism Indicators by Commune",
       x = "Year",
       y = "Value",
       color = "Commune")+
  scale_x_date(
    date_breaks = "3 months",         # tick marks every 3 months (can change to "1 month")
    date_labels = "%b %Y"             # format = "Jan 2020"
  )


# get unique variable names
vars <- unique(df_long$Variable)

# create a named list of plots
plots <- lapply(vars, function(v) {
  ggplot(filter(df_long, Variable == v),
         aes(x = time_date, y = Value, color = name_commune, group = name_commune)) +
    geom_line() +
    theme_minimal() +
    labs(
      title = "Evolution of Tourism Indicators by Commune",
      x = "Date",
      y = "Value",
      color = "Commune"
    ) +
    scale_x_date(
      date_breaks = "3 months",
      date_labels = "%b %Y"   # "Jan 2020"
    ) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)  # vertical labels
    )
})
names(plots) <- vars
plots[["Arrivals"]]
ggplot(df_long, aes(x = time_date, y = Value, color = name_commune, group = name_commune)) +
  geom_line() +
  facet_wrap(~Variable, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Evolution of Tourism Indicators by Commune",
    x = "Month",
    y = "Value",
    color = "Commune"
  ) +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"   # "Jan 2020"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)  # vertical labels
  )

arrivals_stays_by_country_communes <- read.csv("~/Documents/Project ideas/Turism/data/raw_data/municipality_or_district_data/Turism/arrivals_stays_by_country_communes.csv")
stays_by_country <- arrivals_stays_by_country_communes %>%  pivot_wider(names_from = Indicator, values_from = DATA) %>% 
  rename( country_origin = Visitors..country.of.residence...total,name_commune = Commune, stays = `Overnight stays`) %>% 
  filter(Month!="Total of the year") %>% filter(country_origin!= "Visitors' country of residence - total")

#Value = DATA, Variable = Indicator
library(data.table)

setDT(stays_by_country)  # convert to data.table

for (j in names(stays_by_country)) {
  set(stays_by_country, i = which(stays_by_country[[j]] == "\"...\""), j = j, value = NA)
}

stays_by_country <- as.data.frame(stays_by_country)



stays_by_country_canton <- stays_by_country %>% left_join(commune_canton, by = c("name_commune"))


stays_by_country_Fribourg <- stays_by_country_canton %>% filter(canton_code == "FR")
df_long <- stays_by_country_Fribourg  %>%
  pivot_longer(
    cols = c(Arrivals),
    names_to = "Variable",
    values_to = "Value"
  )


# Create a 'date' variable for better time axis (optional, uses year + month)
df_long <- df_long %>%mutate(Month = recode(Month, "January" = 1, "February" = 2, "March" = 3, "April" = 4,"May" =5, "June"=6, "July" =7, "August"=8, "September"=9, "October"=10, "November"=11, "December"=12))%>% 
  mutate(
    # clean year and month strings
    year_num  = as.numeric(trimws(Year)),
    month_num = as.numeric(trimws(Month)),
    
    # make a Date first to be safe
    time_date = as.Date(paste(year_num, month_num, "01", sep = "-")),
    
    # convert to numeric yearmon index
    time = as.numeric(as.yearmon(time_date))
  )  %>% mutate_at("Value", as.numeric)
# Plot
df_long_non_swiss <- df_long %>% filter(country_origin != c("Switzerland","Germany", "France", "Italy", "China", "United Kingdom", "Spain", "Austria"))
df_long_pop<- df_long %>% filter(country_origin == c("Germany", "France", "Italy", "China", "United Kingdom", "Spain", "Austria"))
df_spain <- df_long %>% filter(country_origin == c("Spain"))
ggplot(df_long_pop, aes(x = time_date, y = Value, color = country_origin, group = country_origin)) +
  geom_line() +
  facet_wrap(~name_commune, scales = "free_y") +
  theme_minimal() +
  labs(title = "Evolution of Tourism Indicators by Commune",
       x = "Year",
       y = "Value",
       color = "Commune") +
  scale_x_date(
    date_breaks = "3 months",
    date_labels = "%b %Y"   # "Jan 2020"
  ) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)  # vertical labels
  )
top_n <- 200  
max <-df_long_non_swiss %>%  arrange(desc(Value)) %>% slice_head(n = top_n) 
max <-df_long %>%  group_by(country_origin) %>% slice_max(Value, n = 1, with_ties = TRUE)
