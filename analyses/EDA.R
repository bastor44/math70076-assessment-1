# Load Libraries
library(tidyverse)
library(ggplot2)

# Load colour palette and helper functions
source("R/colour_palette.R")
source("R/rolling_mean.R")
source("R/pad_with_NAs.R")

# Load data
daily_data_2024 <- readRDS("data/daily_data_2024.rds")
age_group_data <- readRDS("data/age_groups_2024.rds")


##### EDA
# total number of registrations per day 
ggplot(daily_data_2024, aes(x=Date, y=total)) +
  geom_line(color=dmnblue, lwd=1.2) +
  theme_minimal()

# rolling mean number of registrations per day 
daily_data_2024$rolling_mean <- rolling_mean(daily_data_2024$total, window_width=7)

ggplot(daily_data_2024, aes(x=Date, y=total)) + 
  geom_line(color=dmnpink, lwd=1.2) + 
  geom_line(aes(x=Date, y=rolling_mean), color=dmnred, lwd=1.2) +
  theme_minimal()


# number of registrations per day by age group
# reshape to long format
long_age_groups <- age_group_data |>
  pivot_longer(cols=starts_with(c("Under", "age", "Over")), 
               names_to="AgeGroup", 
               values_to="Registrations")

ggplot(long_age_groups, aes(x=Date, y=Registrations, colour=AgeGroup)) +
  geom_line() +
  scale_colour_manual(values=c(dmnred, dmnblue, ocre, dmngreen, "black", 
                               dmnorange, dmnpink))

# rolling mean number per day by age group
age_group_data <- age_group_data |>
  mutate(
    under_25_rolling=rolling_mean(Under_25, 7), 
    age_25_to_34_rolling=rolling_mean(age_25_to_34, 7), 
    age_35_to_44_rolling=rolling_mean(age_35_to_44, 7),
    age_45_to_54_rolling=rolling_mean(age_45_to_54, 7),
    age_55_to_64_rolling=rolling_mean(age_55_to_64, 7),
    age_65_to_74_rolling=rolling_mean(age_65_to_74, 7),
    over_75_rolling=rolling_mean(Over_75, 7)
  )

ggplot(age_group_data, aes(x=Date, y=under_25_rolling)) + 
  geom_line(color=dmnred) +
  geom_line(aes(y=age_25_to_34_rolling), color=dmnblue) +
  geom_line(aes(y=age_35_to_44_rolling), color=ocre) +
  geom_line(aes(y=age_45_to_54_rolling), color=dmngreen) + 
  geom_line(aes(y=age_55_to_64_rolling), color='black') + 
  geom_line(aes(y=age_65_to_74_rolling), color=dmnpink) +
  geom_line(aes(y=over_75_rolling), color=water) +
  theme_minimal() +
  ylab("Number of Voter Registrations")
