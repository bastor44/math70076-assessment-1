##### Set Up ###################################################################
# Load libraries 
library(rvest)
library(xml2)
library(lubridate)
library(tidyverse)

# source function to get data 
path <- "math70076-assessment-1"
source(paste0(path, "/R/get_UKVR_data.R"))


##### Daily Numbers ############################################################
daily_data_2024 <- get_UKVR_data(data_type="/applications_breakdown")


# format data for use 
daily_data_2024 <- daily_data_2024 |>
  rename("Paper"="Paper forms") |>
  mutate_at(c("Online", "Paper"), function(x) gsub(",", "", x)) |>
  mutate(Date = as.Date(Date, format = "%d %b %Y")) |>
  arrange(Date) |>
  mutate(Online=as.numeric(Online), Paper=as.numeric(Paper)) |>
  mutate(total=Online + Paper)


# save as RDS file
saveRDS(daily_data_2024, file="data/daily_data_2024.rds")





##### Age Groups ###############################################################
age_groups_2024 <- get_UKVR_data(data_type="/applications_by_age_group")

# format data for use 
age_groups_2024 <- age_groups_2024 |>
  rename_all(function(x) gsub(" ", "_", x)) |>
  rename_with(~ paste0("age_", .), matches("to_")) |>
  mutate_at(c("Under_25", "age_25_to_34", "age_35_to_44", "age_45_to_54",
              "age_55_to_64", "age_65_to_74", "Over_75"), 
            function(x) gsub(",", "", x)) |>
  mutate(Date = as.Date(Date, format = "%d %b %Y")) |>
  arrange(Date) |>
  mutate_at(c("Under_25", "age_25_to_34", "age_35_to_44", "age_45_to_54",
              "age_55_to_64", "age_65_to_74", "Over_75"), 
            function(x) as.numeric(x))


# save as RDS file
saveRDS(age_groups_2024, file="data/age_groups_2024.rds")
