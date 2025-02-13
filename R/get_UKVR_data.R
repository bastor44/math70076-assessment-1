# Load libraries
library(rvest)
library(xml2)
library(lubridate)
library(tidyverse)

##### Function to get desired data #############################################
#' Title
#' @param base_url base URL for the UK voter registration dashboard - current 
#'                 base URL is the default, but parameter included to allow for 
#'                 changes
#' @param data_type part of the voter registration dashboard URL that specifies 
#'                  the section of interest 
#' @param start_date earliest date requested (earliest possible is 2014), in 
#'                   "%d-%m-%Y" format
#' @param end_date latest date requested (latest possible is yesterday), in 
#'                 "%d-%m-%Y" format
#'
#' @return a tibble with the data extracted from the UK voter registration
#'         performance dashboard (*may need to be reformatted before performing
#'         additional analyses)
#' @export
#'
#' @examples
#' get_2024_data(data_type="/applications_breakdown")
#' get_2024_data(data_type="/applications_by_age_group")
#' 
get_UKVR_data <- function(base_url="https://www.registertovote.service.gov.uk/performance",
                          data_type, 
                          start_date="01-01-2024", 
                          end_date="31-12-2024") {
  
  # initialise empty tibble to store scraped data  
  scraped_data <- tibble()
  
  
  # define start and end dates for all requests 
  # will only allow seeing 100 days at a time so need make several requests
  start_date <- as.Date(start_date, format="%d-%m-%Y")
  end_date <- as.Date(end_date, format="%d-%m-%Y")
  date_seq <- seq(from=start_date, to=end_date, by="days")
  
  n_splits <- ceiling(length(date_seq)/100)
  start_index <- seq(1, (n_splits*100), 100)
  end_index <- c(seq(100, floor(length(date_seq)/100)*100, 100), length(date_seq))
  
  split_dates <- data.frame(starts=date_seq[start_index], 
                            ends=date_seq[end_index]) 
  
  
  # extract all data from specified date window
  for (i in 1:n_splits) {
    from <- split_dates[i, "starts"]
    to <- split_dates[i, "ends"]
    
    # from Day
    fromDay <- day(from)
    if (fromDay < 10) {
      fromDay <- paste0("0", as.character(fromDay))
    } else {
      fromDay <- as.character(fromDay)
    }
    
    # from Month 
    fromMonth <- month(from)
    if (fromMonth < 10) {
      fromMonth <- paste0("0", as.character(fromMonth))
    } else {
      fromMonth <- as.character(fromMonth)
    }
    
    # from Year
    fromYear <- year(from)
    
    
    # To date 
    toDay <- day(to)
    if (toDay < 10) {
      toDay <- paste0("0", as.character(toDay))
    } else {
      toDay <- as.character(toDay)
    }
    
    # to Month 
    toMonth <- month(to)
    if (toMonth < 10) {
      toMonth <- paste0("0", as.character(toMonth))
    } else {
      toMonth <- as.character(toMonth)
    }
    
    # to Year
    toYear <- year(to)
    
    
    
    # specify which data is wanted
    data_type <- data_type
    
    url <- sprintf("%s%s/with_dates?fromDay=%s&fromMonth=%s&fromYear=%s&toDay=%s&toMonth=%s&toYear=%s", 
                   base_url, data_type, fromDay, fromMonth, fromYear, toDay, toMonth, toYear)
    
    
    # format as a tibble 
    new_data <- url |>
      read_html() |>
      html_element("table") |>
      html_table() |>
      tibble()
    
    # append next page to already obtained data 
    scraped_data <- rbind(scraped_data, new_data)
  }
  
  
  
  return(scraped_data)
}
