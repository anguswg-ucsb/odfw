# Angus Watters
# Calculate average monthly change between historic & future data
# Additionally, if you could post-process the streamflow change factors that would be great.
# We are interested in comparing the historical flow over a 30-year period (1977-2006) with the mid-century future flow over a 30-year period (2030-2054).
# Let's calculate daily average and monthly average streamflow change factors for each period (historical and future),
# then subtract future from historical, to get the flow change factor (% reduction).

# Daily average streamflow   (historic & future)
# Monthly Average streamflow (historic & future)

# % Reduction
# Flow Change Factor = Historic - Future

rm(list = ls())

library(tidyverse)
library(sf)
library(terra)
library(mapview)
library(nhdplusTools)
library(dataRetrieval)

basepath       <- "D:/odfw/climate_change"
pt_path        <- "D:/odfw/shp/odfw_hatcheries.shp"

# upstream tributary network
ut_network <- readRDS(here::here("data", "upstream_networks", "upstream_nhd_network.rds"))

# Historic data
historic <- readRDS(here::here("data", "historic", "upstream_historic_timeseries.rds"))

# Future Data
future   <- readRDS(here::here("data", "future", "upstream_future_timeseries.rds"))

# Select relevent columns hsitoric
historic <-
  historic %>% 
  dplyr::rename(historic_flow = `m3/day`) %>% 
  dplyr::select(-X1, -`HUC8 Name`, -HUC8, -timeframe, -Month) %>% 
  dplyr::relocate(hatchery, COMIDX, Date, historic_flow)

# Select relevent columns future
future <- 
  future %>% 
  dplyr::rename(future_flow = `m3/day`) %>% 
  dplyr::select(-X1, -`HUC 8 Name`, -HUC8, -timeframe, -Month) %>% 
  dplyr::relocate(hatchery, COMIDX, Date, future_flow) 

# Number of unique COMIDs in data
length(unique(historic$COMIDX))
length(unique(future$COMIDX))

# Check that all COMIDs are in both datasets
unique(historic$COMIDX) %in% unique(future$COMIDX)
unique(future$COMIDX) %in% unique(historic$COMIDX)


stream_flows <-
  historic %>%
  dplyr::left_join(
    future,
    by = c("hatchery", "Date", "COMIDX")
    ) %>% 
  dplyr::mutate(
    date = dplyr::case_when(
      grepl("/", Date) ~ format(strptime(as.character(Date), "%m/%d/%Y"), "%Y-%m-%d"),
      grepl("-", Date) ~ Date
      )
    ) %>% 
  dplyr::mutate(
    month         = lubridate::month(date),
    year          = lubridate::year(date),
    historic_flow = as.numeric(historic_flow),
    future_flow   = as.numeric(future_flow)
  )
stream_flows2 <- 
  stream_flows2 %>% 
  dplyr::mutate(
    month         = lubridate::month(date2),
    year          = lubridate::year(date2),
    historic_flow = as.numeric(historic_flow),
    future_flow   = as.numeric(future_flow)
  )
# rm(stream_flows2, stream_flows)
datedf <- tibble::tibble(
  udate = unique(stream_flows$Date)
  )
# grepl("/", udate) ~ 
grepl("/", datedf$udate[1])
grepl("/", datedf$udate[66000])
grepl("-", datedf$udate[1])
grepl("-", datedf$udate[66000])
# datedf <- data.frame(
#   udate = unique(stream_flows$Date)
datedf2 <- 
  datedf %>% 
  # dplyr::mutate(
  #   udate2 = as.Date(udate),
  #   month = lubridate::month(udate2)
  #   # month = substr()
  # )
  dplyr::mutate(
    date = dplyr::case_when(
      grepl("/", udate) ~ format(strptime(as.character(udate), "%m/%d/%Y"), "%Y-%m-%d"),
      grepl("-", udate) ~ udate
    )
  )
format(strptime(as.character(udate), "%m/%d/%Y"), "%Y-%m-%d")
datedf3 <- 
  datedf2 %>% 
  dplyr::select(udate, date_form1) %>%
  dplyr::filter(date_form1 != "no_date") %>%
  dplyr::mutate(
    newdate = format(strptime(as.character(udate), "%m/%d/%Y"), "%Y-%m-%d")
    # udate2 = as.Date(udate),
    # month  = lubridate::month(udate2)
    # month = substr()
  )
  # dplyr::filter(date_form1 != "no_date")
dplyr::filter(datedf2, date_form2 != "no_date")
# %>% 
#   dplyr::mutate(
#     month         = lubridate::month(Date),
#     year          = lubridate::year(Date),
#     historic_flow = as.numeric(historic_flow),
#     future_flow   = as.numeric(future_flow)
#   )


monthly_flows <- 
  stream_flows %>% 
  dplyr::group_by(hatchery, month, year) %>% 
  dplyr::summarise(
    historic_flow = mean(historic_flow, na.rm = T),
    future_flow   = mean(future_flow, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    date = as.Date(paste0(year, "-", month, "-01"))
  ) %>% 
  dplyr::select(hatchery, date, historic_flow, future_flow) %>% 
  # dplyr::select(hatchery, COMIDX, date, historic_flow, future_flow) %>% 
  dplyr::mutate(
    flow_change_factor = historic_flow - future_flow,
    pct_flow_change    = round(flow_change_factor/historic_flow*100, 2)
    )



# Number of unique COMIDs in data
length(unique(htmp$COMIDX))
length(unique(ftmp$COMIDX))














