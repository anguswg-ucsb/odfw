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
# Some of the hatcheries have more than one COMID associated with it, so can you add the COMIDs in a column?
#   Also, can you calculate the average monthly change so that we have 1 set of 12 values for each comid? For these new average month values, keep the same output values you have already calculated (flow_change_factor & pct_flow_change).

rm(list = ls())

library(tidyverse)
library(sf)
library(terra)
library(mapview)
library(nhdplusTools)
library(dataRetrieval)

basepath       <- "D:/odfw/climate_change"
pt_path        <- "D:/odfw/shp/odfw_hatcheries.shp"

# **********************************
# ---- Hatchery flow processing ----
# **********************************

# Path to data
hist_path        <- "D:/odfw/climate_change/Oregon_historical_flow_m3day_1915-2006.csv"
future_path      <- "D:/odfw/climate_change/Oregon_2040_flow_m3day_1915-2006.csv"

# upstream tributary network
ut_network <- readRDS(here::here("data", "upstream_networks", "upstream_nhd_network.rds"))

# Hatchery and comid dataframe. drop geometry
hatch_comids <- 
  ut_network %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(hatchery, comid)

hist_path        <- "D:/odfw/climate_change/Oregon_historical_flow_m3day_1915-2006.csv"
future_path      <- "D:/odfw/climate_change/Oregon_2040_flow_m3day_1915-2006.csv"

historic <- data.table::fread(hist_path) %>%
  tibble::tibble() %>% 
  setNames(c(gsub("x", "", names(.)))) %>% 
  pivot_longer(
    cols      = c(`23719075`:`23923474`),
    names_to  = "comid",
    values_to = "historic_flow"
  ) %>% 
  janitor::clean_names() %>% 
  dplyr::relocate(date, year, month, day, comid, historic_flow, units)

future <- data.table::fread(future_path) %>%
  tibble::tibble() %>% 
  setNames(c(gsub("x", "", names(.)))) %>% 
  pivot_longer(
    cols      = c(`23719075`:`23923474`),
    names_to  = "comid",
    values_to = "future_flow"
  ) %>% 
  janitor::clean_names() %>% 
  dplyr::relocate(date, year, month, day, comid, future_flow, units)

# Join historic and future
stream_flows <- 
  historic %>% 
  dplyr::left_join(
    dplyr::select(future, date, comid, future_flow),
    by = c("date", "comid")
  ) %>% 
  dplyr::relocate(date, year, month, day, comid, historic_flow, future_flow, units)

# unique(ut_network$comid) %in% unique(stream_flows$comid)
# unique(stream_flows$comid) %in% unique(ut_network$comid)

# Hatchery names & comids
hatch_comids <- 
  ut_network %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(comid %in% unique(stream_flows$comid)) %>% 
  dplyr::select(comid, hatchery)

# Join future and historic data 
stream_flows <-
  stream_flows %>% 
  dplyr::left_join(
    hatch_comids, 
    by = "comid"
  ) %>% 
  dplyr::relocate(date, year, month, day, comid, hatchery, historic_flow, future_flow, units)

# length(unique(stream_flows$date))

# list column of comids for each hatchery
comids_lst <-
  hatch_comids %>% 
  dplyr::group_by(hatchery) %>% 
  dplyr::summarize(comids = list(comid)) %>% 
  dplyr::ungroup()

  # tidyr::nest()
# daily flows
daily_flows <- 
  stream_flows %>% 
  dplyr::group_by(hatchery, date) %>% 
  dplyr::summarise(
    historic_flow = mean(historic_flow, na.rm = T),
    future_flow   = mean(future_flow, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    flow_change_factor = historic_flow - future_flow,
    pct_flow_change    = round(flow_change_factor/historic_flow*100, 2)
  )  %>% 
  dplyr::left_join(
    comids_lst, 
    by = "hatchery"
  )
# monthly flows
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
  ) %>% 
  dplyr::left_join(
    comids_lst, 
    by = "hatchery"
  )

# Save RDS
saveRDS(daily_flows, here::here("data", "flow", "hatchery_daily_flows.rds"))
saveRDS(monthly_flows, here::here("data", "flow", "hatchery_monthly_flows.rds"))

# Save CSV
readr::write_csv(daily_flows, here::here("data", "flow", "hatchery_daily_flows.csv"))
readr::write_csv(monthly_flows, here::here("data", "flow", "hatchery_monthly_flows.csv"))

# ********************************
# ---- Average Monthly Change ----
# ********************************

#   Also, can you calculate the average monthly change so that we have 1 set of 12 values for each comid? 
# For these new average month values, keep the same output values you have already calculated (flow_change_factor & pct_flow_change).
# average monthly flows
avg_month_flows <-
  stream_flows %>% 
  dplyr::group_by(comid, month) %>% 
  dplyr::summarise(
    historic_flow = mean(historic_flow, na.rm = T),
    future_flow   = mean(future_flow, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  # dplyr::select(hatchery, date, historic_flow, future_flow) %>% 
  # dplyr::select(hatchery, COMIDX, date, historic_flow, future_flow) %>% 
  dplyr::mutate(
    flow_change_factor = historic_flow - future_flow,
    pct_flow_change    = round(flow_change_factor/historic_flow*100, 2)
  ) %>% 
  dplyr::left_join(
    hatch_comids,
    by = "comid"
  ) %>% 
  dplyr::relocate(hatchery, comid, month)

# Save RDS
saveRDS(avg_month_flows, here::here("data", "flow", "avg_monthly_flows.rds"))

# Save CSV
readr::write_csv(avg_month_flows, here::here("data", "flow", "avg_monthly_flows.csv"))

# *****************************************************************************************
# ********************************
# ---- Avg Monthly flow plots ----
# ********************************

# Avg Monthly flow
avg_month_flows <- readRDS(here::here("data", "flow", "avg_monthly_flows.rds"))

# upstream tributary network
ut_network <- readRDS(here::here("data", "upstream_networks", "upstream_nhd_network.rds"))

# tmp <- ut_network %>% 
#   dplyr::filter(comid == 23923474)
# tmp2 <- ut_network %>% 
#   dplyr::filter(comid == 23923430)
# mapview::mapview(tmp, color = "red") + mapview::mapview(tmp2, color = "green") + ut_network
# Unique COMIDs
ucoms <- unique(avg_month_flows$comid)

stream <- data.frame(
  comid       = c(ucoms),
  stream_name = c("Deschutes River",        "Oak Springs Creek", "McKenzie River", 
                  "North Fork Alsea River", "North Umpqua River", "Rock Creek", 
                  "Ferry Creek (upstream)", "Ferry Creek (downstream)", "Geiger Creek",
                  "Rogue River", "Rogue River (above Lost Creek Lake)")
  )

avg_flows <- 
  avg_month_flows %>%
  dplyr::left_join(
    stream, 
    by = "comid"
  ) %>% 
  dplyr::group_by(comid) %>% 
  dplyr::mutate(
    # date       = as.Date(paste0("2020-", month, "-01")),
    date       = strptime(paste0("2020-", month, "-01"), format="%Y-%m-%d"),
    month_name = factor(month.abb, levels = c(month.abb), ordered = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(comid) %>% 
  dplyr::rename("Historical Flow" = historic_flow, "Future Projected Flow (2045)" = future_flow) %>%
  tidyr::pivot_longer(cols = c( "Future Projected Flow (2045)", "Historical Flow")) %>% 
  dplyr::mutate(
    name = factor(name, levels = c("Historical Flow", "Future Projected Flow (2045)"))
    ) %>% 
  dplyr::ungroup()

# Unique COMIDs
ucoms <- unique(avg_flows$comid)

# rm(i, flows, avg_flow_plot)
# i <- 2
# library(scales)
for (i in 1:length(ucoms)) {
  
  flows <- 
    avg_flows %>% 
    dplyr::filter(comid == ucoms[i]) %>%
    dplyr::mutate(value = value/1000)

  max_lim <- ceiling(max(flows$value))
  
  logger::log_info("\n\n{i} / {length(ucoms)}\nCOMID: {ucoms[i]}\nHatchery: {flows$hatchery[1]}\nStream: {flows$stream_name[1]}")
  
  avg_flow_plot <-
    ggplot() +
    geom_line(data = flows, aes(x = date, y = value, color = name), size = 2) + 
    scale_x_datetime(labels = scales::date_format("%b"), breaks = "1 month") +
    # scale_y_continuous(labels = scales::comma) +
    labs(
      title = paste0(flows$hatchery[1], " - ", flows$stream_name[1]),
      color = "",
      x = "", 
      y = "Flow (Thousand m3)"
    ) +
    # scale_y_continuous(labels = scales::comma) +
    scale_y_continuous(limits = c(0, max_lim), labels = scales::comma) +
    # ggthemes::theme_gdocs() +
    theme_bw() +
    theme(
      plot.title = element_text(size = 18, hjust = 0.5),
      legend.position    = "bottom",
      legend.text = element_text(size = 12),
      panel.grid.minor.x = element_blank(),
      axis.text          = element_text(size = 12),
      axis.title.y = element_text(size = 12,
                                  margin = margin(t = 0, r = 15, b = 0, l = 0))
    ) 
  
  ggsave(
    paste0("img/avg_month_flows_", 
           gsub(" ", "_", tolower(flows$hatchery[1])), 
           "_",
           ucoms[i], 
           ".png"),
    avg_flow_plot,
    height = 8,
    width  = 12
    )
}
avg_flows
avg_flows$month_name

# avg_flows %>% 
#   dplyr::group_by(comid) %>% 
#   tidyr::pivot_longer(cols = c(historic_flow, future_flow)) %>% 
#   dplyr::ungroup()

# *****************************************************************************************
# *****************************************************************************************

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














