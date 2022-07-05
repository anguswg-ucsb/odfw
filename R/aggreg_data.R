rm(list = ls())

library(tidyverse)
library(sf)
library(terra)
library(mapview)
library(nhdplusTools)
library(dataRetrieval)

basepath       <- "D:/odfw/climate_change"
pt_path        <- "D:/odfw/shp/odfw_hatcheries.shp"

# *******************************************
# ---- Upstream COMIDs from hatchery pts ----
# *******************************************

# hatchery locations
hatch_pts <- sf::read_sf(pt_path) 

# Empty list
hcomid_lst <- list()

# Retrieve hatchery starting comids
for (i in 1:nrow(hatch_pts)) {
  
  hatch_name <- hatch_pts[i,]$Name
  
  logger::log_info("\n\nGetting COMID...\n{hatch_name}")
  
  hcomid_lst[[i]] <- nhdplusTools::get_nhdplus(AOI = hatch_pts[i,]) %>% 
                        dplyr::mutate(
                          hatchery = hatch_name
                        ) %>% 
                        dplyr::select(hatchery, comid, geometry)


}

# Starting comids for hatcheries 
hcomid <- dplyr::bind_rows(hcomid_lst)

# empty upstream flow lines list
ut_lst <- list()
# i <- 6
# Retrieve upstream flowlines 
for (i in 1:nrow(hcomid)) {
  
  com        <- hcomid[i,]$comid  
  hatch_name <- hcomid[i,]$hatchery  
  
  logger::log_info("\n\nGetting Upstream flowlines...\nHatchery: {hatch_name}\nCOMID: {com}")
  
  if (hatch_name == "Oak Springs Hatchery") {
    
    logger::log_info("\n\n{hatch_name} new COMID --> 23719075")
    
    ut_net <- nhdplusTools::navigate_network(
      start       = 23719075,
      mode        = "UT",
      distance_km = 15
    ) %>% 
      dplyr::mutate(
        hatchery = hatch_name
      ) %>% 
      dplyr::select(hatchery, comid, reachcode, ftype, fcode, streamleve, streamorde, streamcalc, hydroseq, geometry) %>% 
      dplyr::mutate(across(where(is.numeric), as.character))
    
    ut_net2 <- nhdplusTools::navigate_network(
      start       = 23722389,
      mode        = "UT",
      distance_km = 15
    ) %>% 
      dplyr::mutate(
        hatchery = hatch_name
      ) %>% 
      dplyr::select(hatchery, comid, reachcode, ftype, fcode, streamleve, streamorde, streamcalc, hydroseq, geometry) %>% 
      dplyr::mutate(across(where(is.numeric), as.character))
 
    ut_net <- dplyr::bind_rows(ut_net, ut_net2)
  
  } else if (hatch_name == "Rock Creek Hatchery") {
    
    logger::log_info("\n\nRock creek")
    
    ut_net <- nhdplusTools::navigate_network(
      start       = com,
      mode        = "UT",
      distance_km = 15
    ) %>% 
      dplyr::mutate(
        hatchery = hatch_name
      ) %>% 
      dplyr::select(hatchery, comid, reachcode, ftype, fcode, streamleve, streamorde, streamcalc, hydroseq, geometry) %>% 
      dplyr::mutate(across(where(is.numeric), as.character))
    
    ut_net2 <- nhdplusTools::navigate_network(
      start       = 23894140,
      mode        = "UT",
      distance_km = 15
    ) %>% 
      dplyr::mutate(
        hatchery = hatch_name
      ) %>% 
      dplyr::select(hatchery, comid, reachcode, ftype, fcode, streamleve, streamorde, streamcalc, hydroseq, geometry) %>% 
      dplyr::mutate(across(where(is.numeric), as.character))
    
    ut_net <- dplyr::bind_rows(ut_net, ut_net2)
    
  } else {
    logger::log_info("\n\n NOT Rock creek")
    ut_net <- nhdplusTools::navigate_network(
                start       = com,
                mode        = "UT",
                distance_km = 15
              ) %>% 
      dplyr::mutate(
        hatchery = hatch_name
        ) %>% 
      dplyr::select(hatchery, comid, reachcode, ftype, fcode, streamleve, streamorde, streamcalc, hydroseq, geometry) %>% 
      dplyr::mutate(across(where(is.numeric), as.character))
  
  }
  
  # mapview(ut_net) + hcomid[i, ]
  ut_lst[[i]] <- ut_net
  
}

ut_network <- dplyr::bind_rows(ut_lst)

# Save RDS
saveRDS(ut_network, here::here("data", "upstream_networks", "upstream_nhd_network.rds"))

# Save Shapefile
sf::write_sf(ut_network, here::here("data", "upstream_networks", "upstream_nhd_network.shp"))

# tmp <- dplyr::select(ut_network, hatchery, geometry)
# mapview(tmp, burst = T)

# *****************************************************************************************
# *****************************************************************************************

# *****************************
# ---- Historic Timeseries ----
# *****************************

# upstream tributary network
ut_network <- readRDS(here::here("data", "upstream_networks", "upstream_nhd_network.rds"))

# Path to historic data
historic_path  <- "D:/odfw/climate_change/historic/"

# Historic data file paths
historic_files <- list.files(
  list.files(historic_path, full.names = T), full.names = T
  )

# Hatchery and comid dataframe. drop geometry
hatch_comids <- 
  ut_network %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(hatchery, comid)

hist_lst <- list()

# i <- 1

# Iterate through historic files
for (i in 1:length(historic_files)) {
  
  logger::log_info("\n\nLoading historic data:\n{historic_files[i]} ")
  
  # historic data
  historic <- readr::read_csv(historic_files[i]) %>% 
    dplyr::mutate(
      COMIDX = sub(
        "^(.*)[.].*",
        "\\1", 
        gsub("X", "", COMIDX)
        )
      )
  # Unique hatchery COMIDs
  uhatch         <- unique(ut_network$comid)
  
  # Filter Timeseries to COMIDs of interest
  net_ts <- 
    historic %>% 
    dplyr::filter(COMIDX %in% uhatch) %>% 
    dplyr::left_join(
      hatch_comids, 
      by = c("COMIDX" = "comid")
    ) %>% 
    dplyr::mutate(
      timeframe = "historic"
    )
  
  logger::log_info("\n\nUnique COMID: {length(unique(net_ts$COMIDX))}\nHatcheries: {unique(net_ts$hatchery)}")
  
  # Add historic data to list
  hist_lst[[i]] <- net_ts  

}
unique(net_ts_hist$COMIDX)
unique(future$COMIDX)

is.Date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}
hist_lst2 <- lapply(hist_lst, function(y){
  y %>%
    dplyr::mutate(
    across(where(is.Date), as.character)
    )
})

historic_df <- dplyr::bind_rows(hist_lst2)

# tmp <- readRDS(here::here("data", "historic", "upstream_historic_timeseries.rds"))
# unique(tmp$hatchery)
# tmp2 <-    tmp %>% filter(hatchery == "Rock Creek Hatchery")

# Save RDS
saveRDS(historic_df, here::here("data", "historic", "upstream_historic_timeseries.rds"))
# saveRDS(net_ts, here::here("data", "historic", "upstream_historic_timeseries_oakspring.rds"))

# Save CSV
readr::write_csv(historic_df, here::here("data", "historic", "upstream_historic_timeseries.csv"))
# readr::write_csv(net_ts, here::here("data", "historic", "upstream_historic_timeseries.csv"))

# *****************************************************************************************
# *****************************************************************************************

# ***************************
# ---- Future Timeseries ----
# ***************************

# upstream tributary network
ut_network <- readRDS(here::here("data", "upstream_networks", "upstream_nhd_network.rds"))

# Path to future data
future_path    <- "D:/odfw/climate_change/future/"

# mapview(ut_network)
# Future data file paths
future_files   <- list.files(
  list.files(future_path, full.names = T), full.names = T
)

# Check if column is date
is.Date <- function(x) {
  inherits(x, c("Date", "POSIXt"))
}

# Hatchery and comid dataframe. drop geometry
hatch_comids <- 
  ut_network %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(hatchery, comid)

future_lst <- list()

# i <- 1

# Iterate through historic files
for (i in 1:length(future_files)) {
  
  logger::log_info("\n\nLoading future data:\n{future_files[i]} ")
  
  if(future_files[i] == "D:/odfw/climate_change/future/Lower Deschutes (1)/Lower Deschutes.csv") {
    
    logger::log_info("\n\n Lower Deschutes ...")
    
    # Future data
    future <- readr::read_csv(future_files[i]) %>% 
      dplyr::select(3:7) %>% 
      setNames(c("X1", "Month", "Date", "COMIDX", "m3/day")) %>% 
      dplyr::mutate(
        COMIDX = sub(
          "^(.*)[.].*",
          "\\1", 
          gsub("X", "", COMIDX)
        ),
        `HUC 8 Name` = "Lower Deschutes",
        HUC8         = future_files[i]
      ) %>% 
      dplyr::mutate(across(where(is.numeric), as.character))
    
  } else if(future_files[i] == "D:/odfw/climate_change/future/Coquille/Coquille.csv") {
    
        logger::log_info("\n\n Coquille ...")
    
        # Future data
        future <- readr::read_csv(future_files[i]) %>% 
          dplyr::select(3:7) %>% 
          setNames(c("X1", "Month", "Date", "COMIDX", "m3/day")) %>% 
          dplyr::mutate(
            COMIDX = sub(
              "^(.*)[.].*",
              "\\1", 
              gsub("X", "", COMIDX)
              ),
            `HUC 8 Name` = "Coquille",
            HUC8         = future_files[i]
            ) %>% 
          dplyr::mutate(across(where(is.numeric), as.character))
        
  } else if(future_files[i] == "D:/odfw/climate_change/future/Mckenzie/Mckenzie.csv") {
    
    logger::log_info("\n\n Mckenzie...")
    
    # Future data
    future <- readr::read_csv(future_files[i]) %>% 
      dplyr::select(-COMIDX) %>% 
      dplyr::relocate(X1, Month, Date, COMIDX = COMID, "m3/day", `HUC 8 Name`, HUC8) %>% 
      dplyr::mutate(across(where(is.numeric), as.character))
    
    } else if(future_files[i] == "D:/odfw/climate_change/future/Upper Rogue/Upper Rogue.csv") {
      
      logger::log_info("\n\n Upper Rogue...")
      
      # Future data
      future <- readr::read_csv(future_files[i]) %>% 
        dplyr::select(3:7) %>% 
        setNames(c("X1", "Month", "Date", "COMIDX", "m3/day")) %>% 
        dplyr::mutate(
          COMIDX = sub(
            "^(.*)[.].*",
            "\\1", 
            gsub("X", "", COMIDX)
            ),
          `HUC 8 Name` = "Upper Rogue",
          HUC8         = future_files[i]
          ) %>% 
        dplyr::mutate(across(where(is.numeric), as.character))
      
    } else if(future_files[i] == "D:/odfw/climate_change/future/North Umpqua/North Umpqua.csv") {
      
      logger::log_info("\n\n North Umpqua...")
      
      # Future data
      future <- readr::read_csv(future_files[i]) %>% 
        dplyr::select(-COMIDX) %>% 
        dplyr::relocate(X1, Month, Date, COMIDX = COMID, "m3/day", `HUC 8 Name`, HUC8) %>% 
        dplyr::mutate(across(where(is.numeric), as.character))

    } else if(future_files[i] == "D:/odfw/climate_change/future/Alsea_Future/Alsea_Future.csv") {
      
      logger::log_info("\n\nAlsea...")
      
      # Future data
      future <- readr::read_csv(future_files[i]) %>% 
        dplyr::mutate(
          COMIDX = sub(
            "^(.*)[.].*",
            "\\1", 
            gsub("X", "", COMIDX)
          )
        ) %>% 
        dplyr::mutate(across(where(is.numeric), as.character))
      
    }
  #   } else {
  #   # i <- 1
  #   # Future data
  #   # future <- read.csv(future_files[i])
  #   future <- readr::read_csv(future_files[i]) %>% 
  #     dplyr::mutate(
  #       COMIDX = sub(
  #         "^(.*)[.].*",
  #         "\\1", 
  #         gsub("X", "", COMIDX)
  #       )
  #     )
  #   
  # }

  # Unique hatchery COMIDs
  uhatch         <- unique(ut_network$comid)
  # ucomid <- unique(future$COMIDX)
 # ucomid <- sub(
 #    "^(.*)[.].*",
 #    "\\1",
 #    gsub("X", "",  unique(future$COMIDX))
 #  )
  # Filter Timeseries to COMIDs of interest
  net_ts <- 
    future %>% 
    dplyr::filter(COMIDX %in% uhatch) %>% 
    dplyr::left_join(
      hatch_comids, 
      by = c("COMIDX" = "comid")
    ) %>% 
    dplyr::mutate(
      timeframe = "future"
    ) %>% 
    dplyr::mutate(across(where(is.Date), as.character)) %>% 
    dplyr::mutate(across(where(is.numeric), as.character))
  
  logger::log_info("\n\nUnique COMID: {length(unique(net_ts$COMIDX))}\nHatcheries: {unique(net_ts$hatchery)}")
  
  # Add historic data to list
  future_lst[[i]] <- net_ts  
  
}
# is.Date <- function(x) {
#   inherits(x, c("Date", "POSIXt"))
# }
# future_lst2 <- lapply(future_lst, function(y){
#   y %>% dplyr::mutate( cross(where(is.numeric), as.character))
# })
# future_df <- dplyr::bind_rows(future_lst2)

future_df <- dplyr::bind_rows(future_lst)

tmp <- future_df %>% head()
tmp <- future_df %>% tail()

length(unique(future_df$COMIDX))
max(future_df$Date)
min(future_df$Date)

# Save RDS
saveRDS(future_df, here::here("data", "future", "upstream_future_timeseries.rds"))

# Save CSV
readr::write_csv(future_df, here::here("data", "future", "upstream_future_timeseries.csv"))


# *****************************************************************************************
# *****************************************************************************************
# ******************
# ---- New Data ----
# ******************

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

stream_flows <-
  stream_flows %>% 
  dplyr::left_join(
    hatch_comids, 
    by = "comid"
  ) %>% 
  dplyr::relocate(date, year, month, day, comid, hatchery, historic_flow, future_flow, units)

# length(unique(stream_flows$date))

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
  )

# Save RDS
# saveRDS(daily_flows, here::here("data", "flow", "hatchery_daily_flows.rds"))
# saveRDS(monthly_flows, here::here("data", "flow", "hatchery_monthly_flows.rds"))

# Save CSV
# readr::write_csv(daily_flows, here::here("data", "flow", "hatchery_daily_flows.csv"))
# readr::write_csv(monthly_flows, here::here("data", "flow", "hatchery_monthly_flows.csv"))

# *****************************************************************************************
# *****************************************************************************************
unique(ut_network$comid) %in% ucomid


future_path    <- "D:/odfw/climate_change/future/Alsea_Future.csv"
# paste0(list.files(list.files(basepath, full.names = T)[2]))

# Alsea future climate data
alsea_future   <- readr::read_csv(future_path)

# Unique COMIDs
# ucomid         <- gsub("X", "", unique(alsea_future$COMIDX))
ucomid <- sub(
  "^(.*)[.].*", 
  "\\1", 
  gsub("X", "", unique(alsea_future$COMIDX))
  )

# pull comids
cc_comids <- nhdplusTools::get_nhdplus(comid = ucomid)
mapview(cc_comids)
sub("^(.*)[.].*", 
    "\\1", 
    gsub("X", "", unique(alsea_future$COMIDX))
    )
mapview(cc_comids)














