# code/functions.R
# This script houses functions and useful bits used by other scripts


# Setup -------------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
library(tidyverse)
library(tidync)
library(padr)
library(XML)
# remotes::install_github("robwschlegel/heatwaveR", force = T) # Development version
library(heatwaveR); packageVersion("heatwaveR")
library(doParallel); registerDoParallel(cores = 50)


# Meta-data ---------------------------------------------------------------

## Potential colour palettes
# w3schools.com/colors/colors_groups.asp
# sciviscolor.org/home/environmental-palettes/

# Consider ROYGBIV
# Because MHWs use ROY it could be good to use GBIV for MCSs
# Maybe don't worry about perceptual symmetry
# Just pick the best colours from a colour wheel

# Load an XML file containing weird rgb vlaues and convert to hex
rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)

# BlueSpectrum colour palette from sciviscolor.org/home/environmental-palettes/
BlueSpectrum <- t(data.frame(xmlToList(xmlParse("metadata/BlueSpectrum.xml"))$ColorMap[1:27], stringsAsFactors = F)) %>% 
  data.frame(., stringsAsFactors = F) %>% 
  remove_rownames(.) %>% 
  select(r, g, b) %>% 
  mutate(r = round(as.numeric(r)*255), g = round(as.numeric(g)*255), b = round(as.numeric(b)*255),
         hex = rgb2hex(r, g, b))
# write_csv(BlueSpectrum, "metadata/BlueSpecturm.csv")

# BlueWater colour palette from sciviscolor.org/home/environmental-palettes/
BlueWater <- t(data.frame(xmlToList(xmlParse("metadata/BlueWater.xml"))$ColorMap[1:12], stringsAsFactors = F)) %>% 
  data.frame(., stringsAsFactors = F) %>% 
  remove_rownames(.) %>% 
  select(r, g, b) %>% 
  mutate(r = round(as.numeric(r)*255), g = round(as.numeric(g)*255), b = round(as.numeric(b)*255),
         hex = rgb2hex(r, g, b))
# write_csv(BlueWater, "metadata/BlueWater.csv")

# This is negotiable...
# MCS_palette <- c(BlueSpectrum$hex[6], BlueSpectrum$hex[13], BlueSpectrum$hex[20], BlueSpectrum$hex[27])
MCS_palette <- c(BlueWater$hex[11], BlueWater$hex[9], BlueWater$hex[6], BlueWater$hex[2])

# Set line colours
lineCol <- c(
  "Temperature" = "black",
  "Climatology" = "grey40",
  "Threshold" = "darkorchid",
  "2x Threshold" = "darkorchid",
  "3x Threshold" = "darkorchid",
  "4x Threshold" = "darkorchid",
  "< -1.5C" = "darkorchid"
)

# Set category fill colours
fillCol <- c(
  "Moderate" = MCS_palette[1],
  "Strong" = MCS_palette[2],
  "Severe" = MCS_palette[3],
  "Extreme" = MCS_palette[4],
  "Ice" = "thistle1"
)

# The MCS colour palette
MCS_colours <- c(
  "I Moderate" = MCS_palette[1],
  "II Strong" = MCS_palette[2],
  "III Severe" = MCS_palette[3],
  "IV Extreme" = MCS_palette[4],
  "V Ice" = "thistle1"
)

# The MHW colour palette
MHW_colours <- c(
  "Moderate" = "#ffc866",
  "Strong" = "#ff6900",
  "Severe" = "#9e0000",
  "Extreme" = "#2d0000"
)

# OISST coords
# The lon coords for the OISST data
lon_OISST <- seq(0.125, 359.875, by = 0.25)
lon_OISST <- ifelse(lon_OISST > 180, lon_OISST-360, lon_OISST)
lat_OISST <- seq(-89.875, 89.875, by = 0.25)
lon_lat_OISST <- base::expand.grid(lon_OISST, lat_OISST) %>% 
  dplyr::rename(lon = Var1, lat = Var2) %>% 
  arrange(lon, lat) %>% 
  data.frame()

# File locations
OISST_files <- dir("../data/OISST", pattern = "avhrr-only", full.names = T)
MCS_files <- dir("../data/MCS", full.names = T)
MCS_lon_files <- dir("../data/cat_lon/MCS", full.names = T)
MCS_cat_files <- dir("../data/cat_clim/MCS", full.names = T, recursive = T)
MCS_event_files <- dir("../data/event", full.names = T, pattern = "MCS")
MCS_count_trend_files <- dir("annual_summary_MCS", pattern = "count_trend", full.names = T)
seas_thresh_files <- dir("../data/thresh", pattern = "MHW.seas.thresh.", full.names = T)

# Metadata
load("metadata/OISST_ocean_coords.Rdata")

# The base map
load("metadata/map_base.Rdata")

# Disable scientific notation
options(scipen = 9999)


# Prep functions ----------------------------------------------------------

# Tester...
# load("../data/MCS.calc.0001.RData")

# Pull out climatologies
MCS_clim <- function(df){
  clim <- df %>% 
    unnest(event) %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest(event)# %>% 
  # select(-(threshCriterion:event))
}
# test <- MCS_clim(MCS_res)

# Pull out events
MCS_event <- function(df){
  event <- df %>% 
    unnest(event) %>% 
    filter(row_number() %% 2 == 0) %>% 
    unnest(event)
}
# test <- MCS_event(MCS_res)

# Pull out category climatologies
MCS_cat_clim <- function(df, long = FALSE){
  cat_clim <- df %>% 
    unnest(cat) %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest(cat)
  if(long){
    cat_clim_long <- cat_clim %>% 
      group_by(lon, lat) %>%
      nest() %>%
      mutate(long = map(data, pad, interval = "day", 
                        start_val = as.Date("1982-01-01"))) %>% 
      dplyr::select(-data) %>%
      unnest()
  } else {
    return(cat_clim)
  }
}
# test <- MCS_cat_clim(MCS_res)
# test <- MCS_cat_clim(MCS_res, long = T)

# Pull out event category summaries
MCS_cat_event <- function(df){
  suppressWarnings(
    cat_event <- df %>% 
      unnest(cat) %>% 
      filter(row_number() %% 2 == 0) %>% 
      unnest(cat)
  )
}
# test <- MCS_cat_event(MCS_res)


# Functions ---------------------------------------------------------------

# Subset event metric files
load_MCS_event_sub <- function(file_name, date_range,
                               lon_range = NA, lat_range){
  res_event <- readRDS(file_name) %>% 
    dplyr::select(lon, lat, event) %>% 
    unnest(event) %>% 
    filter(row_number() %% 2 == 0) %>% 
    unnest(event) %>% 
    filter(date_start >= date_range[1], date_start <= date_range[2],
           lat >= lat_range[1], lat <= lat_range[2])
  res_cat <- readRDS(file_name) %>% 
    dplyr::select(lon, lat, cat) %>% 
    unnest(cat) %>% 
    filter(row_number() %% 2 == 0) %>% 
    unnest(cat) %>% 
    filter(peak_date >= date_range[1], peak_date <= date_range[2],
           lat >= lat_range[1], lat <= lat_range[2])
  res <- left_join(res_event, res_cat,
                   by = c("lon", "lat", "duration", "event_no",
                          "date_peak" = "peak_date", "intensity_max" = "i_max"))
  gc()
  return(res)
}

# Subset climatology files
load_MCS_clim_sub <- function(file_name, date_range,
                              lon_range = NA, lat_range){
  res_clim <- readRDS(file_name) %>% 
    dplyr::select(lon, lat, event) %>% 
    unnest(event) %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest(event) %>% 
    filter(t >= date_range[1], t <= date_range[2],
           lat >= lat_range[1], lat <= lat_range[2])
  res_cat <- readRDS(file_name) %>% 
    dplyr::select(lon, lat, cat) %>% 
    unnest(cat) %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest(cat) %>% 
    filter(t >= date_range[1], t <= date_range[2],
           lat >= lat_range[1], lat <= lat_range[2])
  res <- left_join(res_clim, res_cat,
                   by = c("lon", "lat", "t", "event_no"))
  gc()
  return(res)
}

# Function for loading all data streams
load_MCS_ALL <- function(bbox, date_range = c("1982-01-01", "2020-12-31")){
  # Load event data
  event_data <- plyr::ldply(MCS_lon_files[which(lon_OISST >= bbox[3] & lon_OISST <= bbox[4])], 
                            .fun = load_MCS_event_sub, .parallel = T, 
                            # date_range = c("1982-01-01", "2020-12-31"),
                            date_range = date_range,
                            lat_range = c(bbox[1], bbox[2]))
  
  # Load clim data
  clim_data <- plyr::ldply(MCS_lon_files[which(lon_OISST >= bbox[3] & lon_OISST <= bbox[4])], 
                           .fun = load_MCS_clim_sub, .parallel = T, 
                           # date_range = c("1982-01-01", "2020-12-31"),
                           date_range = date_range,
                           lat_range = c(bbox[1], bbox[2]))
  
  # Combine into list and exit
  list_data <- list(event_data = event_data,
                    clim_data = clim_data)
  gc()
  return(list_data)
}

extract_MCS_grid_year <- function(dat, year_choice, spread = 1){
  # Subset clim data
  dat_clim_sub <- dat$clim_data %>% 
    filter(lubridate::year(t) >= year_choice-spread,
           lubridate::year(t) <= year_choice+spread)
  
  # Subset event+cat data
  dat_event_cat_sub <- dat$event_data %>% 
    filter(lubridate::year(date_peak) >= year_choice-spread,
           lubridate::year(date_peak) <= year_choice+spread)
  
  # Return
  dat_res <- list(clim_data = dat_clim_sub,
                  event_data = dat_event_cat_sub)
}

# Function that loads and merges sst/seas/thresh for a given lon_step
# lon_step <- lon_OISST[2]
# date_range <- as.Date("2018-01-01")
# date_range <- c(as.Date("2016-02-01"), as.Date("2017-04-01"))
sst_seas_thresh_merge <- function(lon_step, date_range){
  
  # Establish lon row number
  lon_row <- which(lon_OISST == lon_step)
  
  # Establish date range
  if(length(date_range) == 1) date_range <- c(date_range, Sys.Date())
  
  # OISST data
  tidync_OISST <- tidync(OISST_files[lon_row]) %>% 
    hyper_filter(time = between(time, as.integer(date_range[1]), as.integer(date_range[2]))) %>% 
    hyper_tibble() %>% 
    mutate(time = as.Date(time, origin = "1970-01-01"),
           year = year(time)) %>% 
    dplyr::rename(t = time, temp = sst) %>%
    mutate(doy = yday(t)) %>% 
    group_by(year) %>% 
    mutate(doy = ifelse(!leap_year(year),
                        ifelse(doy > 59, doy+1, doy), doy)) %>% 
    ungroup() %>%
    select(lon, lat, t, doy, temp)
  
  # Merge to seas/thresh and exit
  sst_seas_thresh <- tidync_OISST %>%
    left_join(hyper_tibble(tidync(seas_thresh_files[lon_row])),
              by = c("lon", "lat", "doy" = "time")) %>%
    mutate(anom = round(temp - seas, 2))
  return(sst_seas_thresh)
}

