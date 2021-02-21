# code/workflow.R
# This script houses the code used for calculating global historic MCSs
# NB: This script is designed to run on a server that contains OISST data in a "../data/" folder
# NB: This script is also designed to output the larger results to that "../data/" folder
# NB: Smaller summary files are saved locally to "data/" and pushed to GitHub
# 1: Setup
# 2: Full calculations
# 3: Daily categories
# 4: Annual summaries
# 5: Total summaries 
# 6: Trends 
# 7: MHWs minus MCSs
# 8: SSTa skewness and kurtosis
# 9: Spatial correlations
# 10: Check on MCS hole in Antarctica


# 1: Setup ----------------------------------------------------------------

# Libraries
.libPaths(c("~/R-packages", .libPaths()))
source("code/functions.R")
library(lubridate)
library(dtplyr)
library(tidync)
library(broom)
library(e1071)
library(ggridges)
# remotes::install_github("robwschlegel/heatwaveR", force = T) # Development version
library(heatwaveR); packageVersion("heatwaveR")
library(doParallel); registerDoParallel(cores = 50)

# Coordinates with surface area
load("metadata/lon_lat_OISST_area.RData")

# Check on one pixel
# SST <- tidync(OISST_files[which(lon_OISST == -178.625)]) %>%
#   hyper_filter(lat = lat == -10.875) %>% 
#   hyper_tibble() %>% 
#   mutate(time = as.Date(time, origin = "1970-01-01")) %>% 
#   dplyr::rename(t = time, temp = sst)
# SST_clim <- ts2clm(SST, climatologyPeriod = c("1982-01-01", "2011-12-31"), pctile = 10)
# SST_event <- detect_event(SST_clim, coldSpells = T)
# SST_event_event <- SST_event$event
# SST_event_clim <- SST_event$climatology
# SST_cat <- category(SST_event, climatology = T, season = "peak")
# SST_cat_event <- SST_cat$event
# SST_cat_clim <- SST_cat$climatology
# write_csv(SST, "test_SST.csv")


# 2: Full calculations  ---------------------------------------------------

# Function for loading OISST data, calculating MCSs, and saving the results
# lon_row <- 1
MCS_calc <- function(lon_row){
  
  # Begin
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  print(paste("Began run", lon_row_pad, "at", Sys.time()))
  
  # Load data
  SST <- tidync(OISST_files[lon_row]) %>% 
    hyper_tibble() %>% 
    mutate(time = as.Date(time, origin = "1970-01-01")) %>% 
    dplyr::rename(t = time, temp = sst)
  
  # Make calculations
  MCS_res <- SST %>%
    # filter(lat == -63.375, lon == 0.125) %>% # tester...
    group_by(lon, lat) %>%
    nest() %>% 
    mutate(clim = purrr::map(data, ts2clm, climatologyPeriod = c("1982-01-01", "2011-12-31"), pctile = 10),
           event = purrr::map(clim, detect_event, coldSpells = T), 
           cat = purrr::map(event, category, climatology = T, season = "peak"),
           cat_correct = purrr::map(event, category, climatology = T, season = "peak", MCScorrect = T)) %>%
    select(-data, -clim)
  
  # Finish
  saveRDS(MCS_res, paste0("../data/MCS/MCS.calc.", lon_row_pad,".Rds"))
  rm(SST, MCS_res); gc()
  print(paste("Completed run ",lon_row_pad," at ",Sys.time()))
}

# system.time(
#   MCS_calc(1)
# ) # 150 seconds

# Ran on Saturday, October 31st, 2020
# plyr::l_ply(1:1440, .fun = MCS_calc, .parallel = T)
# Takes just over two hours on 50 cores


# 3: Daily categories -----------------------------------------------------

# Function for loading a cat_lon slice and extracting a single day of values
# testers...
# cat_lon_file <- MCS_lon_files[1]
# date_range <- c(as.Date("2019-11-01"), as.Date("2020-01-07"))
# date_range <- c(as.Date("1982-01-01"), as.Date("1982-12-31"))
load_sub_cat_clim <- function(cat_lon_file, date_range){
  
  # The original MCS methodology
  cat_sub <- readRDS(cat_lon_file) %>%
    dplyr::select(-event, -cat_correct) %>% 
    unnest(cols = cat) %>% 
    filter(row_number() %% 2 == 1) %>% 
    filter(nrow(cat$climatology) > 0) %>%
    unnest(cols = cat) %>% 
    ungroup() %>% 
    filter(t >= date_range[1], t <= date_range[2])
  
  # Those corrected for the 1.8C freezing point
  cat_correct_sub <- readRDS(cat_lon_file) %>%
    dplyr::select(-event, -cat) %>% 
    unnest(cols = cat_correct) %>% 
    filter(row_number() %% 2 == 1) %>% 
    filter(nrow(cat_correct$climatology) > 0) %>%
    unnest(cols = cat_correct) %>% 
    ungroup() %>% 
    filter(t >= date_range[1], t <= date_range[2])
  
  # The categories corrected for near-ice events
  cat_ice_ref <- readRDS(cat_lon_file)  %>%
    dplyr::select(-cat, -cat_correct) %>% 
    unnest(cols = event) %>% 
    filter(row_number() %% 2 == 1) %>% 
    unnest(cols = event) %>% 
    ungroup() %>% 
    filter(thresh < -1.5, event_no > 0,
           t >= date_range[1], t <= date_range[2]) %>% 
    mutate(ice = TRUE) %>% 
    dplyr::select(lon, lat, t, event_no, ice) %>%
    distinct()
  cat_ice_sub <- cat_correct_sub %>% 
    left_join(cat_ice_ref, by = c("lon", "lat", "t", "event_no")) %>% 
    mutate(category_ice = as.character(category),
           category_ice = case_when(ice == TRUE ~ "V Ice",
                                    TRUE ~ category)) %>% 
    dplyr::select(-ice, -category)
  
  # Join and exit
  cat_clim_sub <- left_join(cat_sub, cat_correct_sub,
                            by = c("lon", "lat", "t", "event_no", "intensity")) %>% 
    left_join(cat_ice_sub, by = c("lon", "lat", "t", "event_no", "intensity")) %>% 
    dplyr::rename(category = category.x, category_correct = category.y)
  rm(cat_sub, cat_correct_sub, cat_ice_ref, cat_ice_sub); gc()
  return(cat_clim_sub)
}

# Function for saving daily global cat files
# tester...
# df <- cat_clim_sub
# date_choice <- as.Date("2020-01-01")
save_sub_cat_clim <- function(date_choice, df){
  
  # Establish file name and save location
  cat_clim_year <- lubridate::year(date_choice)
  cat_clim_dir <- paste0("../data/cat_clim_MCS/",cat_clim_year)
  dir.create(as.character(cat_clim_dir), showWarnings = F)
  cat_clim_name <- paste0("cat.clim.MCS.",date_choice,".Rds")
  
  # Extract data and save
  df_sub <- df %>% 
    filter(t == date_choice)
  saveRDS(df_sub, file = paste0(cat_clim_dir,"/",cat_clim_name))
  rm(df); gc()
}

# Function for loading, prepping, and saving the daily global category slices
# tester...
# date_range <- c(as.Date("1982-01-01"), as.Date("1982-01-31"))
cat_clim_global_daily <- function(date_range){
  cat_clim_daily <- plyr::ldply(MCS_lon_files, load_sub_cat_clim,
                                .parallel = T, date_range = date_range) %>%
    mutate(intensity = round(intensity, 2),
           category = factor(category, 
                             levels = c("I Moderate", "II Strong",
                                        "III Severe", "IV Extreme")),
           category_correct = factor(category_correct, 
                                     levels = c("I Moderate", "II Strong",
                                                "III Severe", "IV Extreme")),
           category_ice = factor(category_ice,
                                 levels = c("I Moderate", "II Strong",
                                            "III Severe", "IV Extreme", "V Ice"))) %>%
    data.frame()
  
  # NB: Running this on too many cores may cause RAM issues
  registerDoParallel(cores = 10)
  plyr::l_ply(seq(min(cat_clim_daily$t), max(cat_clim_daily$t), by = "day"), 
              save_sub_cat_clim, .parallel = T,
              df = cat_clim_daily)
  rm(cat_clim_daily); gc()
}

# NB: Better not to run the entire 30+ years at once
# registerDoParallel(cores = 50)
# cat_clim_global_daily(date_range = c(as.Date("1982-01-01"), as.Date("1990-12-31"))) # ~20 minutes
# registerDoParallel(cores = 50)
# cat_clim_global_daily(date_range = c(as.Date("1991-01-01"), as.Date("2000-12-31")))
# registerDoParallel(cores = 50)
# cat_clim_global_daily(date_range = c(as.Date("2001-01-01"), as.Date("2010-12-31")))
# registerDoParallel(cores = 50)
# cat_clim_global_daily(date_range = c(as.Date("2011-01-01"), as.Date("2020-12-31")))


# 4: Annual summaries -----------------------------------------------------

# Function for finding the first date of the highest category MHW per pixel
max_event_date <- function(df){
  df %>% 
    group_by(lat) %>% 
    filter(as.integer(category_ice) == max(as.integer(category_ice))) %>% 
    filter(t == min(t)) %>% 
    ungroup()
}

# Function to create annual summaries
# testers...
# chosen_year <- 1982
# force_calc <- T
MCS_annual_state <- function(chosen_year, force_calc = F){
  
  print(paste0("Started run on ", chosen_year," at ",Sys.time()))
  
  ## Find file location
  MCS_cat_files <- dir(paste0("../data/cat_clim_MCS/", chosen_year), full.names = T)
  
  ## Create figure title
  if(length(MCS_cat_files) < 365){
    extra_bit <- " (so far)"
  } else{
    extra_bit <- ""
  }
  
  product_name <- "NOAA OISST"
  fig_title <- paste0("MCS categories of ",chosen_year,
                      "\n",product_name,"; Climatogy period: 1982-2011")
  
  ## Load/Process data
  # Categories per pixel
  if(file.exists(paste0("data/MCS_cat_pixel_",chosen_year,".Rds")) & !force_calc){
    MCS_cat_pixel <- readRDS(paste0("data/MCS_cat_pixel_",chosen_year,".Rds"))
  } else{
    
    # system.time(
    MCS_cat <- plyr::ldply(MCS_cat_files, readRDS, .parallel = T) #%>% 
      # right_join(OISST_no_ice_coords, by = c("lon", "lat")) %>%  # Filter out ice if desired
      # na.omit()
    # ) # 8 seconds
    
    # The sum of intensities per pixel for the year
    MCS_intensity <- MCS_cat %>% 
      group_by(lon, lat) %>% 
      summarise(intensity_sum = sum(intensity), .groups = "drop")
    
    # The earliest date of the highest category of event + the sum of intensities
    # system.time(
    MCS_cat_pixel <- MCS_cat %>% 
      dplyr::select(-event_no) %>% 
      na.omit() %>% 
      plyr::ddply(., c("lon"), max_event_date, .parallel = T) %>% 
      distinct() %>%
      left_join(MCS_intensity, by = c("lon", "lat"))
    # ) # 11 seconds
    saveRDS(MCS_cat_pixel, file = paste0("data/MCS_cat_pixel_",chosen_year,".Rds"))
    
    # Summarise the count of how many of each category of events were experienced in each pixel
    # system.time(
    MCS_cat_count <- lazy_dt(MCS_cat) %>% 
      group_by(lon, lat, event_no) %>% 
      summarise(max_cat = max(as.integer(category)),
                max_cat_correct = max(as.integer(category_correct)),
                max_cat_ice = max(as.integer(category_ice))) %>% 
      data.frame() %>% 
      dplyr::select(-event_no) %>% 
      mutate(max_cat = factor(max_cat, levels = c(1:4),  labels = levels(MCS_cat$category)),
             max_cat_correct = factor(max_cat_correct, levels = c(1:4),  labels = levels(MCS_cat$category)),
             max_cat_ice = factor(max_cat_ice, levels = c(1:5),  labels = levels(MCS_cat$category_ice))) %>% 
      pivot_longer(cols = max_cat:max_cat_ice) %>% 
      group_by(lon, lat, name) %>% 
      table() %>% 
      as.data.frame() %>% 
      pivot_wider(values_from = Freq, names_from = name) %>% 
      mutate(lon = as.numeric(as.character(lon)),
             lat = as.numeric(as.character(lat)))
    # ) # 34 seconds
    saveRDS(MCS_cat_count, paste0("data/MCS_cat_count_", chosen_year,".Rds"))
  }
  
  # Daily count and cumulative count per pixel
  if(file.exists(paste0("data/MCS_cat_daily_",chosen_year,".Rds")) & !force_calc){
    MCS_cat_daily <- readRDS(paste0("data/MCS_cat_daily_",chosen_year,".Rds"))
  } else{
    
    # Complete dates by categories data.frame
    full_grid <- expand_grid(t = seq(as.Date(paste0(chosen_year,"-01-01")), max(MCS_cat$t), by = "day"), 
                             category = as.factor(levels(MCS_cat$category_ice)),
                             name = c("category", "category_correct", "category_ice"))
    
    # The count and area of the first time the largest category pixel occurs at each pixel and the cumulative values
    # system.time(
    MCS_cat_first <- MCS_cat_pixel %>%
      dplyr::select(lon, lat, t, category, category_correct, category_ice) %>% 
      right_join(lon_lat_OISST_area, by = c("lon", "lat")) %>% 
      pivot_longer(cols = category:category_ice, values_to = "category") %>% 
      group_by(t, name, category) %>%
      summarise(first_n = n(),
                first_area = sum(sq_area), .groups = "drop") %>% 
      right_join(full_grid, by = c("t", "category", "name")) %>%
      mutate(first_n = ifelse(is.na(first_n), 0, first_n),
             first_n_prop = round(first_n/nrow(OISST_ocean_coords), 4),
             first_area = ifelse(is.na(first_area), 0, first_area),
             first_area_prop = round(first_area/sum(lon_lat_OISST_area$sq_area), 4)) %>% 
      arrange(t, name, category) %>% 
      group_by(name, category) %>%
      mutate(first_n_cum = cumsum(first_n),
             first_area_cum = cumsum(first_area),
             first_n_cum_prop = round(first_n_cum/nrow(OISST_ocean_coords), 4),
             first_area_cum_prop = round(first_area_cum/sum(lon_lat_OISST_area$sq_area), 4)) %>% 
      ungroup()
    # ) # 1 second
    
    # The count and area of categories of MCSs happening on a given day, and cumulatively throughout the year
    # system.time(
    MCS_cat_daily <- MCS_cat %>% 
      dplyr::select(lon, lat, t, category, category_correct, category_ice) %>% 
      right_join(lon_lat_OISST_area, by = c("lon", "lat")) %>% 
      pivot_longer(cols = category:category_ice, values_to = "category") %>% 
      group_by(t, name, category) %>%
      summarise(cat_n = n(),
                cat_area = sum(sq_area), .groups = "drop") %>% 
      right_join(full_grid, by = c("t", "category", "name")) %>% 
      mutate(cat_n = ifelse(is.na(cat_n), 0, cat_n),
             cat_n_prop = round(cat_n/nrow(OISST_ocean_coords), 4),
             cat_area = ifelse(is.na(cat_area), 0, cat_area),
             cat_area_prop = round(cat_area/sum(lon_lat_OISST_area$sq_area), 4)) %>% 
      arrange(t, name, category) %>% 
      group_by(name, category) %>%
      mutate(cat_n_cum = cumsum(cat_n),
             cat_area_cum = cumsum(cat_area),
             cat_n_cum_prop = round(cat_n_cum/nrow(OISST_ocean_coords), 4),
             cat_area_cum_prop = round(cat_area_cum/sum(lon_lat_OISST_area$sq_area), 4)) %>% 
      right_join(MCS_cat_first, by = c("t", "name", "category"))
    # ) # 16 second
    saveRDS(MCS_cat_daily, file = paste0("data/MCS_cat_daily_",chosen_year,".Rds"))
  }

  # Chose the type of categories to display
  MCS_cat_filter <- filter(MCS_cat_daily, name == "category")
  
  # Extract small data.frame for easier labeling
  MCS_cat_filter_labels <- MCS_cat_filter %>% 
    group_by(category) %>% 
    filter(t == max(t)) %>% 
    ungroup() %>% 
    mutate(label_first_n_cum = cumsum(first_area_cum_prop)) %>% 
    filter(first_n_cum != 0)
  
  ## Create figures
  # Global map of MHW occurrence
  fig_map <- ggplot(MCS_cat_pixel, aes(x = lon, y = lat)) +
    # geom_tile(data = OISST_ice_coords, fill = "powderblue", colour = NA, alpha = 0.5) +
    geom_tile(aes(fill = category), colour = NA) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    scale_fill_manual("Category", values = MCS_colours) +
    coord_cartesian(expand = F, ylim = c(min(OISST_ocean_coords$lat),
                                         max(OISST_ocean_coords$lat))) +
    theme_void() +
    guides(fill = guide_legend(override.aes = list(size = 10))) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          legend.position = "bottom",
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          panel.background = element_rect(fill = "grey90"))
  # fig_map
  
  # Stacked barplot of global daily count of MHWs by category
  fig_count <- ggplot(MCS_cat_filter, aes(x = t, y = cat_area_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MCS_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
    labs(y = "Daily MCS coverage for ocean\n(non-cumulative)", x = "Day of the year") +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 13))
  # fig_count
  
  # Stacked barplot of cumulative percent of ocean affected by MHWs
  fig_cum <- ggplot(MCS_cat_filter, aes(x = t, y = first_area_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    geom_hline(data = MCS_cat_filter_labels, show.legend = F,
               aes(yintercept = label_first_n_cum, colour = category)) +
    scale_fill_manual("Category", values = MCS_colours) +
    scale_colour_manual("Category", values = MCS_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +
    labs(y = "Top MCS category for ocean\n(cumulative)", x = "Day of first occurrence") +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 13))
  # fig_cum
  
  # Stacked barplot of average cumulative MHW days per pixel
  fig_prop <- ggplot(MCS_cat_filter, aes(x = t, y = cat_area_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = F,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MCS_colours) +
    scale_y_continuous(breaks = round(seq(sum(MCS_cat_filter_labels$cat_area_cum_prop)*0.25,
                                          sum(MCS_cat_filter_labels$cat_area_cum_prop)*0.75, length.out = 3), 0)) +
    scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +  
    labs(y = "Average MCS days for ocean\n(cumulative)", x = "Day of the year") +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 13))
  # fig_prop
  
  # print("Combining figures")
  fig_ALL_sub <- ggpubr::ggarrange(fig_count, fig_cum, fig_prop, ncol = 3, align = "hv",
                                   labels = c("B)", "C)", "D)"), font.label = list(size = 16))
  fig_ALL <- ggpubr::ggarrange(fig_map, fig_ALL_sub, ncol = 1, heights = c(1, 0.6),
                               labels = c("A)"), common.legend = T, legend = "bottom",
                               font.label = list(size = 16))
  
  # Add title
  fig_ALL_cap <- grid::textGrob(fig_title, x = 0.01, just = "left", gp = grid::gpar(fontsize = 20))
  fig_ALL_cap <- ggpubr::ggarrange(fig_ALL_cap, fig_ALL, heights = c(0.07, 1), nrow = 2)
  
  # print("Saving final figure")
  ggsave(fig_ALL_cap, height = 12, width = 18, filename = paste0("output/cat_summary_",chosen_year,".png"))
}

# Run ALL years
# NB: Running this in parallel will cause a proper stack overflow
# registerDoParallel(cores = 50)
# plyr::l_ply(1982:2020, MCS_annual_state, .parallel = F, force_calc = T) # ~60 seconds for one


# 5: Total summaries ------------------------------------------------------

MCS_total_state <- function(){
  
  # Load data
  MCS_cat_daily_files <- dir("data", pattern = "MCS_cat_daily", full.names = T)
  MCS_cat_daily_files <- MCS_cat_daily_files[!grepl("total", MCS_cat_daily_files)]
  
  # Create mean values of daily count
  cat_daily_mean <- map_dfr(MCS_cat_daily_files, readRDS) %>%
    mutate(t = lubridate::year(t)) %>%
    group_by(t, name, category) %>%
    # summarise_all(mean, .groups = "drop") %>% 
    summarise(cat_area_prop_mean = mean(cat_area_prop, na.rm = T), .groups = "drop") %>%
    # mutate(cat_n_prop_daily_mean = round(cat_n/nrow(OISST_ocean_coords), 4)) %>%
    # na.omit() %>% 
    data.frame()
  
  # Extract only values from December 31st
  cat_daily <- map_dfr(MCS_cat_daily_files, readRDS) %>%
    filter(lubridate::month(t) == 12, lubridate::day(t) == 31) %>%
    mutate(t = lubridate::year(t)) %>% #,
           # first_n_cum_prop = round(first_n_cum/nrow(OISST_ocean_coords), 4)) %>% 
    left_join(cat_daily_mean, by = c("t", "name", "category"))
  
  # Save and exit
  saveRDS(cat_daily, paste0("data/MCS_cat_daily_total.Rds"))
}

## Run it
# MCS_total_state()

## Create figures
MCS_total_state_fig <- function(df){
  
  # Chose category system
  df_filter <- filter(df, name == "category") %>% 
    filter(first_n_cum > 0)
  
  # Stacked barplot of global daily count of MHWs by category
  fig_count_historic <- ggplot(df_filter, aes(x = t, y = cat_area_prop_mean)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = T,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MCS_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_continuous(breaks = seq(1982, 2019, 5)) +
    labs(y = "Average daily MCS \ncoverage for ocean", x = NULL) +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16))
  # fig_count_historic
  
  # Stacked barplot of cumulative percent of ocean affected by MHWs
  fig_cum_historic <- ggplot(df_filter, aes(x = t, y = first_area_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = T,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MCS_colours) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = seq(0.2, 0.8, length.out = 4),
                       labels = paste0(seq(20, 80, by = 20), "%")) +
    scale_x_continuous(breaks = seq(1982, 2019, 5)) +
    labs(y = "Total ocean experienceing \nat least one MCS", x = NULL) +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16))
  # fig_cum_historic
  
  # Stacked barplot of average cumulative MHW days for ocean
  fig_prop_historic <- ggplot(df_filter, aes(x = t, y = cat_area_cum_prop)) +
    geom_bar(aes(fill = category), stat = "identity", show.legend = T,
             position = position_stack(reverse = TRUE), width = 1) +
    scale_fill_manual("Category", values = MCS_colours) +
    scale_y_continuous(limits = c(0, 30),
                       breaks = seq(10, 20, length.out = 2)) +
    scale_x_continuous(breaks = seq(1982, 2019, 5)) +
    labs(y = "Total MCS days for ocean", x = NULL) +
    coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16))
  # fig_prop_historic
  
  # Create the figure title
  product_title <- "NOAA OISST"
  min_year <- min(df_filter$t)
  max_year <- max(df_filter$t)
  fig_title <- paste0("MCS category summaries: ",min_year,"-",max_year,
                      "\n",product_title,"; Climatogy period: 1982-2011")
  
  # Stick them together and save
  fig_ALL_total <- ggpubr::ggarrange(fig_count_historic, fig_cum_historic, fig_prop_historic,
                                     ncol = 3, align = "hv", labels = c("A)", "B)", "C)"), hjust = -0.1,
                                     font.label = list(size = 14), common.legend = T, legend = "bottom")
  
  # Add title
  fig_ALL_cap <- grid::textGrob(fig_title, x = 0.01, just = "left", gp = grid::gpar(fontsize = 20))
  fig_ALL_cap <- ggpubr::ggarrange(fig_ALL_cap, fig_ALL_total, heights = c(0.07, 1), nrow = 2)
  
  # Save
  ggsave(fig_ALL_cap, filename = paste0("output/cat_summary_total.png"), height = 4.25, width = 12)
}

## Run it
# MCS_total <- readRDS("data/MCS_cat_daily_total.Rds")
# MCS_total_state_fig(MCS_total)


# 6: Trends ---------------------------------------------------------------

MCS_trend_calc <- function(lon_step){
  
  # Start
  lon_step_pad <- str_pad(lon_step, 4, pad = "0")
  print(paste0("Began run on ",lon_step," at ", Sys.time()))
  
  # Load chosen file
  MCS_res <- readRDS(MCS_lon_files[lon_step])
  
  # Unpack categories
  # Change these following lines to select category method results
  MCS_cat <- MCS_res %>%
    dplyr::select(-event, -cat_correct) %>%
    unnest(cols = cat) %>% 
    filter(row_number() %% 2 == 0) %>% 
    filter(nrow(cat$event) > 0) %>% 
    unnest(cols = cat) %>% 
    ungroup() %>% 
    mutate(category = factor(category, levels = c("I Moderate", "II Strong",
                                                  "III Severe", "IV Extreme")),
           season = factor(season, levels = c("Spring", "Summer", "Fall", "Winter"))) %>% 
    data.frame()
  
  # Unpack event metrics and join
  MCS_event <- MCS_res %>%
    dplyr::select(-cat, -cat_correct) %>% 
    unnest(cols = event) %>% 
    filter(row_number() %% 2 == 0) %>% 
    filter(nrow(event$event) > 0) %>%
    unnest(cols = event) %>%
    ungroup() %>% 
    left_join(MCS_cat, by = c("lon", "lat", "event_no", "duration")) %>% 
    ungroup() %>% 
    mutate(year = year(date_peak))
  rm(MCS_res); gc()
  
  # Annual metric summaries
  suppressWarnings(
  suppressMessages(
  MCS_metric <- MCS_event %>% 
    group_by(lon, lat, year) %>% 
    summarise(count = n(),
              dur_mean = mean(duration, na.rm = T),
              dur_sum = sum(duration, na.rm = T),
              i_mean = mean(intensity_mean, na.rm = T),
              i_max_mean = mean(intensity_max, na.rm = T),
              i_max_min = min(intensity_max, na.rm = T),
              i_cum_mean = mean(intensity_cumulative, na.rm = T),
              i_cum_sum = sum(intensity_cumulative, na.rm = T),
              onset_mean = mean(rate_onset, na.rm = T),
              onset_min = min(rate_onset, na.rm = T),
              decline_mean = mean(rate_decline, na.rm = T),
              decline_min = min(rate_decline, na.rm = T),
              p_moderate = mean(p_moderate, na.rm = T),
              p_strong = mean(p_strong, na.rm = T),
              p_severe = mean(p_severe, na.rm = T),
              p_extreme = mean(p_extreme, na.rm = T)) %>% 
    filter(p_extreme >= 0) %>% 
    mutate_if(is.numeric, round, 4) %>% 
    pivot_longer(cols = c(-lon, -lat, -year))
  ))
  MCS_metric$value[is.na(MCS_metric$value)] <- NA
  MCS_metric$value[is.infinite(as.matrix(MCS_metric$value))] <- NA
  
  # Annual metric trends
  suppressWarnings(
  MCS_metric_trends <- MCS_metric %>% 
    group_by(lon, lat, name) %>% 
    nest() %>% 
    mutate(model = map(data, ~lm(value ~ year, data = .)),
           model_out = map(model, ~broom::tidy(.))) %>% 
    dplyr::select(-data, -model) %>% 
    unnest(cols = model_out) %>% 
    ungroup() %>% 
    filter(term == "year") %>% 
    dplyr::rename(slope = estimate) %>% 
    dplyr::select(lon, lat, name, slope, p.value) %>% 
    mutate(slope = round(slope, 4), 
           p.value = round(p.value, 4))
  )
  MCS_metric_trends[is.na(MCS_metric_trends)] <- 1
  
  # Annual category count summaries 
  MCS_cat_count <- MCS_event %>% 
    dplyr::select(lon, lat, year, category) %>% 
    group_by(lon, lat, year) %>% 
    table() %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    pivot_wider(values_from = Freq, names_from = category)
  
  # Annual peak of season count
  MCS_season_count <- MCS_event %>% 
    dplyr::select(lon, lat, year, season) %>% 
    group_by(lon, lat, year) %>% 
    table() %>% 
    as.data.frame(stringsAsFactors = F) %>% 
    pivot_wider(values_from = Freq, names_from = season)
  
  # Join all count data.frames
  MCS_count <- left_join(MCS_cat_count, MCS_season_count,
                         by = c("lon", "lat", "year")) %>%
    mutate(total_count = Spring + Summer + Fall + Winter,
           lon = as.numeric(lon),
           lat = as.numeric(lat),
           year = as.numeric(year)) %>% 
    pivot_longer(cols = c(-lon, -lat, -year))
  
  # Trends in count values
  suppressWarnings(
  MCS_count_trends <- MCS_count %>% 
    group_by(lon, lat, name) %>% 
    nest() %>% 
    mutate(model = map(data, ~lm(value ~ year, data = .)),
           model_out = map(model, ~broom::tidy(.))) %>% 
    dplyr::select(-data, -model) %>% 
    unnest(cols = model_out) %>% 
    ungroup() %>% 
    filter(term == "year") %>% 
    dplyr::rename(slope = estimate) %>% 
    dplyr::select(lon, lat, name, slope, p.value) %>% 
    mutate(slope = round(slope, 4), 
           p.value = round(p.value, 4))
  )
  MCS_count_trends[is.na(MCS_count_trends)] <- 1
  
  # Final data.frame and save
  MCS_count_trend <- rbind(MCS_metric, MCS_count) %>% 
    dplyr::select(-year) %>% 
    group_by(lon, lat, name) %>%
    summarise_if(is.numeric, mean, na.rm = T) %>% 
    left_join(rbind(MCS_metric_trends, MCS_count_trends), by = c("lon", "lat", "name")) %>% 
    mutate(value = round(value, 4))
  # saveRDS(MCS_count_trend, paste0("data/MCS_count_trend_",lon_step_pad,".Rds"))
}

# Run one
# system.time(
#   MCS_trend_calc(1)
# ) # 49 seconds

# Run all
# registerDoParallel(cores = 50)
system.time(MCS_count_trend <- plyr::ldply(1:1440, MCS_trend_calc, .parallel = T, .paropts = c(.inorder = F))) # 2737 seconds
# NB: This file is too large to host on GitHub
saveRDS(MCS_count_trend, "data/MCS_count_trend.Rds")

# Figures of trends and annual states
var_mean_trend_fig <- function(var_name){
  
  # Basic filter
  df <- MCS_count_trend %>% 
    filter(name == var_name,
           lat >= -70, lat <= 70)
  
  # Significant results
  df_p <- df %>% 
    filter(p.value <= 0.05)
  
  # Find 10th and 90th quantiles to round off tails for plotting
  value_q10 <- quantile(df$value, 0.1)
  value_q90 <- quantile(df$value, 0.9)
  slope_q10 <- quantile(df$slope, 0.1)
  slope_q90 <- quantile(df$slope, 0.9)
  
  # The mean value map
  mean_map <- df %>% 
    mutate(value = case_when(value <= value_q10 ~ value_q10,
                             value >= value_q90 ~ value_q90,
                             TRUE ~ value)) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    scale_fill_viridis_c("Mean\n(annual)") +
    # coord_cartesian(expand = F, ylim = c(min(OISST_ocean_coords$lat),
    #                                      max(OISST_ocean_coords$lat))) +
    coord_cartesian(expand = F, ylim = c(-70, 70)) +
    theme_void() +
    # guides(fill = guide_legend(override.aes = list(size = 10))) +
    labs(title = var_name) +
    theme(legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          panel.background = element_rect(fill = "grey90"))
  # mean_map
  
  # The trend map
  trend_map <- df %>% 
    mutate(slope = case_when(slope <= slope_q10 ~ slope_q10,
                             slope >= slope_q90 ~ slope_q90,
                             TRUE ~ slope)) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_raster(aes(fill = slope)) +
    # geom_point(data = df_p, shape = 4, size = 0.1, alpha = 0.1) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    scale_fill_gradient2("Slope\n(annual)", low = "blue", high = "red") +
    # coord_cartesian(expand = F, ylim = c(min(OISST_ocean_coords$lat),
    #                                      max(OISST_ocean_coords$lat))) +
    coord_cartesian(expand = F, ylim = c(-70, 70)) +
    theme_void() +
    # guides(fill = guide_legend(override.aes = list(size = 10))) +
    labs(title = paste0(var_name," trend")) +
    theme(legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          panel.background = element_rect(fill = "grey90"))
  # trend_map
  
  full_map <- ggpubr::ggarrange(mean_map, trend_map, ncol = 1, nrow = 2, align = "hv")
  ggsave(paste0("output/mean_trend_",var_name,".png"), full_map, width = 12, height = 12)
}

# Load all pixel values/trends into one brick
MCS_count_trend <- readRDS("data/MCS_count_trend.Rds")
unique(MCS_count_trend$name)

# Create all variable maps
plyr::l_ply(unique(MCS_count_trend$name), var_mean_trend_fig, .parallel = T)

# Global average trends
MCS_count_trend %>%
  filter(name %in% c("total_count", "dur_mean", "i_max_mean", "i_cum_mean")) %>%
  group_by(name) %>%
  summarise(mean_slope = mean(slope, na.rm = T),
            median_slope = median(slope, na.rm = T))


# 7: MHWs minus MCSs ------------------------------------------------------

# Function that loads one MHW and one MCS lon slice and subtracts them
MHW_v_MCS_func <- function(lon_row){
  lon_row_pad <- str_pad(lon_row, width = 4, pad = "0", side = "left")
  
  # Load and prep MHW data
  MHW_mean <- readRDS(paste0("../data/event/MHW.event.",lon_row_pad,".Rda")) %>% 
    dplyr::select(lon, lat, duration, intensity_mean:intensity_cumulative) %>% 
    group_by(lon, lat) %>% 
    mutate(count = n()) %>% 
    summarise_all("mean", .groups = "drop")
  
  # Load and prep MCS data
  MCS_mean <- load_MCS_event_sub(MCS_lon_files[lon_row], lat_range = c(-90, 90),
                                 date_range = c("1982-01-01", "2020-12-31")) %>% 
    dplyr::select(lon, lat, duration, intensity_mean, intensity_max, intensity_cumulative) %>% 
    group_by(lon, lat) %>% 
    mutate(count = n()) %>% 
    summarise_all("mean", .groups = "drop")
  
  # Subtract MCS from MHW
  MHW_v_MCS <- left_join(MHW_mean, MCS_mean, by = c("lon", "lat")) %>% 
    mutate(count = count.x - count.y,
           dur = round(duration.x - duration.y, 2),
           i_mean = round(intensity_mean.x - abs(intensity_mean.y), 2),
           i_max = round(intensity_max.x - abs(intensity_max.y), 2),
           i_cum = round(intensity_cumulative.x - abs(intensity_cumulative.y), 2)) %>% 
    dplyr::select(lon, lat, count:i_cum)
  return(MHW_v_MCS)
}

registerDoParallel(cores = 50)
system.time(MHW_v_MCS <- plyr::ldply(1:1440, MHW_v_MCS_func, .parallel = T, .paropts = c(.inorder = F))) # 104 seconds
saveRDS(MHW_v_MCS, "data/MHW_v_MCS.Rds")


# 8: SSTa skewness and kurtosis -------------------------------------------

# testers...
# file_name_MCS <- MCS_RData[1]
# lon_step <- lon_OISST[1]
skew_kurt_calc <- function(lon_step){
  
  # Load the data
  df <- sst_seas_thresh_merge(lon_step, date_range = as.Date("1982-01-01"))
  
  # Add a season category
  df_season <- df %>% 
    mutate(month = month(t, label = T)) %>% 
    mutate(season = case_when(month %in% c("Jan", "Feb", "Mar") & lat > 0 ~ "Winter",
                              month %in% c("Apr", "May", "Jun") & lat > 0 ~ "Spring",
                              month %in% c("Jul", "Aug", "Sep") & lat > 0 ~ "Summer",
                              month %in% c("Oct", "Nov", "Dec") & lat > 0 ~ "Autumn",
                              month %in% c("Jan", "Feb", "Mar") & lat < 0 ~ "Summer",
                              month %in% c("Apr", "May", "Jun") & lat < 0 ~ "Autumn",
                              month %in% c("Jul", "Aug", "Sep") & lat < 0 ~ "Winter",
                              month %in% c("Oct", "Nov", "Dec") & lat < 0 ~ "Spring")) %>% 
    dplyr::select(-month)
  
  # Combine data frames and calculate skewness and kurtosis
  skew_kurt <- df %>%
    mutate(season = "Total") %>% 
    rbind(., df_season) %>% 
    mutate(season = factor(season, levels = c("Total", "Spring", "Summer", "Autumn", "Winter"))) %>% 
    group_by(lon, lat, season) %>% 
    summarise(anom_skew = round(skewness(anom), 2),
              anom_kurt = round(kurtosis(anom), 2),
              anom_min = min(anom),
              anom_mean = round(mean(anom), 2),
              anom_max = max(anom),
              anom_sd = round(sd(anom), 4), .groups = "drop")
  return(skew_kurt)
}

# Calculate global SSTa stats
registerDoParallel(cores = 25)
system.time(SSTa_stats <- plyr::ldply(lon_OISST, skew_kurt_calc, .parallel = T, .paropts = c(.inorder = F))) # 1001 seconds
# NB: This file is too large to host on GitHub
saveRDS(SSTa_stats, "data/SSTa_stats.Rds")

# Show a ridegplot with the fill for kurtosis and the colour for skewness
SSTa_ridge <- SSTa_stats %>%
  mutate(lat_10 = factor(plyr::round_any(lat, 10))) %>%
  dplyr::select(-lon, -lat) %>%
  mutate(season = factor(season, levels = c("Spring", "Summer", "Autumn", "Winter", "Total"))) %>%
  ggplot(aes(x = anom_skew, y = lat_10)) +
  geom_density_ridges(aes(fill = season), alpha = 0.5, size = 0.1) +
  # scale_x_continuous(limits = c(-2, 10), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-2, 5), expand = c(0, 0)) +
  theme_ridges()
ggsave("output/kurt_skew_lon.png", SSTa_ridge, width = 12)


# 9: Spatial correlations -------------------------------------------------

# Load mean values
MCS_count_trend <- readRDS("data/MCS_count_trend.Rds")
SSTa_stats <- readRDS("data/SSTa_stats.Rds")
MHW_v_MCS <- readRDS("data/MHW_v_MCS.Rds")
global_stats <- MCS_count_trend %>%
  left_join(filter(SSTa_stats, season == "Total"), by = c("lon", "lat")) %>%
  left_join(MHW_v_MCS, by = c("lon", "lat"))

## Calculate spatial correlations
# cor count vs. duration
global_stats %>%
  filter(name %in% c("dur_mean", "total_count")) %>%
  dplyr::select(lon, lat, name, value) %>%
  pivot_wider(values_from = value) %>%
  na.omit() %>%
  summarise(r = correlation::cor_test(., x = "dur_mean", y = "total_count"))

# cor max int. vs. SSTa SD
global_stats %>%
  filter(name %in% c("i_max_mean")) %>%
  dplyr::select(lon, lat, name, value, anom_sd) %>%
  pivot_wider(values_from = value) %>%
  na.omit() %>%
  summarise(r = correlation::cor_test(., x = "i_max_mean", y = "anom_sd"))

# cor max. int. vs. cum. int.
global_stats %>%
  filter(name %in% c("i_max_mean", "i_cum_mean")) %>%
  dplyr::select(lon, lat, name, value) %>%
  pivot_wider(values_from = value) %>%
  na.omit() %>%
  summarise(r = correlation::cor_test(., x = "i_max_mean", y = "i_cum_mean"))

# cor count vs. duration trend
global_stats %>%
  filter(name %in% c("dur_mean", "total_count")) %>%
  dplyr::select(lon, lat, name, slope) %>%
  pivot_wider(values_from = slope) %>%
  na.omit() %>%
  summarise(r = correlation::cor_test(., x = "dur_mean", y = "total_count"))

# cor duration vs. int. cum. trend
global_stats %>%
  filter(name %in% c("dur_mean", "i_cum_mean")) %>%
  dplyr::select(lon, lat, name, slope) %>%
  pivot_wider(values_from = slope) %>%
  na.omit() %>%
  summarise(r = correlation::cor_test(., x = "dur_mean", y = "i_cum_mean"))
  
# cor MHW-MCS intensity vs. SSTa skewness
global_stats %>%
  dplyr::select(lon, lat, anom_skew, i_mean) %>%
  distinct() %>%
  na.omit() %>%
  as_tibble() %>% 
  summarise(r = correlation::cor_test(., x = "anom_skew", y = "i_mean"))


# 10: Check on MCS hole in Antarctica -------------------------------------

# There is a hole in the MCS results in the Southern Ocean where no MCS are reported
# The analysis below reveals that the issue is the 10th percentile is -1.8C because 
# his patch is almost always frozen throughout the entire satellite record
# On second pass it appears that the detect_event() function is changing the threshold
# after it was already calculated...

# Load comparison data to find the hole easily via NA results
MHW_v_MCS <- readRDS("data/MHW_v_MCS.Rds")

# Extract example coords: lon = -37.875, lat = -75.875
hole_SST <- tidync(OISST_files[which(lon_OISST == -37.875)]) %>%
  hyper_tibble() %>%
  mutate(time = as.Date(time, origin = "1970-01-01")) %>%
  dplyr::rename(t = time, temp = sst) %>%
  filter(lat == -75.875)

# Calculate clims separately
hole_clim_MCS <- ts2clm(hole_SST, climatologyPeriod = c("1982-01-01", "2011-12-31"), pctile = 10)

# Plot. Change pctile argument above to see effect here. E.g. pctile = 50 will give the seasonal signal
hole_clim_MCS %>%
  dplyr::select(doy, thresh) %>%
  distinct() %>%
  ggplot(aes(x = doy, y = thresh)) +
  geom_line()

# Calculate MHW and MCS
hole_MHW <- detect_event(ts2clm(hole_SST, climatologyPeriod = c("1982-01-01", "2011-12-31")))
hole_MCS <- detect_event(hole_clim_MCS, coldSpells = T)

# Look at results
hole_MCS_clim <- hole_MCS$climatology

