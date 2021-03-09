# MCS_figures.R
# This script houses the code used to make the figures for the MCS manuscript


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
source("code/functions.R")
library(ggpattern)
library(ggsci) # Scientific colour palettes


# Figure 1 ----------------------------------------------------------------

# Figure showing where in the world noteworthy MCSs from the literature occurred

# Function that prepares category grid for plotting
# intensity_choice = "max"
# date_range <- c("2003-05-01", "2003-10-31")
fig_1_prep_func <- function(MCS_data, date_range, intensity_choice = "cumulative", rgn){
  
  # Find the most intense point
  if(intensity_choice == "max"){
    centre_point <- MCS_data$event_data %>% 
      filter(date_start >= date_range[1],
             date_end <= date_range[2]) %>%
      filter(intensity_max == min(intensity_max, na.rm = T))
  } else if(intensity_choice == "cumulative"){
    centre_point <- MCS_data$event_data %>% 
      filter(date_start >= date_range[1],
             date_end <= date_range[2]) %>%
      filter(intensity_cumulative == min(intensity_cumulative, na.rm = T))
  } else if(intensity_choice == "date_range"){
    centre_point <- data.frame(date_start = date_range[1],
                               date_end = date_range[2])
  }
  
  # Extract data based on dates of occurrence of MCS
  suppressWarnings( # Don't need to know about pixels without MCS data
    res <- MCS_data$clim_data %>% 
      filter(t >= centre_point$date_start,
             t <= centre_point$date_end) %>%
      mutate(category = factor(category,
                               levels = c("I Moderate", "II Strong",
                                          "III Severe", "IV Extreme")),
             cat_int = as.integer(category)) %>% 
      group_by(lon, lat) %>% 
      filter(cat_int == max(cat_int, na.rm = T)) %>% 
      filter(intensity == min(intensity, na.rm = T)) %>% 
      ungroup() %>% 
      mutate(region = rgn)
  )
}

# Test plot to check extracted data
fig_1_test_plot <- function(MCS_data){
  ggplot(data = MCS_data, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = category)) +
    scale_fill_manual(values = MCS_colours)
}

# The California 2002, Florida 2003 & 2010, Taiwan 2008, North Atlantic 2013-2015, Australia 2017 and 1962-3 Europe?

# California current
CC_bound <- c(34, 48, -130, -124)
CC_data <- load_MCS_ALL(CC_bound)
CC_data_2002 <- extract_MCS_grid_year(CC_data, 2002)
CC_prep_2002 <- fig_1_prep_func(CC_data_2002, c("2002-05-01", "2002-10-31"), "cumulative", "CC") %>% 
  filter(lon >= -128, lat >= 36)
fig_1_test_plot(CC_prep_2002)
rm(CC_data); gc()

# One of the most widely published MCS is that which occurred off Florida in 2003
FL_bound <- c(24, 40, -84, -70)
FL_data <- load_MCS_ALL(FL_bound)
FL_data_2003 <- extract_MCS_grid_year(FL_data, 2003)
FL_data_2010 <- extract_MCS_grid_year(FL_data, 2010)
FL_prep_2003 <- fig_1_prep_func(FL_data_2003, c("2003-05-01", "2003-10-31"), "cumulative", "FL") %>% 
  mutate(category2 = ifelse(lon >= -77.5 & lat <= 29.5, NA, category))
FL_prep_2010 <- fig_1_prep_func(FL_data_2010, c("2010-09-01", "2011-01-31"), "date_range", "FL")
fig_1_test_plot(FL_prep_2003)
fig_1_test_plot(FL_prep_2010)
rm(FL_data); gc()

# Texas is always getting pummeled
TX_bound <- c(17, 31, -98, -81)
TX_data <- load_MCS_ALL(TX_bound)
TX_data_1998 <- extract_MCS_grid_year(TX_data, 1998)
TX_prep_1998 <- fig_1_prep_func(TX_data_1998, c("1998-02-01", "1998-10-31"), "cumulative", "TX")
fig_1_test_plot(TX_prep_1998)
rm(TX_data); gc()

# Taiwan Strait
TS_bound <- c(18, 30, 112, 126)
TS_data <- load_MCS_ALL(TS_bound)
TS_data_2008 <- extract_MCS_grid_year(TS_data, 2008)
TS_prep_2008 <- fig_1_prep_func(TS_data_2008, c("2007-11-01", "2008-03-31"), "max", "TS")
fig_1_test_plot(TS_prep_2008)
rm(TS_data); gc()

# Atlantic Ocean cold blob 2014 - 2016 under Greenland
# NB: This one takes a while and a lot of RAM
AO_bound <- c(43, 65, -50, -7)
AO_data <- load_MCS_ALL(AO_bound)
AO_data_2013_15 <- extract_MCS_grid_year(AO_data, 2014, 3)
AO_prep_2013_15 <- fig_1_prep_func(AO_data_2013_15, c("2012-6-01", "2017-05-31"), "cumulative", "AO")
fig_1_test_plot(AO_prep_2013_15)
rm(AO_data); gc()

# Australia southern reef
OZ_bound <- c(-42, -34, 144, 155)
OZ_data <- load_MCS_ALL(OZ_bound)
OZ_data_2017 <- extract_MCS_grid_year(OZ_data, 2017)
OZ_prep_2017 <- fig_1_prep_func(OZ_data_2017, c("2017-02-01", "2017-06-30"), "date_range", "OZ") %>% 
  filter(lat > -41, lon < 149)
fig_1_test_plot(OZ_prep_2017)
rm(OZ_data); gc()

# Combine data
fig_1_data <- rbind(CC_prep_2002, FL_prep_2003, TX_prep_1998, TS_prep_2008, AO_prep_2013_15, OZ_prep_2017)

# Load icons for map

# Create matrix of lon/lat values from Table 1
# fig_1_table <- data.frame(lon = c(-96.1, -76.3, 3.0, -80.6, 118.2),
#                           lat = c(28.5, 35.4, 54.0, 28.8, 24.8),
#                           year = c(1941, 1958, 1962, 1977, 2008),
#                           impact = c("Fish kill", "Fish kill", "Fish kill", "Coral mortality", "Mass death"))

# Plot it
fig_1 <- ggplot(fig_1_data, aes(x = lon, y = lat)) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group), 
               fill = "grey50") +
  # geom_point(aes(colour = year, shape = impact), size = 5) +
  geom_raster(aes(fill = category)) +
  scale_fill_manual(values = MCS_colours) +
  labs(x = NULL, y = NULL) +
  coord_quickmap(expand = F, ylim = c(-70, 70)) +
  theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "top")
fig_1
ggsave("figures/fig_1.png", fig_1, height = 4, width = 8)
ggsave("figures/fig_1.pdf", fig_1, height = 4, width = 8)


# Figure 2 ----------------------------------------------------------------

# Find a pixel that naturally experienced a Cat 4 event
AC_bound <- c(-35, 4-5, 20, 35)
AC_data <- load_MCS_ALL(AC_bound)

# Manually look through the events to find a good Cat 4
AC_data_cat <- AC_data$cat_data

# A 2018 event at 29.625 -31.625 looks like a good candidate
AC_data_event_sub <- AC_data$event_data %>% 
  filter(lon == 29.625, lat == -31.625)
AC_data_clim_sub <- AC_data$clim_data %>% 
  filter(lon == 29.625, lat == -31.625,
         t >= "2017-12-01", t <= "2018-04-30") %>% 
  mutate(diff = thresh - seas,
         thresh_2x = thresh + diff,
         thresh_3x = thresh_2x + diff,
         thresh_4x = thresh_3x + diff)

# Further subset for correct hatch filling
AC_data_clim_sub_sub <- AC_data_clim_sub %>% 
  filter(event_no > 0)

# Schematic of a MCS
fig_2 <- ggplot(data = AC_data_clim_sub, aes(x = t)) +
  geom_flame(aes(y = thresh, y2 = temp, fill = "Moderate"), n = 5, n_gap = 2) +
  geom_flame(aes(y = thresh_2x, y2 = temp, fill = "Strong")) +
  geom_flame(aes(y = thresh_3x, y2 = temp, fill = "Severe")) +
  geom_flame(aes(y = thresh_4x, y2 = temp, fill = "Extreme")) +
  geom_ribbon_pattern(data = AC_data_clim_sub_sub, aes(ymin = seas, ymax = temp), 
                      pattern_fill = "steelblue1",
                      pattern = 'stripe', fill = NA, colour  = 'black', alpha = 0.3) +
  geom_line(aes(y = thresh_2x, col = "2x Threshold"), size = 0.2, linetype = "dashed") +
  geom_line(aes(y = thresh_3x, col = "3x Threshold"), size = 0.2, linetype = "dotdash") +
  geom_line(aes(y = thresh_4x, col = "4x Threshold"), size = 0.2, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 0.6) +
  geom_line(aes(y = thresh, col = "Threshold"), size = 0.6) +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.4) +
  # Cumulative intensity label
  geom_curve(colour = "steelblue1",
             aes(x = as.Date("2018-02-22"), xend = as.Date("2018-02-10"),
                 y = 25.8013, yend = 19.39), curvature = -0.4) +
  geom_label(aes(label = "Cum. Intensity = -70.04°CxDays", x = as.Date("2018-02-26"), y = 22.0),
             colour = "steelblue1", label.size = 3) +
  geom_label(aes(label = "Cum. Intensity = -70.04°CxDays", x = as.Date("2018-02-26"), y = 22.0),
             colour = "black", label.size = 0) +
  # Max intensity label
  geom_segment(colour = "midnightblue",
               aes(x = as.Date("2018-02-10"), xend = as.Date("2018-02-10"),
                   y = 25.6323, yend = 19.0)) +
  geom_label(aes(label = "Max. Intensity = -6.24°C", x = as.Date("2018-02-10"), y = 19.0),
             colour = "midnightblue", label.size = 3) +
  geom_label(aes(label = "Max. Intensity = -6.24°C", x = as.Date("2018-02-10"), y = 19.0),
             colour = "black", label.size = 0) +
  # Duration label
  geom_segment(colour = "slateblue1",
               aes(x = as.Date("2018-01-29"), xend = as.Date("2018-01-29"),
                   y = 24.1951, yend = 26.0)) +
  geom_segment(colour = "slateblue1",
               aes(x = as.Date("2018-02-22"), xend = as.Date("2018-02-22"),
                   y = 24.6653, yend = 26.0)) +
  geom_segment(colour = "slateblue1",
               aes(x = as.Date("2018-01-29"), xend = as.Date("2018-02-22"),
                   y = 26.0, yend = 26.0)) +
  geom_label(aes(label = "Duration = 25 days", x = as.Date("2018-02-10"), y = 26.0),
             colour = "slateblue1", label.size = 3) +
  geom_label(aes(label = "Duration = 25 days", x = as.Date("2018-02-10"), y = 26.0),
             colour = "black", label.size = 0) +
  # Other aesthetics
  scale_colour_manual(name = "Line colours", values = lineCol,
                      breaks = c("Temperature", "Climatology", "Threshold",
                                 "2x Threshold", "3x Threshold", "4x Threshold")) +
  scale_fill_manual(name = "Category", values = fillCol, breaks = c("Moderate", "Strong", "Severe", "Extreme")) +
  scale_x_date(expand = c(0, 0), date_labels = "%b %Y", 
               breaks = c(as.Date("2018-02-01"), as.Date("2018-03-01")),
               limits = c(as.Date("2018-01-10"), as.Date("2018-03-15"))) +
  scale_y_continuous(limits = c(18, 28), expand = c(0, 0), breaks = seq(20, 26, by = 2)) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid",
                                                                "dashed", "dotdash", "dotted"),
                                                   size = c(1, 1, 1, 1, 1, 1)))) +
  labs(y = "Temperature (°C)", x = NULL) +
  theme(panel.border = element_rect(colour = "black", fill = NA))
# fig_2
ggsave("figures/fig_2.png", fig_2, width = 12, height = 6)
ggsave("figures/fig_2.pdf", fig_2, width = 12, height = 6)


# Figure 3 ----------------------------------------------------------------

# One of the most widely published MCS is that which occurred off Florida in 2003
FL_bound <- c(26, 36, -84, -72)
FL_data <- load_MCS_ALL(FL_bound)
FL_data_2003 <- extract_MCS_grid_year(FL_data, 2003)
rm(FL_data); gc()

# Taiwan Strait
TS_bound <- c(18, 30, 112, 126)
TS_data <- load_MCS_ALL(TS_bound)
TS_data_2008 <- extract_MCS_grid_year(TS_data, 2008)
rm(TS_data); gc()

# Atlantic Ocean cold blob 2014 - 2016 under Greenland
# NB: This one takes a while and a lot of RAM
AO_bound <- c(43, 65, -50, -7)
AO_data <- load_MCS_ALL(AO_bound)
AO_data_2013_15 <- extract_MCS_grid_year(AO_data, 2013, 2)
rm(AO_data); gc()

# TO DO: Consider searching for the day that has the highest total max intensity pixels
# Consider allowing this function to ingest multiple datasets so they can be plotted together at the same time
# This would then allow binning of the figures by row so that they can share legends

# Function for each panel
# Or rather extract and combine the clim and event data for the three events
# Then pass those to the function below so that one can use facets to do the heavy lifting


# testers...
# date_range <- c("2014-01-01", "2016-12-31")
# intensity_choice <- "cumulative"
Hobday_Fig_3_MCS <- function(MCS_data, date_range, intensity_choice = "max", line_legend = "none"){
  
  # Find the most intense point
  if(intensity_choice == "max"){
    centre_point <- MCS_data$clim_data %>% 
      mutate(anom = temp - seas) %>% 
      filter(t >= date_range[1],
             t <= date_range[2]) %>% 
      filter(anom == min(anom))
    # centre_date <- centre_point$t
  } else if(intensity_choice == "cumulative"){
    centre_point <- MCS_data$event_data %>% 
      filter(date_start >= date_range[1],
             date_end <= date_range[2]) %>% 
      filter(intensity_cumulative == min(intensity_cumulative))
    # centre_date <- centre_point$date_peak
  }
  
  # Find the date range of the event
  centre_dates <- MCS_data$event_data %>% 
    filter(lon == centre_point$lon[1],
           lat == centre_point$lat[1],
           event_no == centre_point$event_no[1])
  
  # Event name
  centre_name <- paste0(lubridate::year(centre_dates$date_peak), " event")
  
  # Extract the top event rows
  mcs_top <- MCS_data$clim_data %>% 
    filter(lon == centre_point$lon[1],
           lat == centre_point$lat[1],
           t >= centre_dates$date_start[1]-1,
           t <= centre_dates$date_end[1]+1)
  
  # Map figure
  mf <- MCS_data$clim_data %>% 
    filter(t == centre_dates$date_peak) %>% 
    mutate(anom = temp - seas) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_tile(aes(fill = anom)) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    geom_label(aes(x = min(lon), y = max(lat), label = centre_dates$date_peak), hjust = 0, vjust = 1, size = 6) +
    geom_point(data = centre_point, aes(x = lon, y = lat), shape = 21, fill = "yellow", size = 3) +
    coord_quickmap(expand = F, xlim = range(MCS_data$clim_data$lon), ylim = range(MCS_data$clim_data$lat)) +
    scale_fill_gradient2(low = "blue", high = "red") +
    labs(x = NULL, y = NULL, fill = "SSTa (°C)") +
    theme(panel.border = element_rect(colour = "black", fill = NA))
  # mf
  
  # Event line figure
  el <- MCS_data$clim_data %>% 
    filter(lon == centre_point$lon[1],
           lat == centre_point$lat[1],
           t >= centre_dates$date_start-30,
           t <= centre_dates$date_end+30) %>% 
    mutate(diff = thresh - seas,
           thresh_2x = thresh + diff,
           thresh_3x = thresh_2x + diff,
           thresh_4x = thresh_3x + diff) %>% 
    ggplot(aes(x = t)) +
    geom_flame(aes(y = thresh, y2 = temp, fill = "Moderate"), n = 5, n_gap = 2) +
    geom_flame(aes(y = thresh_2x, y2 = temp, fill = "Strong")) +
    geom_flame(aes(y = thresh_3x, y2 = temp, fill = "Severe")) +
    geom_flame(aes(y = thresh_4x, y2 = temp, fill = "Extreme")) +
    geom_line(aes(y = thresh_2x, col = "2x Threshold"), size = 0.2, linetype = "dashed") +
    geom_line(aes(y = thresh_3x, col = "3x Threshold"), size = 0.2, linetype = "dotdash") +
    geom_line(aes(y = thresh_4x, col = "4x Threshold"), size = 0.2, linetype = "dotted") +
    geom_line(aes(y = seas, col = "Climatology"), size = 0.6) +
    geom_line(aes(y = thresh, col = "Threshold"), size = 0.6) +
    geom_line(aes(y = temp, col = "Temperature"), size = 0.4) +
    scale_colour_manual(name = "Line colours", values = lineCol,
                        breaks = c("Temperature", "Climatology", "Threshold",
                                   "2x Threshold", "3x Threshold", "4x Threshold")) +
    scale_fill_manual(name = "Category", values = fillCol, breaks = c("Moderate", "Strong", "Severe", "Extreme")) +
    scale_x_date(date_labels = "%b %Y", expand = c(0, 0)) +
    guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid", "dashed", "dotdash", "dotted"),
                                                     size = c(1, 1, 1, 1, 1, 1)))) +
    labs(y = expression(paste("Temperature (°C)")), x = NULL) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          legend.position = line_legend)
  # el
  
  # Lolliplot figures
  ld <- MCS_data$event_data %>% 
    filter(lon == centre_point$lon[1],
           lat == centre_point$lat[1]) %>% 
    ggplot(aes(x = date_peak, y = duration)) +
    geom_lolli(colour = "steelblue3") +
    geom_lolli(data = centre_dates, colour = "navy") +
    labs(x = NULL, y = "Duration (days)", colour = "Events") +
    theme(axis.text.x = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA))
  # ld
  lim <- MCS_data$event_data %>% 
    filter(lon == centre_point$lon[1],
           lat == centre_point$lat[1]) %>% 
    ggplot(aes(x = date_peak, y = intensity_max)) +
    geom_lolli(colour = "steelblue3") +
    geom_lolli(data = centre_dates, colour = "navy") +
    labs(x = NULL, y = "Maximum Intensity (°C)", colour = "Events") +
    theme(axis.text.x = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA))
  # lim
  lic <- MCS_data$event_data %>% 
    filter(lon == centre_point$lon[1],
           lat == centre_point$lat[1]) %>% 
    ggplot(aes(x = date_peak, y = intensity_cumulative)) +
    geom_lolli(colour = "steelblue3") +
    geom_lolli(data = centre_dates, colour = "navy") +
    labs(x = "Peak date", y = "Cumulative Intensity (°C)", colour = "Events") +
    theme(panel.border = element_rect(colour = "black", fill = NA))
  # lic
  
  # Combine and save
  full_fig <- ggarrange(mf, el, ld, lim, lic, ncol = 1, nrow = 5, align = "h", 
                        heights = c(1.2, 0.7, 0.5, 0.5, 0.5))
  return(full_fig)
}

# The 2003 Florida summer event
FL_2003_summer <- Hobday_Fig_3_MCS(FL_data, c("2003-07-01", "2003-7-31"))
# ggsave("output/FL_2003_summer.png", FL_2003_summer, height = 14, width = 5)

# The 2002 winter event
FL_2002_winter <- Hobday_Fig_3_MCS(FL_data, c("2002-09-01", "2003-01-31"))
# ggsave("output/FL_2002_winter.png", FL_2002_winter, height = 14, width = 5)

# The biggest event
FL_max <- Hobday_Fig_3_MCS(FL_data, c("1982-01-01", "2020-12-31"))
# ggsave("output/FL_max.png", FL_max, height = 14, width = 5)

# Combine all three
FL_trio <- ggarrange(FL_2003_summer, FL_2002_winter, FL_max, ncol = 3, nrow = 1)
# ggsave("output/FL_trio.png", FL_trio, height = 14, width = 15)

# Atlantic Ocean cold blob of 2014 - 2016
AO_blob <- Hobday_Fig_3_MCS(AO_data, c("2014-01-01", "2016-12-31"), intensity_choice = "cumulative", line_legend = "right")
# ggsave("output/AO_blob.png", AO_blob, height = 14, width = 5)

# Australia event
OZ_reef <- Hobday_Fig_3_MCS(OZ_data, c("2003-01-01", "2003-12-31"), intensity_choice = "cumulative")
# ggsave("output/OZ_reef.png", OZ_reef, height = 14, width = 5)

# California current
CC_coast <- Hobday_Fig_3_MCS(CC_data, c("2003-01-01", "2003-12-31"), intensity_choice = "max")
# ggsave("output/CC_coast.png", CC_coast, height = 14, width = 5)

# Taiwan Strait
TS_coast <- Hobday_Fig_3_MCS(TS_data, c("2007-01-01", "2008-12-31"), intensity_choice = "max")
# ggsave("output/TS_coast.png", TS_coast, height = 14, width = 5)

# Combine the three notorious MCS multi-panel figures
fig_3 <- ggarrange(FL_2003_summer, TS_coast, AO_blob, ncol = 3, nrow = 1, labels = c("A)", "B)", "C)"))
ggsave("graph/MCS/fig_3.png", fig_3, height = 14, width = 15)
ggsave("graph/MCS/fig_3.pdf", fig_3, height = 14, width = 15)


# Figure 4 ----------------------------------------------------------------
# Maps of the mean metrics

# Load all results into one brick
MCS_count_trend <- readRDS("data/MCS_count_trend.Rds")
unique(MCS_count_trend$name)

# Only the significant values
MCS_sig <- MCS_count_trend %>% 
  filter(p.value <= 0.05)

# Figures of trends and annual states
fig_4_func <- function(var_name, legend_title, mean_plot = T){
  
  # Determine which column to plot
  if(mean_plot){
    type_filter <- "value"
  } else{
    type_filter <- "slope"
  }
  
  # Basic filter
  df <- MCS_count_trend %>% 
    filter(name == var_name,
           lat >= -70, lat <= 70) %>% 
    pivot_longer(cols = c(value, slope), 
                 names_to = "type", values_to = "val") %>% 
    filter(type == type_filter)
  
  # Significant results
  df_p <- df %>% 
    filter(p.value <= 0.05)
  
  # Find quantiles to round off tails for plotting
  q05 <- quantile(df$val, 0.05, names = F)
  q10 <- quantile(df$val, 0.1, names = F)
  q50 <- quantile(df$val, 0.5, names = F)
  q90 <- quantile(df$val, 0.9, names = F)
  q95 <- quantile(df$val, 0.95, names = F)
  
  # Determine colour palette
  if(var_name == "total_count") {
    viridis_choice <- "A"
    vir_dir <- 1
    slope_low <- "plum"
    slope_high <- "gold"
    rn <- 1
    if(!mean_plot) rn <- 2
  } else if(var_name == "dur_mean") {
    viridis_choice <- "C"
    vir_dir <- 1
    slope_low <- "orange"
    slope_high <- "olivedrab"
    rn <- 1
  } else if(var_name == "i_max_mean") {
    viridis_choice <- "B"
    vir_dir <- -1
    slope_low <- "blue"
    slope_high <- "red"
    rn <- 1
    if(!mean_plot) rn <- 2
  }  else {
    viridis_choice <- "D"
    vir_dir <- -1
    slope_low <- "blue"
    slope_high <- "red"
    rn <- 1
  }
  
  # The figure without colour palette
  map_res <- df %>% 
    mutate(val = case_when(val <= q05 ~ q05,
                           val >= q95 ~ q95,
                           TRUE ~ val)) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_raster(aes(fill = val)) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    # scale_fill_viridis_c(option = viridis_choice, direction = vir_dir) +
    coord_quickmap(expand = F, ylim = c(-70, 70)) +
    labs(x = NULL, y = NULL, fill = legend_title) +
    guides(fill = guide_colourbar(barwidth = grid::unit(3, units = "inches"))) +
    # theme_void() +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          legend.position = "top",
          legend.title = element_text(size = 14, vjust = 1),
          legend.text = element_text(size = 12),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  # Add the colour palette
  if(mean_plot){
    map_res <- map_res +
      scale_fill_viridis_c(option = viridis_choice, direction = vir_dir, 
                           breaks = c(q05, q50, q95), 
                           labels = c(paste0("  <",round(q05, rn)), round(q50, rn), paste0(round(q95, rn),">")))
  } else{
    map_res <- map_res +
      scale_fill_gradient2(low = slope_low, high = slope_high,
                           breaks = c(q05, q50, q95), 
                           labels = c(paste0("  <",round(q05, rn)), round(q50, rn), paste0(round(q95, rn),">")))
  }
  map_res
}

# Create panels
fig_4a <- fig_4_func("total_count", "Count (n)")
fig_4b <- fig_4_func("dur_mean", "Duration\n(days)")
fig_4c <- fig_4_func("i_max_mean", "Maximum\nintensity (°C)")
fig_4d <- fig_4_func("i_cum_mean", "Cumulative\nintensity (°C days)")

# Combine and save
fig_4 <- ggpubr::ggarrange(fig_4a, fig_4b, fig_4c, fig_4d, ncol = 2, nrow = 2, 
                           align = "hv", labels = c("A)", "B)", "C)", "D)"))
ggsave("figures/fig_4.png", fig_4, height = 6, width = 11)
ggsave("figures/fig_4.pdf", fig_4, height = 6, width = 11)


# Figure 5 ----------------------------------------------------------------
# Grouped global trends in MCS metrics

# Load annual data
MCS_annual_mean <- read_rds("data/MCS_annual_mean.Rds")

# Join with ice data and create global means
MCS_annual_global_mean <- MCS_annual_mean %>% 
  left_join(lon_lat_OISST_ice, by = c("lon", "lat")) %>% 
  mutate(ice_group = case_when(ice & lat <= 0 ~ "ice S",
                               ice & lat > 0 ~ "ice N",
                               TRUE ~ "ocean")) %>% 
  group_by(year, ice_group) %>% 
  summarise_all("mean", .groups = "drop") %>% 
  dplyr::select(-ice, -lon, -lat) %>% 
  group_by(ice_group) %>% 
  mutate(temp_anom = temp_annual - mean(temp_annual, na.rm = T)) %>% 
  ungroup()

# Long data for analysis
MCS_annual_global_mean_long <- MCS_annual_global_mean %>% 
  pivot_longer(cols = count_annual:temp_anom)

# Trends
MCS_annual_global_mean_trends <- MCS_annual_global_mean_long %>% 
  group_by(ice_group, name) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(value ~ year, data = .)),
         model_out = map(model, ~broom::tidy(.))) %>% 
  dplyr::select(-data, -model) %>% 
  unnest(cols = model_out) %>% 
  ungroup() %>% 
  filter(term == "year") %>% 
  dplyr::rename(slope = estimate) %>% 
  dplyr::select(ice_group, name, slope, p.value) %>% 
  mutate(slope = round(slope, 4), 
         p.value = round(p.value, 4))

# ANOVA
MCS_annual_global_mean_aov <- MCS_annual_global_mean_long %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(model = map(data, ~aov(value ~ ice_group, data = .)),
         model_out = map(model, ~broom::tidy(.))) %>% 
  dplyr::select(-data, -model) %>% 
  unnest(cols = model_out) %>% 
  ungroup() %>% 
  filter(term == "ice_group") %>% 
  dplyr::select(name, term, df, p.value) %>% 
  mutate(p.value = round(p.value, 4))

# Function for creating line plots
fig_5_func <- function(var_name, y_title, y_val){
  
  # Subset slope and ANOVA results
  sub_slope <- MCS_annual_global_mean_trends %>% 
    filter(name == var_name) %>% 
    mutate(ice_group = ifelse(ice_group == "ocean", "open", ice_group),
           slope = paste0("m = ",round(slope, 3)),
           p.value = case_when(p.value < 0.01 ~ "p < 0.01",
                               p.value >= 0.01 ~ paste0("p = ",round(p.value,2))),
           label = paste0(slope,"; ",p.value))
  sub_ANOVA <- MCS_annual_global_mean_aov %>% 
    filter(name == var_name) %>% 
    mutate(p.value = case_when(p.value < 0.01 ~ "p < 0.01",
                               p.value >= 0.01 ~ paste0("p = ",round(p.value,2))),
           label = paste0("ANOVA: ",p.value))
  
  # Plot the chosen variable
  MCS_annual_global_mean_long %>% 
    filter(name == var_name) %>% 
    mutate(ice_group = ifelse(ice_group == "ocean", "open", ice_group)) %>% 
    ggplot(aes(x = year, y = value)) +
    # Add lines, points, and lm
    geom_line(aes(colour = ice_group), show.legend = F) +
    geom_point(aes(colour = ice_group)) +
    geom_smooth(aes(colour = ice_group), show.legend = F, method = "lm", formula = "y ~ x", size = 2) +
    # Add slope labels
    geom_label(data = sub_slope, label.size = 3, show.legend = F,
               aes(x = c(1990, 2000, 2010), y = y_val[1], label = label, colour = ice_group)) +
    geom_label(data = sub_slope, label.size = 0,
               aes(x = c(1990, 2000, 2010), y = y_val[1], label = label)) +
    # Add ANOVA label
    geom_label(data = sub_ANOVA, label.size = 3, show.legend = F,
               aes(x = 2000, y = y_val[2], label = label), colour = "darkorchid") +
    geom_label(data = sub_ANOVA, label.size = 0,
               aes(x = 2000, y = y_val[2], label = label)) +
    # Other
    scale_x_continuous(breaks = seq(1982, 2020, 5), expand = c(0, 0)) +
    scale_color_brewer(palette = "Paired") +
    labs(y = y_title, x = NULL, colour = "Ocean group") +
    guides(colour = guide_legend(override.aes = list(shape = 15, size = 10))) +
    # coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 16))
  # facet_wrap(~name, scales = "free_y")
}

# Create the panels
fig_5a <- fig_5_func("count_annual", "Count (n)", c(3.35, 0.45))
fig_5b <- fig_5_func("duration", "Duration\n(days)", c(62, -5))
fig_5c <- fig_5_func("intensity_max", "Maximum\nintensity (°C)", c(-0.31, -1.57))
fig_5d <- fig_5_func("intensity_cumulative", "Cumulative\nintensity (°C days)", c(0, -30))
fig_5e <- fig_5_func("temp_anom", "Temperature\nanomaly (°C)", c(0.335, -0.32))

# Combine and save
fig_5 <- ggpubr::ggarrange(fig_5a, fig_5b, fig_5c, fig_5d, fig_5e, ncol = 1, nrow = 5, 
                           align = "hv", labels = c("A)", "B)", "C)", "D)", "E)"), common.legend = T)
ggsave("figures/fig_5.png", fig_5, height = 11, width = 7)
ggsave("figures/fig_5.pdf", fig_5, height = 11, width = 7)


# Figure 6 ----------------------------------------------------------------
# Maps of the trends in the metrics
# NB: This requires functions from Figure 4 code section

# Crate panels
fig_6a <- fig_4_func("total_count", "Count (n)", mean_plot = F)
fig_6b <- fig_4_func("dur_mean", "Duration\n(days)", mean_plot = F)
fig_6c <- fig_4_func("i_max_mean", "Maximum\nintensity (°C)", mean_plot = F)
fig_6d <- fig_4_func("i_cum_mean", "Cumulative\nintensity (°C days)", mean_plot = F)

# Combine and save
fig_6 <- ggpubr::ggarrange(fig_6a, fig_6b, fig_6c, fig_6d, ncol = 2, nrow = 2, 
                           align = "hv", labels = c("A)", "B)", "C)", "D)"))
ggsave("figures/fig_6.png", fig_6, height = 6, width = 11)
ggsave("figures/fig_6.pdf", fig_6, height = 6, width = 11)


# Figure 7 ----------------------------------------------------------------
# Comparison of SSTa skewness and MHW vs. MCS intensity

# Load the MCS vs. MHW results
MHW_v_MCS <- readRDS("data/MHW_v_MCS.Rds")

# Melt long for easier plotting
MHW_v_MCS_long <- MHW_v_MCS %>% 
  pivot_longer(cols = count:i_cum, names_to = "name", values_to = "value") %>% 
  na.omit()

# Figure for plotting the panels
fig_7_func <- function(var_name, rn = 2){
  
  # Basic filter
  df <- MHW_v_MCS_long %>% 
    filter(name == var_name,
           lat >= -70, lat <= 70)
  
  # Find 10th and 90th quantiles to round off tails for plotting
  q05 <- quantile(df$value, 0.05, names = F)
  q10 <- quantile(df$value, 0.1, names = F)
  q50 <- quantile(df$value, 0.5, names = F)
  q90 <- quantile(df$value, 0.9, names = F)
  q95 <- quantile(df$value, 0.95, names = F)
  
  # Figure
  df %>% 
    mutate(value = case_when(value <= q05 ~ q05,
                             value >= q95 ~ q95,
                             TRUE ~ value)) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_tile(aes(fill = value)) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    coord_quickmap(expand = F, ylim = c(-70, 70)) +
    scale_fill_gradient2(low = "blue", mid = "grey", high = "red",
                         breaks = c(q05, q50, q95), 
                         labels = c(paste0("  <",round(q05, rn)), round(q50, rn), paste0(round(q95, rn),">")),) +
    guides(fill = guide_colourbar(barwidth = grid::unit(3, units = "inches"))) +
    labs(x = NULL, y = NULL, fill = var_name) +
    # theme_void() +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          legend.position = "top",
          legend.title = element_text(size = 14, vjust = 1),
          legend.text = element_text(size = 12),
          axis.text = element_blank(),
          axis.ticks = element_blank())
}

# Plot a metric
fig_7a <- fig_7_func("i_max") +
  labs(fill = "Max. intensity (°C)")

# Prep SSTa stats
SSTa_stats <- readRDS("data/SSTa_stats.Rds") %>% 
  dplyr::select(lon:anom_kurt) %>% 
  pivot_longer(c(anom_kurt, anom_skew)) %>% 
  mutate(name = case_when(name == "anom_kurt" ~ "kurtosis",
                          name == "anom_skew" ~ "skewness")) %>% 
  filter(lat >= -70, lat <= 70)

# Prep data for plotting
# SSTa_prep <- SSTa_stats %>% 
#   filter(season == "Total") %>% 
#   pivot_wider(names_from = "name", values_from = "value") %>% 
#   left_join(MHW_v_MCS) %>% 
#   na.omit()

# Find upper skewness and kurtosis quantiles
skew_quants <- SSTa_stats %>% 
  filter(name == "skewness", season == "Total") %>% 
  summarise(q05 = quantile(value, 0.05, names = F),
            q10 = quantile(value, 0.1, names = F),
            q50 = quantile(value, 0.5, names = F),
            q90 = quantile(value, 0.9, names = F),
            q95 = quantile(value, 0.95, names = F))

# Map of skewness per pixel
fig_7b <- SSTa_stats %>% 
  filter(name == "skewness", season == "Total") %>% 
  mutate(value = case_when(value <= skew_quants$q05 ~ skew_quants$q05,
                           value >= skew_quants$q95 ~ skew_quants$q95,
                           TRUE ~ value)) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = value)) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
  coord_quickmap(expand = F, ylim = c(-70, 70)) +
  scale_fill_gradient2("Skewness", low = pal_jco()(3)[1], mid = pal_jco()(3)[3], high = pal_jco()(3)[2],
                       breaks = c(skew_quants$q05, skew_quants$q50, skew_quants$q95), 
                       labels = c(paste0("  <",round(skew_quants$q05, 1)), 
                                  round(skew_quants$q50, 1), 
                                  paste0(round(skew_quants$q95, 1),">"))) +
  guides(fill = guide_colourbar(barwidth = grid::unit(3, units = "inches"))) +
  labs(x = NULL, y = NULL) +
  # theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "top",
        legend.title = element_text(size = 14, vjust = 1),
        legend.text = element_text(size = 12),
        axis.text = element_blank(),
        axis.ticks = element_blank())
# fig_7b

fig_7 <- ggpubr::ggarrange(fig_7a, fig_7b, ncol = 2, nrow = 1, labels = c("A)", "B)"))
ggsave("figures/fig_7.png", fig_7, height = 3, width = 11)
ggsave("figures/fig_7.pdf", fig_7, height = 3, width = 11)


# Figure 8 ----------------------------------------------------------------
# Global annual summaries of MCSs

# TO DO: Add bars with black border showing the Southern Ocean contribution

# Load data
MCS_total <- readRDS("data/MCS_cat_daily_total.Rds")

# Chose category system
MCS_total_filter <- filter(MCS_total, name == "category") %>% 
  dplyr::select(-hemi) %>% 
  group_by(t, name, category) %>% 
  summarise_all(sum) %>% 
  filter(first_n_cum > 0)

# Get Ice only for overplotting
MCS_total_ice <- filter(MCS_total, name == "category_ice") %>% 
  filter(category == "V Ice")

# Stacked barplot of global daily count of MHWs by category
fig_count_historic <- ggplot(MCS_total_filter, aes(x = t, y = cat_area_prop_mean)) +
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
fig_cum_historic <- ggplot(MCS_total_filter, aes(x = t, y = first_area_cum_prop)) +
  geom_bar(aes(fill = category), stat = "identity", show.legend = T,
           position = position_stack(reverse = TRUE), width = 1) +
  geom_bar_pattern(data = MCS_total_ice, stat = "identity", show.legend = F,
                   aes(pattern_colour = hemi, colour = hemi), 
                   pattern = "stripe", pattern_fill = NA, fill = NA, 
                   pattern_density = 1, pattern_size = 1.5) +
  scale_fill_manual("Category", values = MCS_colours) +
  scale_colour_manual(values = c("lightpink", "plum")) +
  scale_pattern_colour_manual(values = c("lightpink", "plum")) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0.2, 0.8, length.out = 4),
                     labels = paste0(seq(20, 80, by = 20), "%")) +
  scale_x_continuous(breaks = seq(1982, 2019, 5)) +
  # guides(pattern_fill = F, pattern_colour = F, hemi = F) +
  labs(y = "Total ocean experienceing \nat least one MCS", x = NULL) +
  coord_cartesian(expand = F) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))
# fig_cum_historic

# Stacked barplot of average cumulative MHW days per pixel
fig_prop_historic <- ggplot(MCS_total_filter, aes(x = t, y = cat_area_cum_prop)) +
  geom_bar(aes(fill = category), stat = "identity", show.legend = T,
           position = position_stack(reverse = TRUE), width = 1) +
  geom_bar_pattern(data = MCS_total_ice, stat = "identity", show.legend = F,
                   aes(pattern_colour = hemi, colour = hemi), 
                   pattern = "stripe", pattern_fill = NA, fill = NA, 
                   pattern_density = 1, pattern_size = 1.5) +
  scale_fill_manual("Category", values = MCS_colours) +
  scale_colour_manual(values = c("lightpink", "plum")) +
  scale_pattern_colour_manual(values = c("lightpink", "plum")) +
  scale_y_continuous(limits = c(0, 30),
                     breaks = seq(10, 20, length.out = 2)) +
  scale_x_continuous(breaks = seq(1982, 2019, 5)) +
  labs(y = "Total MCS days for ocean", x = NULL) +
  coord_cartesian(expand = F) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none",
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16))
# fig_prop_historic

# Create the figure title
product_title <- "NOAA OISST"
min_year <- min(MCS_total_filter$t)
max_year <- max(MCS_total_filter$t)
fig_title <- paste0("MCS category summaries: ",min_year,"-",max_year,
                    "\n",product_title,"; Climatogy period: 1982-2011")

# Stick them together and save
fig_8 <- ggpubr::ggarrange(fig_count_historic, fig_cum_historic, fig_prop_historic,
                           ncol = 3, align = "hv", labels = c("A)", "B)", "C)"), hjust = -0.1,
                           font.label = list(size = 14), common.legend = T, legend = "bottom")
ggsave(fig_8, filename = paste0("figures/fig_8.png"), height = 4.25, width = 12)
ggsave(fig_8, filename = paste0("figures/fig_8.pdf"), height = 4.25, width = 12)

# Summed up annual values for easier reading
sum_all <- MCS_total_filter %>% 
  group_by(t) %>% 
  summarise_if(is.numeric, sum)

# Summed up annual ice values for easier reading
sum_ice <- MCS_total_ice %>% 
  group_by(t) %>% 
  summarise_if(is.numeric, sum)

# Values minus ice
sum_all$first_area_cum_prop - sum_ice$first_area_cum_prop
sum_all$cat_area_cum_prop - sum_ice$cat_area_cum_prop

# Linear models
x <- 1:nrow(sum_all)
y1 <- sum_all$first_area_cum_prop - sum_ice$first_area_cum_prop
y2 <- c(sum_all$cat_area_cum_prop - sum_ice$cat_area_cum_prop)
summary(lm(formula = y1 ~ x))
summary(lm(formula = y2 ~ x))


# Figure 9 ----------------------------------------------------------------

# Could be interesting to show a global time series of the difference between average MCS and MHW days over the whole ocean.

