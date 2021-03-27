# MCS_figures.R
# This script houses the code used to make the figures for the MCS manuscript


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
source("code/functions.R")
library(ggpattern)
# library(ggsci) # Scientific colour palettes


# Figure 1 ----------------------------------------------------------------

# Move duration label up
# Remove cum. int. crescent line
# Spread out colour palette to give better contrast four cat 4
# Make clim lines bigger

# Find a pixel that naturally experienced a Cat 4 event
AC_bound <- c(-35, 4-5, 20, 35)
AC_data <- load_MCS_ALL(AC_bound); gc()

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
fig_1 <- ggplot(data = AC_data_clim_sub, aes(x = t)) +
  geom_flame(aes(y = thresh, y2 = temp, fill = "Moderate"), n = 5, n_gap = 2) +
  geom_flame(aes(y = thresh_2x, y2 = temp, fill = "Strong")) +
  geom_flame(aes(y = thresh_3x, y2 = temp, fill = "Severe")) +
  geom_flame(aes(y = thresh_4x, y2 = temp, fill = "Extreme")) +
  geom_ribbon_pattern(data = AC_data_clim_sub_sub, aes(ymin = seas, ymax = temp), 
                      pattern_fill = "steelblue1",
                      pattern = 'stripe', fill = NA, colour  = 'black', alpha = 0.3) +
  geom_line(aes(y = thresh_2x, col = "2x Threshold"), size = 0.7, linetype = "dashed") +
  geom_line(aes(y = thresh_3x, col = "3x Threshold"), size = 0.7, linetype = "dotdash") +
  geom_line(aes(y = thresh_4x, col = "4x Threshold"), size = 0.7, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 1.0) +
  geom_line(aes(y = thresh, col = "Threshold"), size = 1.0) +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.7) +
  # Cumulative intensity label
  geom_label(aes(x = as.Date("2018-02-26"), y = 22.0),
             label = expression(italic("i"[cum])*" = -70.04"*degree*"C days"),
             colour = "steelblue1", label.size = 2) +
  geom_label(aes(x = as.Date("2018-02-26"), y = 22.0),
             label = expression(italic("i"[cum])*" = -70.04"*degree*"C days"),
             colour = "black", label.size = 0) +
  # Max intensity label
  geom_segment(colour = "midnightblue",
               aes(x = as.Date("2018-02-10"), xend = as.Date("2018-02-10"),
                   y = 25.6323, yend = 19.0)) +
  geom_label(aes(x = as.Date("2018-02-10"), y = 18.8), # label = "Max. Intensity = -6.24°C", ),
             label = expression(italic("i"[max])*" = -6.24"*degree*"C"),
             colour = "midnightblue", label.size = 2) +
  geom_label(aes(x = as.Date("2018-02-10"), y = 18.8),
             label = expression(italic("i"[max])*" = -6.24"*degree*"C"),
             colour = "black", label.size = 0) +
  # Duration label
  geom_segment(colour = "slateblue1",
               aes(x = as.Date("2018-01-29"), xend = as.Date("2018-01-29"),
                   y = 24.1951, yend = 26.5)) +
  geom_segment(colour = "slateblue1",
               aes(x = as.Date("2018-02-22"), xend = as.Date("2018-02-22"),
                   y = 24.6653, yend = 26.5)) +
  geom_segment(colour = "slateblue1",
               aes(x = as.Date("2018-01-29"), xend = as.Date("2018-02-22"),
                   y = 26.5, yend = 26.5)) +
  geom_label(aes(x = as.Date("2018-02-10"), y = 26.5),
             label = expression(italic("D")*" = 25 days"),
             colour = "slateblue1", label.size = 2) +
  geom_label(aes(x = as.Date("2018-02-10"), y = 26.5),
             label = expression(italic("D")*" = 25 days"),
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
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
fig_1
ggsave("figures/fig_1.png", fig_1, width = 8, height = 4)
ggsave("figures/fig_1.pdf", fig_1, width = 8, height = 4)


# Figure 2 ----------------------------------------------------------------

# Add a sentence about Penghu surface mismatch

# Function that prepares category grid for plotting
# MCS_data <- FL_data
# intensity_choice = "max"
# date_range <- c("2003-05-01", "2003-10-31")
main_event <- function(MCS_data, date_range, intensity_choice = "cumulative"){
  
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
      mutate(lon_centre = centre_point$lon,
             lat_centre = centre_point$lat,
             event_no_centre = centre_point$event_no,
             date_start = centre_point$date_start,
             date_peak = centre_point$date_peak,
             date_end = centre_point$date_end)
  )
  return(res)
}

# Test plot to check extracted data
fig_2_test_plot <- function(event_sub){
  event_sub %>% 
    # filter(t == event_sub$date_peak) %>% 
    na.omit() %>% 
    ggplot(aes(x = lon, y = lat)) +
    # geom_raster(aes(fill = intensity))
    geom_raster(aes(fill = category)) +
    geom_point(aes(x = lon_centre, y = lat_centre), shape = 21, fill = "yellow", size = 3) +
    scale_fill_manual(values = MCS_colours)
}

# One of the most widely published MCS is that which occurred off Florida in 2003
FL_bound <- c(26, 34, -84, -74)
FL_data <- load_MCS_ALL(FL_bound, c("2003-01-01", "2003-12-31")); gc()
FL_event_2003 <- main_event(FL_data, c("2003-05-01", "2003-10-31"), "max")
fig_2_test_plot(FL_event_2003)
FL_pixel <- load_MCS_ALL(c(FL_event_2003$lat_centre[1], FL_event_2003$lat_centre[1],
                           FL_event_2003$lon_centre[1], FL_event_2003$lon_centre[1]))

# Taiwan Strait
TS_bound <- c(20, 28, 115, 124)
TS_data <- load_MCS_ALL(TS_bound, c("2007-06-01", "2008-05-31")); gc()
TS_event_2008 <- main_event(TS_data, c("2007-11-01", "2008-03-31"), "max")
fig_2_test_plot(TS_event_2008)
TS_pixel <- load_MCS_ALL(c(TS_event_2008$lat_centre[1], TS_event_2008$lat_centre[1],
                           TS_event_2008$lon_centre[1], TS_event_2008$lon_centre[1]))

# Atlantic Ocean cold blob 2014 - 2016 under Greenland
AO_bound <- c(40, 65, -40, -5)
AO_data <- load_MCS_ALL(AO_bound, c("2012-07-01", "2017-06-30")); gc()
AO_event_2013_16 <- main_event(AO_data, c("2013-01-01", "2016-12-31"), "cumulative")
fig_2_test_plot(AO_event_2013_16)
AO_pixel <- load_MCS_ALL(c(AO_event_2013_16$lat_centre[1], AO_event_2013_16$lat_centre[1],
                           AO_event_2013_16$lon_centre[1], AO_event_2013_16$lon_centre[1]))

# Map panel for figure 2
fig_2_panel_1 <- function(event_sub, lon_breaks, lon_label, lat_breaks, lat_label){
  mf <- event_sub %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_tile(aes(fill = category), show.legend = F) +
    geom_polygon(data = map_base, aes(x = lon, y = lat, group = group)) +
    geom_point(data = event_sub, aes(x = lon_centre, y = lat_centre), shape = 21, fill = "yellow", size = 3) +
    coord_quickmap(expand = F, xlim = range(event_sub$lon), ylim = range(event_sub$lat)) +
    scale_x_continuous(breaks = lon_breaks, labels = paste0(abs(lon_breaks),lon_label)) +
    scale_y_continuous(breaks = lat_breaks, labels = paste0(abs(lat_breaks),lat_label)) +
    scale_fill_manual(values = MCS_colours) +
    # guides(fill = guide_legend(nrow = 1, byrow = TRUE, label.position = "bottom")) +
    labs(x = NULL, y = NULL, fill = "Category") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  # mf
}
panel_1_FL <- fig_2_panel_1(FL_event_2003, seq(-82, -76, 2), "°W", seq(28, 32, 2), "°N")
panel_1_TS <- fig_2_panel_1(TS_event_2008, seq(117, 123, 2), "°E", seq(22, 26, 2), "°N")
panel_1_AO <- fig_2_panel_1(AO_event_2013_16, seq(-35, -15, 10), "°W", seq(45, 60, 5), "°N")
panel_1 <- ggpubr::ggarrange(panel_1_FL, panel_1_TS, panel_1_AO, align = "hv", labels = c("A)", "B)", "C)"),
                             common.legend = T, legend = "bottom", ncol = 3, nrow = 1)
# panel_1

# The line plot of the event
fig_2_panel_2 <- function(MCS_data, event_sub, y_label, ts_spread = 30){
  if(ts_spread < 30){
    x_format <- "%d %b %Y"
  } else{
    x_format <- "%b %Y"
  }
  el <- MCS_data$clim_data %>% 
    filter(lon == event_sub$lon_centre[1],
           lat == event_sub$lat_centre[1],
           t >= event_sub$date_start[1]-ts_spread,
           t <= event_sub$date_end[1]+ts_spread) %>% 
    mutate(diff = thresh - seas,
           thresh_2x = thresh + diff,
           thresh_3x = thresh_2x + diff,
           thresh_4x = thresh_3x + diff) %>% 
    ggplot(aes(x = t)) +
    geom_flame(aes(y = thresh, y2 = temp, fill = "Moderate"), n = 5, n_gap = 2) +
    geom_flame(aes(y = thresh_2x, y2 = temp, fill = "Strong")) +
    geom_flame(aes(y = thresh_3x, y2 = temp, fill = "Severe")) +
    geom_flame(aes(y = thresh_4x, y2 = temp, fill = "Extreme")) +
    geom_line(aes(y = thresh_2x, col = "2x Threshold"), size = 0.7, linetype = "dashed") +
    geom_line(aes(y = thresh_3x, col = "3x Threshold"), size = 0.7, linetype = "dotdash") +
    geom_line(aes(y = thresh_4x, col = "4x Threshold"), size = 0.7, linetype = "dotted") +
    geom_line(aes(y = seas, col = "Climatology"), size = 0.9) +
    geom_line(aes(y = thresh, col = "Threshold"), size = 0.9) +
    geom_line(aes(y = temp, col = "Temperature"), size = 0.6) +
    scale_colour_manual(name = "Line colours", values = lineCol,
                        breaks = c("Temperature", "Climatology", "Threshold",
                                   "2x Threshold", "3x Threshold", "4x Threshold")) +
    scale_fill_manual(name = "Category", values = fillCol, breaks = c("Moderate", "Strong", "Severe", "Extreme")) +
    scale_x_date(date_labels = x_format, expand = c(0, 0)) +
    guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid", "dashed", "dotdash", "dotted"),
                                                     size = c(1, 1, 1, 1, 1, 1)))) +
    labs(y = y_label, x = NULL) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  # el
}
panel_2_FL <- fig_2_panel_2(FL_data, FL_event_2003, "Temp. (°C)")
panel_2_TS <- fig_2_panel_2(TS_data, TS_event_2008, NULL, 10)
panel_2_AO <- fig_2_panel_2(AO_data, AO_event_2013_16, NULL)
panel_2 <- ggpubr::ggarrange(panel_2_FL, panel_2_TS, panel_2_AO, align = "hv", 
                             common.legend = T, legend = "top", ncol = 3, nrow = 1)
# panel_2

# Lolliplots of duration
fig_2_panel_3 <- function(MCS_pixel, event_sub, y_label){
  pixel_sub <- MCS_pixel$event_data %>%
    filter(lon == event_sub$lon_centre[1],
           lat == event_sub$lat_centre[1],
           event_no == event_sub$event_no_centre[1])
  ld <- MCS_pixel$event_data %>% 
    ggplot(aes(x = date_peak, y = duration)) +
    geom_lolli(colour = "steelblue3") +
    geom_lolli(data = pixel_sub, colour = "navy") +
    scale_y_continuous(limits = c(0, max(MCS_pixel$event_data$duration)*1.1), expand = c(0,0)) +
    labs(x = NULL, y = y_label, colour = "Events") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  # ld
}
panel_3_FL <- fig_2_panel_3(FL_pixel, FL_event_2003, expression(italic("D")*" (days)"))
panel_3_TS <- fig_2_panel_3(TS_pixel, TS_event_2008, NULL)
panel_3_AO <- fig_2_panel_3(AO_pixel, AO_event_2013_16, NULL)
# panel_3 <- ggpubr::ggarrange(panel_3_FL, panel_3_TS, panel_3_AO, align = "hv", 
#                              common.legend = T, legend = "bottom", ncol = 3, nrow = 1)
# panel_3

# Lolliplots of max intensity
fig_2_panel_4 <- function(MCS_pixel, event_sub, y_label){
  pixel_sub <- MCS_pixel$event_data %>%
    filter(lon == event_sub$lon_centre[1],
           lat == event_sub$lat_centre[1],
           event_no == event_sub$event_no_centre[1])
  lim <- MCS_pixel$event_data %>% 
    ggplot(aes(x = date_peak, y = intensity_max)) +
    geom_lolli(colour = "steelblue3") +
    geom_lolli(data = pixel_sub, colour = "navy") +
    scale_y_continuous(limits = c(min(MCS_pixel$event_data$intensity_max, na.rm = T)*1.1, 0), expand = c(0,0)) +
    labs(x = NULL, y = y_label, colour = "Events") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_text(face = "italic"),
          panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  # lim
}
panel_4_FL <- fig_2_panel_4(FL_pixel, FL_event_2003, expression(italic("i"[max])*" ("*degree*"C)"))
panel_4_TS <- fig_2_panel_4(TS_pixel, TS_event_2008, NULL)
panel_4_AO <- fig_2_panel_4(AO_pixel, AO_event_2013_16, NULL)
# panel_4 <- ggpubr::ggarrange(panel_4_FL, panel_4_TS, panel_4_AO, align = "hv", 
#                              common.legend = T, legend = "bottom", ncol = 3, nrow = 1)
# panel_4

# Lolliplots of max intensity
fig_2_panel_5 <- function(MCS_pixel, event_sub, y_label){
  pixel_sub <- MCS_pixel$event_data %>%
    filter(lon == event_sub$lon_centre[1],
           lat == event_sub$lat_centre[1],
           event_no == event_sub$event_no_centre[1])
  lic <- MCS_pixel$event_data %>%  
    ggplot(aes(x = date_peak, y = intensity_cumulative)) +
    geom_lolli(colour = "steelblue3") +
    geom_lolli(data = pixel_sub, colour = "navy") +
    scale_y_continuous(limits = c(min(MCS_pixel$event_data$intensity_cumulative)*1.1, 0), expand = c(0,0)) +
    labs(x = NULL, y = y_label, colour = "Events") +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  # lic
}
panel_5_FL <- fig_2_panel_5(FL_pixel, FL_event_2003, expression(italic("i"[cum])*" ("*degree*"C days)"))
panel_5_TS <- fig_2_panel_5(TS_pixel, TS_event_2008, NULL)
panel_5_AO <- fig_2_panel_5(AO_pixel, AO_event_2013_16, NULL)
# panel_5 <- ggpubr::ggarrange(panel_5_FL, panel_5_TS, panel_5_AO, align = "hv", 
#                              common.legend = T, legend = "bottom", ncol = 3, nrow = 1)
# panel_5

# COmbine all lollis
panel_lollis <- ggpubr::ggarrange(panel_3_FL, panel_3_TS, panel_3_AO,
                                  panel_4_FL, panel_4_TS, panel_4_AO,
                                  panel_5_FL, panel_5_TS, panel_5_AO, 
                                  align = "hv", ncol = 3, nrow = 3)

# Combine the three notorious MCS multi-panel figures
fig_2 <- ggpubr::ggarrange(panel_1, panel_2, panel_lollis, heights = c(1.2, 0.7, 1.5),
                           ncol = 1, nrow = 3, align = "hv")
ggsave("figures/fig_2.png", fig_2, height = 14, width = 15)
ggsave("figures/fig_2.pdf", fig_2, height = 14, width = 15)
# rm(FL_data, TS_data, AO_data); gc()


# Figure 3 ----------------------------------------------------------------
# Maps of the mean metrics


# Change colour bars to discrete steps
# Change land to grey
# Use blue and yellow for the contrasting changes in Metrics
# Or light yellow to dark blue


# Load all results into one brick
MCS_count_trend <- readRDS("data/MCS_count_trend.Rds")
unique(MCS_count_trend$name)

# Only the significant values
# Filter down pixels for better plotting
MCS_sig <- MCS_count_trend %>% 
  filter(p.value <= 0.05,
         lon %in% seq(-179.875, 179.875, 1),
         lat %in% seq(-78.375, 89.375, 1))

# Figures of trends and annual statesx
fig_map_func <- function(var_name, legend_title, mean_plot = T){
  
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
    filter(p.value <= 0.05,
           lon %in% seq(-179.875, 179.875, 2),
           lat %in% seq(-78.375, 89.375, 2))
  
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
    # col_pal <- viridis::magma(n = 8, direction = 1)
    slope_low <- "lightgoldenrod"
    slope_high <- "cadetblue4"
    rn <- 1
    mid_q <- q05
    if(!mean_plot) rn <- 2
  } else if(var_name == "dur_mean") {
    viridis_choice <- "C"
    vir_dir <- 1
    slope_low <- "lightgoldenrod"
    slope_high <- "deepskyblue4"
    rn <- 1
    mid_q <- q05
  } else if(var_name == "i_max_mean") {
    viridis_choice <- "B"
    vir_dir <- -1
    slope_low <- "royalblue4"
    slope_high <- "lightgoldenrod"
    rn <- 1
    mid_q <- q95
    if(!mean_plot) rn <- 2
  }  else {
    viridis_choice <- "D"
    vir_dir <- -1
    slope_low <- "slateblue4"
    slope_high <- "lightgoldenrod"
    rn <- 1
    mid_q <- q95
  }
  
  # The figure without colour palette
  map_res <- df %>% 
    mutate(val = round(val, rn),
           val = case_when(val <= q05 ~ q05,
                           val >= q95 ~ q95,
                           TRUE ~ val)) %>% 
    ggplot(aes(x = lon, y = lat)) +
    geom_raster(aes(fill = val)) +
    # geom_polygon(data = map_base, aes(x = lon, y = lat, group = group), fill = "grey70") +
    # scale_fill_viridis_c(option = viridis_choice, direction = vir_dir) +
    coord_quickmap(expand = F, ylim = c(-70, 70)) +
    labs(x = NULL, y = NULL, fill = legend_title) +
    # guides(fill = guide_colourbar(barwidth = grid::unit(3, units = "inches"))) +
    # theme_void() +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          # legend.position = "top",
          legend.title = element_text(size = 14),# vjust = 1),
          legend.text = element_text(size = 12),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  # Add the colour palette
  if(mean_plot){
    map_res <- map_res +
      geom_polygon(data = map_base, aes(x = lon, y = lat, group = group), fill = "grey70") +
      scale_fill_gradient2(low = slope_low, high = slope_high,
                           breaks = c(q05, q50, q95), midpoint = mid_q,
                           labels = c(paste0("  <",round(q05, rn)), round(q50, rn), paste0(">",round(q95, rn))))
    # scale_fill_viridis_c(option = viridis_choice, direction = vir_dir,
    #                      breaks = c(q05, q50, q95),
    #                      labels = c(paste0("  <",round(q05, rn)), round(q50, rn), paste0(round(q95, rn),">")))
    # scale_fill_gradientn(colours = col_pal,
    #                      limits = c(q05, q95),
    #                      breaks = c(q05, q50, q95),
    #                      labels = c(paste0("  <",round(q05, rn)), round(q50, rn), paste0(round(q95, rn),">")),
    #                      guide = "legend", na.value = NA)
  } else{
    map_res <- map_res +
      geom_point(data = df_p, size = 0.01, alpha = 0.2) +
      geom_polygon(data = map_base, aes(x = lon, y = lat, group = group), fill = "grey70") +
      scale_fill_gradient2(low = slope_low, high = slope_high,
                           breaks = c(q05, q50, q95),
                           labels = c(paste0("<",round(q05, rn)), round(q50, rn), paste0(">",round(q95, rn))))
  }
  # map_res
}

# Create panels
fig_3a <- fig_map_func("total_count", expression(italic("n")))
fig_3b <- fig_map_func("dur_mean", expression(italic("D")))
fig_3c <- fig_map_func("i_max_mean", expression(italic("i"[max])))
fig_3d <- fig_map_func("i_cum_mean", expression(italic("i"[cum])))

# Combine and save
fig_3 <- ggpubr::ggarrange(fig_3a, fig_3b, fig_3c, fig_3d, ncol = 1, nrow = 4, 
                           align = "hv", labels = c("A)", "B)", "C)", "D)"))
ggsave("figures/fig_3.png", fig_3, height = 14, width = 7)
ggsave("figures/fig_3.pdf", fig_3, height = 14, width = 7)


# Figure 4 ----------------------------------------------------------------
# Grouped global trends in MCS metrics

# Load annual data
MCS_annual_mean <- read_rds("data/MCS_annual_mean.Rds")

# Pixels with near-ice
load("metadata/lon_lat_OISST_ice.RData")

# Join with ice data and create global means
MCS_annual_global_mean <- MCS_annual_mean %>% 
  left_join(lon_lat_OISST_ice, by = c("lon", "lat")) %>% 
  mutate(ice_group = case_when(ice & lat <= 0 ~ "S ice",
                               ice & lat > 0 ~ "N ice",
                               TRUE ~ "Ocean")) %>% 
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
fig_line_func <- function(var_name, y_title, y_val, y_expand){
  
  # Subset slope and ANOVA results
  sub_slope <- MCS_annual_global_mean_trends %>% 
    filter(name == var_name) %>% 
    mutate(slope = paste0("m = ",round(slope, 3)),
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
    ggplot(aes(x = year, y = value)) +
    # Add lines, points, and lm
    geom_smooth(aes(colour = ice_group), show.legend = F, method = "lm", 
                formula = "y ~ x", size = 0.5, alpha = 0.2) +
    geom_line(aes(colour = ice_group), show.legend = F) +
    geom_point(aes(colour = ice_group)) +
    # Add slope labels
    geom_label(data = sub_slope, label.size = 2, show.legend = F,
               aes(x = c(1990, 2000, 2010), y = y_val[1], label = label, colour = ice_group)) +
    geom_label(data = sub_slope, label.size = 0,
               aes(x = c(1990, 2000, 2010), y = y_val[1], label = label)) +
    # Add ANOVA label
    geom_label(data = sub_ANOVA, label.size = 2, show.legend = F,
               aes(x = 2000, y = y_val[2], label = label), colour = "darkorchid") +
    geom_label(data = sub_ANOVA, label.size = 0,
               aes(x = 2000, y = y_val[2], label = label)) +
    # Other
    # scale_x_continuous(breaks = seq(1982, 2020, 5), expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(1984, 2019, 7), expand = c(0, 0)) +
    # scale_y_continuous(limits = c(y_val[2]*1.1, y_val[1]*1.1)) +
    scale_y_continuous(expand = c(y_expand, y_expand)) +
    # scale_color_brewer(palette = "Paired") +
    scale_colour_manual(values = c("lightpink", "royalblue", "plum")) +
    labs(y = y_title, x = NULL, colour = "Ocean group") +
    guides(colour = guide_legend(override.aes = list(shape = 15, size = 10))) +
    # coord_cartesian(expand = F) +
    theme(panel.border = element_rect(colour = "black", fill = NA),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 10),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  # facet_wrap(~name, scales = "free_y")
}

# Create the panels
fig_4a <- fig_line_func("count_annual", expression(italic("n")), c(4.2, -0.3), 0.1)
fig_4b <- fig_line_func("duration", expression(italic("D")), c(81, -14), 0.1)
fig_4c <- fig_line_func("intensity_max", expression(italic("i"[max])), c(0.1, -1.8), 0.1)
fig_4d <- fig_line_func("intensity_cumulative", expression(italic("i"[cum])), c(7, -35), 0.12)
fig_4e <- fig_line_func("temp_anom", "SSTa (°C)", c(0.5, -0.45), 0.07)

# Combine and save
fig_4 <- ggpubr::ggarrange(fig_4a, fig_4b, fig_4c, fig_4d, fig_4e, ncol = 1, nrow = 5, 
                           align = "hv", labels = c("A)", "B)", "C)", "D)", "E)"), common.legend = T)
ggsave("figures/fig_4.png", fig_4, height = 11, width = 7)
ggsave("figures/fig_4.pdf", fig_4, height = 11, width = 7)


# Figure 5 ----------------------------------------------------------------
# Maps of the trends in the metrics
# NB: This requires functions from Figure 4 code section

# Crate panels
fig_5a <- fig_map_func("total_count", expression(italic("n")), mean_plot = F)
fig_5b <- fig_map_func("dur_mean", expression(italic("D")), mean_plot = F)
fig_5c <- fig_map_func("i_max_mean", expression(italic("i"[max])), mean_plot = F)
fig_5d <- fig_map_func("i_cum_mean", expression(italic("i"[cum])), mean_plot = F)

# Combine and save
fig_5 <- ggpubr::ggarrange(fig_5a, fig_5b, fig_5c, fig_5d, ncol = 1, nrow = 4, 
                           align = "hv", labels = c("A)", "B)", "C)", "D)"))
ggsave("figures/fig_5.png", fig_5, height = 14, width = 7)
ggsave("figures/fig_5.pdf", fig_5, height = 14, width = 7)


# Figure 6 ----------------------------------------------------------------
# Comparison of SSTa skewness and MHW vs. MCS intensity

# Load the MCS vs. MHW results
MHW_v_MCS <- readRDS("data/MHW_v_MCS.Rds")

# Melt long for easier plotting
MHW_v_MCS_long <- MHW_v_MCS %>% 
  pivot_longer(cols = count:i_cum, names_to = "name", values_to = "value") %>% 
  na.omit() %>% 
  filter(name == "i_max",
         lat >= -70, lat <= 70)

# Find 10th and 90th quantiles to round off tails for plotting
q05 <- quantile(MHW_v_MCS_long$value, 0.05, names = F)
q10 <- quantile(MHW_v_MCS_long$value, 0.1, names = F)
q50 <- quantile(MHW_v_MCS_long$value, 0.5, names = F)
q90 <- quantile(MHW_v_MCS_long$value, 0.9, names = F)
q95 <- quantile(MHW_v_MCS_long$value, 0.95, names = F)

# Plot max intensity
fig_6a <- MHW_v_MCS_long %>%
  mutate(value = case_when(value <= q05 ~ q05,
                           value >= q95 ~ q95,
                           TRUE ~ value)) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = value)) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group), fill = "grey70") +
  coord_quickmap(expand = F, ylim = c(-70, 70)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       breaks = c(q05, q50, q95), 
                       labels = c(paste0("  <",round(q05, 2)), round(q50, 2), paste0(round(q95, 2),">")),) +
  # guides(fill = guide_colourbar(barwidth = grid::unit(3, units = "inches"))) +
  # labs(x = NULL, y = NULL, fill = expression(paste0(italic("i"[maxMCS])," + ",italic("i"[maxMHW])))) +
  labs(fill = expression(paste("abc \n ab" [reported]))) +
  # theme_void() +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        # legend.position = "top",
        legend.title = element_text(size = 14),# vjust = 1),
        legend.text = element_text(size = 12),
        axis.text = element_blank(),
        axis.ticks = element_blank())
fig_6a

# Prep SSTa stats
SSTa_stats <- readRDS("data/SSTa_stats.Rds") %>% 
  dplyr::select(lon:anom_kurt) %>% 
  pivot_longer(c(anom_kurt, anom_skew)) %>% 
  mutate(name = case_when(name == "anom_kurt" ~ "kurtosis",
                          name == "anom_skew" ~ "skewness")) %>% 
  filter(lat >= -70, lat <= 70)

# Find upper skewness and kurtosis quantiles
skew_quants <- SSTa_stats %>% 
  filter(name == "skewness", season == "Total") %>% 
  summarise(q05 = quantile(value, 0.05, names = F),
            q10 = quantile(value, 0.1, names = F),
            q50 = quantile(value, 0.5, names = F),
            q90 = quantile(value, 0.9, names = F),
            q95 = quantile(value, 0.95, names = F))

# Map of skewness per pixel
fig_6b <- SSTa_stats %>% 
  filter(name == "skewness", season == "Total") %>% 
  mutate(value = case_when(value <= skew_quants$q05 ~ skew_quants$q05,
                           value >= skew_quants$q95 ~ skew_quants$q95,
                           TRUE ~ value)) %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_raster(aes(fill = value)) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group), fill = "grey70") +
  coord_quickmap(expand = F, ylim = c(-70, 70)) +
  scale_fill_gradient2("SSTa skewness",
                       low = "blue", mid = "white", high = "red",
                       # low = pal_jco()(3)[1], mid = pal_jco()(3)[3], high = pal_jco()(3)[2],
                       breaks = c(skew_quants$q05, skew_quants$q50, skew_quants$q95), 
                       labels = c(paste0("  <",round(skew_quants$q05, 1)), 
                                  round(skew_quants$q50, 1), 
                                  paste0(round(skew_quants$q95, 1),">"))) +
  guides(fill = guide_colourbar(barwidth = grid::unit(3, units = "inches"))) +
  labs(x = NULL, y = NULL) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        # legend.position = "top",
        legend.title = element_text(size = 14, vjust = 1),
        legend.text = element_text(size = 12),
        axis.text = element_blank(),
        axis.ticks = element_blank())
# fig_6b

# Save
fig_6 <- ggpubr::ggarrange(fig_6a, fig_6b, ncol = 1, nrow = 2, labels = c("A)", "B)"))
ggsave("figures/fig_6.png", fig_6, height = 7, width = 7)
ggsave("figures/fig_6.pdf", fig_6, height = 7, width = 7)


# Figure 7 ----------------------------------------------------------------
# Global annual summaries of MCSs

# Load data
MCS_total <- readRDS("data/MCS_cat_daily_total.Rds")

# Chose category system
MCS_total_filter <- filter(MCS_total, name == "category") %>% 
  dplyr::select(-hemi) %>% 
  group_by(t, name, category) %>% 
  summarise_all(sum) %>% 
  filter(first_n_cum > 0) %>% 
  ungroup()

# Get Ice only for overplotting
MCS_total_ice <- filter(MCS_total, name == "category_ice") %>% 
  filter(category == "V Ice")

# Stacked barplot of global daily count of MHWs by category
fig_count_historic <- ggplot(MCS_total_filter, aes(x = t, y = cat_area_cum_prop)) +
  geom_bar(aes(fill = category), stat = "identity", show.legend = T,
           position = position_stack(reverse = TRUE), width = 1) +
  geom_bar_pattern(data = MCS_total_ice, stat = "identity", show.legend = F,
                   aes(pattern_colour = hemi, colour = hemi), 
                   pattern = "stripe", pattern_fill = NA, fill = NA, 
                   pattern_density = 1, pattern_size = 0.6) +
  scale_fill_manual("Category", values = MCS_colours) +
  scale_colour_manual(values = c("lightpink", "plum")) +
  scale_pattern_colour_manual(values = c("lightpink", "plum")) +
  scale_y_continuous(limits = c(0, 27),
                     breaks = seq(5, 25, length.out = 5),
  # scale_y_continuous(limits = c(0, 0.08),
  #                    breaks = seq(0.02, 0.06, length.out = 3),
                     # labels = paste0(seq(2, 6, by = 2), "%"),
                     sec.axis = sec_axis(name = "Average daily MCS coverage", trans = ~ . + 0,
                                         breaks = c(7.3, 14.6, 21.9),
                                         labels = c("2%", "4%", "6%"))) +
  scale_x_continuous(breaks = seq(1984, 2019, 7)) +
  guides(pattern_colour = FALSE, colour = FALSE) +
  labs(y = "Average MCS days", x = NULL) +
  coord_cartesian(expand = F) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
# fig_count_historic

# Stacked barplot of cumulative percent of ocean affected by MHWs
fig_cum_historic <- ggplot(MCS_total_filter, aes(x = t, y = first_area_cum_prop)) +
  geom_bar(aes(fill = category), stat = "identity", show.legend = T,
           position = position_stack(reverse = TRUE), width = 1) +
  geom_bar_pattern(data = MCS_total_ice, stat = "identity", show.legend = F,
                   aes(pattern_colour = hemi, colour = hemi), 
                   pattern = "stripe", pattern_fill = NA, fill = NA, 
                   pattern_density = 1, pattern_size = 0.6) +
  scale_fill_manual("Category", values = MCS_colours) +
  scale_colour_manual(values = c("lightpink", "plum")) +
  scale_pattern_colour_manual(values = c("lightpink", "plum")) +
  scale_y_continuous(position = "right", 
                     limits = c(0, 0.65),
                     breaks = seq(0.15, 0.6, length.out = 4),
                     labels = paste0(seq(15, 60, by = 15), "%")) +
  scale_x_continuous(breaks = seq(1984, 2019, 7)) +
  labs(y = "Total annual MCS coverage", x = NULL) +
  coord_cartesian(expand = F) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
# fig_cum_historic

# Stacked barplot of average cumulative MHW days per pixel
fig_prop_historic <- ggplot(MCS_total_filter, aes(x = t, y = cat_area_cum_prop)) +
  geom_bar(aes(fill = category), stat = "identity", show.legend = T,
           position = position_stack(reverse = TRUE), width = 1) +
  geom_bar_pattern(data = MCS_total_ice, stat = "identity", show.legend = F,
                   aes(pattern_colour = hemi, colour = hemi), 
                   pattern = "stripe", pattern_fill = NA, fill = NA, 
                   pattern_density = 1, pattern_size = 0.6) +
  scale_fill_manual("Category", values = MCS_colours) +
  scale_colour_manual(values = c("lightpink", "plum")) +
  scale_pattern_colour_manual(values = c("lightpink", "plum")) +
  scale_y_continuous(limits = c(0, 27),
                     breaks = seq(5, 25, length.out = 5)) +
  scale_x_continuous(breaks = seq(1984, 2019, 7)) +
  labs(y = "Average MCS days", x = NULL) +
  coord_cartesian(expand = F) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "none",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
# fig_prop_historic

# Create the figure title
# product_title <- "NOAA OISST"
# min_year <- min(MCS_total_filter$t)
# max_year <- max(MCS_total_filter$t)
# fig_title <- paste0("MCS category summaries: ",min_year,"-",max_year,
#                     "\n",product_title,"; Climatogy period: 1982-2011")

# Stick them together and save
fig_7 <- ggpubr::ggarrange(fig_count_historic, fig_cum_historic, #fig_prop_historic,
                           ncol = 2, align = "hv", labels = c("A)", "B)"), #hjust = -0.1,
                           font.label = list(size = 12), common.legend = T, legend = "bottom")
ggsave(fig_7, filename = paste0("figures/fig_7.png"), height = 3.75, width = 8)
ggsave(fig_7, filename = paste0("figures/fig_7.pdf"), height = 3.75, width = 8)

# Summed up annual ice values for easier reading
sum_ice <- MCS_total_ice %>% 
  group_by(t, name, category) %>% 
  summarise_if(is.numeric, sum) %>% 
  ungroup()

# Summed up annual values for easier reading
sum_all <- MCS_total_filter %>% 
  group_by(t, name) %>% 
  summarise_if(is.numeric, sum) %>%
  ungroup() %>% 
  mutate(first_area_cum_prop = first_area_cum_prop-sum_ice$first_area_cum_prop,
         cat_area_cum_prop = cat_area_cum_prop - sum_ice$cat_area_cum_prop,
         category = "All")

# Values by category
sum_moderate <- MCS_total_filter %>% 
  filter(category == "I Moderate") %>% 
  mutate(first_area_cum_prop = first_area_cum_prop-sum_ice$first_area_cum_prop,
         cat_area_cum_prop = cat_area_cum_prop-sum_ice$cat_area_cum_prop)
sum_cat <- MCS_total_filter %>% 
  filter(category != "I Moderate") %>% 
  rbind(sum_moderate) 
sum_cat_noice <- sum_cat %>% 
  group_by(t, name) %>% 
  summarise_if(is.numeric, sum) %>%
  ungroup() %>% 
  mutate(category = "All_noice")
sum_cat_all <- sum_cat %>%
  rbind(sum_cat_noice) %>% 
  rbind(sum_all) %>% 
  rbind(sum_ice) %>% 
  arrange(t, category) %>% 
  group_by(category) %>% 
  mutate(row_idx = 1:n()) %>% 
  ungroup()

# Linear models
sum_lm <- sum_cat_all %>% 
  group_by(category) %>% 
  nest() %>% 
  mutate(
    cover = map(data, ~ lm(first_area_cum_prop ~ row_idx, data = .x)),
    days = map(data, ~ lm(cat_area_cum_prop ~ row_idx, data = .x)),
    cover_tidy = map(cover, broom::tidy),
    days_tidy = map(days, broom::tidy),
    cover_glance = map(cover, broom::glance),
    days_glance = map(days, broom::glance)
  )

# Extract stats for MCS annual coverage
lm_cover <- unnest(sum_lm, cover_tidy) %>% 
  filter(term == "row_idx") %>% 
  left_join(unnest(sum_lm, cover_glance), by = c("category")) %>% 
  dplyr::select(category, estimate, p.value.y, adj.r.squared) %>% 
  mutate(estimate = round(estimate, 4), p.value.y = round(p.value.y, 2), adj.r.squared = round(adj.r.squared, 2))

# Extract stats for MCS days
lm_days <- unnest(sum_lm, days_tidy) %>% 
  filter(term == "row_idx") %>% 
  left_join(unnest(sum_lm, days_glance), by = c("category")) %>% 
  dplyr::select(category, estimate, p.value.y, adj.r.squared) %>% 
  mutate(estimate = round(estimate, 4), p.value.y = round(p.value.y, 2), adj.r.squared = round(adj.r.squared, 2))

# Display in console
lm_cover
lm_days


# Figure S1 ---------------------------------------------------------------

# Where does the near ice category get flagged
# A map of where the ice flagged MHWs are

# Pixels with near-ice
load("metadata/lon_lat_OISST_ice.RData")
lon_lat_OISST_ice$ice_int <- as.integer(lon_lat_OISST_ice$ice)

# Load event count data per pixel
# NB: This is created in the ice category vignette
MCS_cat_count <- readRDS("data/MCS_cat_count.Rds")

# Calculate the total count of MCSs per pixel
MCS_cat_count_total <- MCS_cat_count %>%
  filter(method == "ice") %>%
  # mutate(category = "total count") %>%
  group_by(lon, lat) %>%
  summarise(total_count = sum(cat_count), .groups = "drop")

# Calculate the proportion of Ice events per pixel
MCS_cat_count_proc <- MCS_cat_count %>%
  filter(method == "ice",
         category == "V Ice") %>% 
  dplyr::select(lon, lat, category, cat_count) %>%
  pivot_wider(values_from = cat_count, names_from = category) %>%
  right_join(MCS_cat_count_total, by = c("lon", "lat")) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(ice_prop = round(`V Ice`/total_count, 4))

# Ice area and proportion of ICE MCS
fig_S1 <- MCS_cat_count_proc %>% 
  ggplot(aes(x = lon, y = lat)) +
  geom_tile(aes(fill = ice_prop)) +
  geom_polygon(data = map_base, aes(x = lon, y = lat, group = group), fill = "grey70") +
  geom_contour(data = lon_lat_OISST_ice, aes(z = ice_int), colour = "black", breaks = c(1)) +
  coord_quickmap(expand = F, ylim = c(-70, 70)) +
  scale_fill_gradient(low = "white", high = "mediumaquamarine") +
  guides(fill = guide_colourbar(barwidth = grid::unit(3, units = "inches"))) +
  labs(x = NULL, y = NULL, fill =  "Ice MCS proportion ") +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        legend.position = "top",
        legend.title = element_text(size = 14, vjust = 1),
        legend.text = element_text(size = 12),
        axis.text = element_blank(),
        axis.ticks = element_blank())
fig_S1

# Save
ggsave("figures/fig_S1.png", fig_S1, height = 3.5, width = 7)
ggsave("figures/fig_S1.pdf", fig_S1, height = 3.5, width = 7)

