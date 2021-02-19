# code/visuals.R
# This script provides some space were specific MCS visualisations may be made
# These are not intended for publication and the results are saved to 'output/'


# Libraries ---------------------------------------------------------------

.libPaths(c("~/R-packages", .libPaths()))
source("MCS_prep.R")
library(gganimate)
library(ggpubr)
library(ggridges)
library(ggpattern)
library(glow)
library(viridisLite)
library(heatwaveR); packageVersion("heatwaveR")
library(doParallel); registerDoParallel(cores = 50)
#


# Colour palette comparison figure ----------------------------------------

# Find extreme MCSs
  # NB: No extreme MCS in Med, NWA, WA
  # NB: 2 severe days in Med, 0 in the others
sst_MCS <- detect_event(ts2clm(sst_Med, climatologyPeriod = c("1982-01-01", "2011-12-31"), pctile = 10), coldSpells = T)$climatology %>% 
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff,
                temp = temp/1.2) # Cook the books to get the desired colour range 
# filter(sst_MCS, temp < thresh_4x)

# Centre a line plot on 1994-01-09 
MCS_test_palette <- ggplot(data = sst_MCS, aes(x = t)) +
  geom_flame(aes(y = thresh, y2 = temp, fill = "Moderate"), n = 5, n_gap = 2, show.legend = F) +
  geom_flame(aes(y = thresh_2x, y2 = temp, fill = "Strong"), show.legend = F) +
  geom_flame(aes(y = thresh_3x, y2 = temp, fill = "Severe"), show.legend = F) +
  geom_flame(aes(y = thresh_4x, y2 = temp, fill = "Extreme"), show.legend = F) +
  geom_line(aes(y = thresh_2x, col = "2x Threshold"), size = 0.2, linetype = "dashed") +
  geom_line(aes(y = thresh_3x, col = "3x Threshold"), size = 0.2, linetype = "dotdash") +
  geom_line(aes(y = thresh_4x, col = "4x Threshold"), size = 0.2, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 0.6) +
  geom_line(aes(y = thresh, col = "Threshold"), size = 0.6) +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.4) +
  scale_colour_manual(name = NULL, values = lineCol,
                      breaks = c("Temperature", "Climatology", "Threshold",
                                 "2x Threshold", "3x Threshold", "4x Threshold")) +
  scale_fill_manual(name = "Event colour", values = fillCol) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),
               limits = c(as.Date("1993-10-02"), as.Date("1994-03-30"))) +
  # scale_y_continuous(limits = c(18, 32), expand = c(0, 0),
                     # breaks = seq(20, 30, by = 5)) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid",
                                                                "dashed", "dotdash", "dotted"),
                                                   size = c(1, 1, 1, 1, 1, 1)))) +
  labs(y = "Temp. [°C]", x = NULL) #+
  # formatting for multi-panel figure
  # labs(y = NULL) +
  # theme(axis.text.y = element_blank(),
        # axis.ticks.y = element_blank())
# ggsave("graph/MCS_test_palette.png", MCS_test_palette, width = 6, height = 4)
# ggsave("graph/MCS_test_palette.pdf", MCS_test_palette, width = 6, height = 4)

# Get Oz event
MHW <- ts <- ts2clm(sst_WA, climatologyPeriod = c("1982-01-01", "2011-12-31")) %>% 
  detect_event()
clim_cat <- MHW$clim %>% 
  dplyr::mutate(diff = thresh - seas,
                thresh_2x = thresh + diff,
                thresh_3x = thresh_2x + diff,
                thresh_4x = thresh_3x + diff) %>% 
  dplyr::slice(10580:10690)

# Create MHW for demo
MHW_demo <- ggplot(data = clim_cat, aes(x = t, y = temp)) +
  geom_flame(aes(y2 = thresh, fill = "Moderate")) +
  geom_flame(aes(y2 = thresh_2x, fill = "Strong")) +
  geom_flame(aes(y2 = thresh_3x, fill = "Severe")) +
  geom_flame(aes(y2 = thresh_4x, fill = "Extreme")) +
  geom_line(aes(y = thresh_2x, col = "2x Threshold"), size = 0.2, linetype = "dashed") +
  geom_line(aes(y = thresh_3x, col = "3x Threshold"), size = 0.2, linetype = "dotdash") +
  geom_line(aes(y = thresh_4x, col = "4x Threshold"), size = 0.2, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 0.5) +
  geom_line(aes(y = thresh, col = "Threshold"), size = 0.5) +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.4) +
  scale_colour_manual(name = NULL, values = lineCol,
                      breaks = c("Temperature", "Climatology", "Threshold",
                                 "2x Threshold", "3x Threshold", "4x Threshold")) +
  scale_fill_manual(name = NULL, values = MHW_colours, guide = FALSE) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid",
                                                                "dashed", "dotdash", "dotted"),
                                                   size = c(0.6, 0.7, 0.7, 0.7, 0.7, 0.7)))) +
  labs(y = "Temp. [°C]", x = NULL)
# MHW_demo

# Side by side events
event_compare <- ggarrange(MHW_demo, MCS_test_palette, ncol = 1, nrow = 2, align = "hv", common.legend = TRUE)
ggsave("graph/event_compare.png", event_compare, height = 4, width = 6)

# Create the colour palette for plotting by itself
colour_palette <- data.frame(category = factor(c("I Moderate", "II Strong", "III Severe", "IV Extreme"),
                                               levels = c("I Moderate", "II Strong", "III Severe", "IV Extreme")),
                             MHW = c(MHW_colours[1], MHW_colours[2], MHW_colours[3], MHW_colours[4]),
                             MCS = c(MCS_palette[1], MCS_palette[2], MCS_palette[3], MCS_palette[4])) %>% 
  pivot_longer(cols = c(MHW, MCS), names_to = "event", values_to = "colour")

# Show the palettes side-by-side
palette_compare <- ggplot(data = colour_palette, aes(x = category, y = event)) +
  geom_tile(fill = colour_palette$colour) +
  coord_cartesian(expand = F) +
  labs(x = NULL, y = NULL)
ggsave("graph/palette_compare.png", palette_compare, height = 3, width = 6)

MCS_test_palette <- ggplot(data = sst_MCS, aes(x = t)) +
  geom_flame(aes(y = thresh, y2 = temp, fill = "Moderate"), n = 5, n_gap = 2, show.legend = F) +
  geom_flame(aes(y = thresh_2x, y2 = temp, fill = "Strong"), show.legend = F) +
  geom_flame(aes(y = thresh_3x, y2 = temp, fill = "Severe"), show.legend = F) +
  geom_flame(aes(y = thresh_4x, y2 = temp, fill = "Extreme"), show.legend = F) +
  geom_line(aes(y = thresh_2x, col = "2x Threshold"), size = 0.2, linetype = "dashed") +
  geom_line(aes(y = thresh_3x, col = "3x Threshold"), size = 0.2, linetype = "dotdash") +
  geom_line(aes(y = thresh_4x, col = "4x Threshold"), size = 0.2, linetype = "dotted") +
  geom_line(aes(y = seas, col = "Climatology"), size = 0.6) +
  geom_line(aes(y = thresh, col = "Threshold"), size = 0.6) +
  geom_line(aes(y = temp, col = "Temperature"), size = 0.4) +
  scale_colour_manual(name = NULL, values = lineCol,
                      breaks = c("Temperature", "Climatology", "Threshold",
                                 "2x Threshold", "3x Threshold", "4x Threshold")) +
  scale_fill_manual(name = "Event colour", values = fillCol) +
  scale_x_date(date_labels = "%b %Y", expand = c(0, 0),
               limits = c(as.Date("1993-10-02"), as.Date("1994-03-30"))) +
  # scale_y_continuous(limits = c(18, 32), expand = c(0, 0),
  # breaks = seq(20, 30, by = 5)) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "solid", "solid",
                                                                "dashed", "dotdash", "dotted"),
                                                   size = c(1, 1, 1, 1, 1, 1)))) +
  labs(y = "Temp. [°C]", x = NULL)


# Event data --------------------------------------------------------------

## Halifax
# Extract event metrics
MCS_event_YHZ <- plyr::ldply(MCS_RData[which(lon_OISST >= YHZ_bound[3] & lon_OISST <= YHZ_bound[4])], 
                             .fun = load_MCS_event_sub, .parallel = T, 
                             date_range = c("2016-06-01", "2017-06-01"),
                             lat_range = c(YHZ_bound[1], YHZ_bound[2]))

# edit lat/lon
MCS_event_YHZ <- MCS_event_YHZ %>% 
  mutate(lon = ifelse(lon > 180, lon-360, lon),
         lon = round(lon, 3), lat = round(lat, 3))

# Subset for plotting
MCS_event_YHZ_sub <- MCS_event_YHZ %>% 
  filter(date_start >= "2016-12-10", date_start <= "2017-01-15")

# One pixel for time series example
MCS_event_YHZ_one <- MCS_event_YHZ %>% 
  filter(lon == -64.125, lat == 46.375)


# Event visuals -----------------------------------------------------------

# Maximum intensity
event_max <- ggplot(MCS_event_YHZ_sub, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = intensity_max)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  labs(x = "", y = "", fill = "Max. Intensity (°C)") +
  scale_fill_gradient(low = "grey", high = "navy") +
  theme(legend.position = "bottom")
event_max

# Cumulative intensity
event_cum <- ggplot(MCS_event_YHZ_sub, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = intensity_max)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  labs(x = "", y = "", fill = "Cum. Intensity (°C x days)") +
  scale_fill_gradient(low = "grey", high = "deepskyblue") +
  theme(legend.position = "bottom")
event_cum

# Rate of onset
event_onset <- ggplot(MCS_event_YHZ_sub, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = rate_onset)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  labs(x = "", y = "", fill = "Rate onset (°C/days)") +
  scale_fill_gradient(low = "grey", high = "darkorchid") +
  theme(legend.position = "bottom")
event_onset

# Duration
event_duration <- ggplot(MCS_event_YHZ_sub, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = duration)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  labs(x = "", y = "", fill = "Duration (days)") +
  scale_fill_gradient(low = "grey", high = "steelblue") +
  theme(legend.position = "bottom")
event_duration

# Combine
event_all <- ggpubr::ggarrange(event_max, event_cum, event_onset, event_duration)
event_all
ggsave(event_all, filename = "graph/MCS/YHZ_2016_12.png", width = 18, height = 12)

ggplot(MCS_event_YHZ_one, aes(x = date_start, y = intensity_max)) +
  geom_lolli(colour = "steelblue3", colour_n = "navy", n = 0) +
  labs(x = "Start Date",
       y = expression(paste("Max. intensity [", degree, "C]")))


# Clim data ---------------------------------------------------------------

# Currently interested in the 2016 winter MCS that happened just outside of the Bay of Fundy
YHZ_bound <- c(42, 47, -75, -55)

# Extract event metrics
MCS_clim_YHZ <- plyr::ldply(MCS_RData[which(lon_OISST >= YHZ_bound[3] & lon_OISST <= YHZ_bound[4])], 
                            .fun = load_MCS_clim_sub, .parallel = T, 
                            date_range = c("2016-06-01", "2017-06-01"),
                            lat_range = c(YHZ_bound[1], YHZ_bound[2]))

# Subset for plotting
MCS_clim_YHZ_sub <- MCS_clim_YHZ %>% 
  mutate(lon = ifelse(lon > 180, lon-360, lon),
         intensity = thresh-temp) %>% 
  filter(t >= "2016-12-01", t <= "2016-12-31")

# One pixel for time series example
MCS_clim_YHZ_one <- MCS_clim_YHZ %>% 
  mutate(lon = round(lon, 3), lat = round(lat, 3),
         lon = ifelse(lon > 180, lon-360, lon)) %>% 
  filter(lon == -56.875, lat == 42.875)

# Top event from above dataframe
MCS_clim_YHZ_top <- MCS_clim_YHZ_one %>% 
  slice(223:239)


# Clim visuals ------------------------------------------------------------

# Time series
ggplot(MCS_clim_YHZ_one, aes(x = t, y = thresh, y2 = temp)) +
  geom_flame(aes(y = thresh, y2 = temp, fill = "all"), show.legend = T) +
  geom_flame(data = MCS_clim_YHZ_top, aes(y = thresh, y2 = temp, fill = "top"), show.legend = T) +
  geom_line(aes(y = temp, colour = "temp")) +
  geom_line(aes(y = thresh, colour = "thresh"), size = 1.0) +
  geom_line(aes(y = seas, colour = "seas"), size = 1.2) +
  scale_colour_manual(name = "Line Colour",
                      values = c("temp" = "black", "thresh" =  "forestgreen", "seas" = "grey80")) +
  scale_fill_manual(name = "Event Colour", values = c("all" = "steelblue3", "top" = "navy")) +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  scale_y_continuous(limits = c(2, 25))

# A map
clim_temp <- ggplot(filter(MCS_clim_YHZ_sub, t == "2016-12-18"), aes(x = lon, y = lat)) +
  geom_raster(aes(fill = intensity)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  scale_fill_gradient(low = "grey", high = "steelblue") +
  theme(legend.position = "bottom")
clim_temp

# Base map
yhz_base <- ggplot(map_base, aes(x = lon, y = lat)) +
  geom_polygon(data = map_base, aes(group = group)) +
  coord_cartesian(xlim = YHZ_bound[3:4], ylim = YHZ_bound[1:2], expand = F) +
  theme(legend.position = "bottom")
yhz_base

# Animation for December 2016
yhz_base + geom_raster(data = MCS_clim_YHZ_sub, aes(fill = intensity)) +
  scale_fill_gradient(low = "navy", high = "grey") +
  labs(title = 'Date: {frame_time}', x = '', y = '', fill = "°C below threshold") +
  transition_time(t)
anim_save("graph/MCS/YHZ_2016_12.gif")

