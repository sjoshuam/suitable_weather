##########==========##########==========##########==========##########==========

## SET UP ======================================================================

## meta-information
## author: Josh M.
## creation: 2021-03-05
## version: R v4.0.3
## description: This script calculates and maps the best month to road trip
   ## through different regions of the United States.  This accomplishes three
   ## purposes: (1) serves as a trial run for the calculations that will run
   ## in the background of the interactive tool (2) generates a thumbnail
   ## visualization for the GitHub project gallery (3) generates a standard,
   ## static reference map for ideal road trip condition

## environment set up
remove(list= objects())
options(width = 80, scipen = 2, digits = 6, dplyr.summarise.inform = FALSE)
library(tidyverse)
library(sp)
library(sf)

## READ DATA ===================================================================
county_data   <- readRDS("B_Intermediates/county_data.RData")
county_map    <- readRDS("B_Intermediates/county_map.RData") %>%
  mutate(group = paste(geoid, group, sep = "-"))
state_map    <- readRDS("B_Intermediates/state_map.RData") 
weather_data  <- readRDS("B_Intermediates/weather_data.RData")

## remove oconus
county_data <- filter(county_data, !(state %in% c("AK", "HI")))
county_map <- filter(county_map, !(state %in% c("AK", "HI")))

## DECLARE CORE FUNCTIONS ======================================================

## Interpolate temperature ranges, given likely activity hours
InterpolateTemperature <- function(w_data = weather_data, hours){
  
  ## generate interpolation factors for time of day
  hours <- min(hours):max(hours)
  hour_temp <- tibble(
    hour = c(6:14, 15:23, 0:5),
    factor = c(
      seq(from = 0, to = 1, length.out = 9),
      seq(from = 1, to = 0, length.out = 24 - 9)
      )
    ) %>%
    filter(hour %in% hours) %>%
    pull(factor) %>%
    range()
  
  ## adjust min and max temperature accordingly
  w_data <- w_data %>%
    mutate(
      low = (tmax * min(hour_temp)) + (tmin * (1 - min(hour_temp))),
      high = (tmax * max(hour_temp)) + (tmin * (1 - max(hour_temp))),
      ) %>%
    select(-tmin, -tmax) %>%
    rename(tmin = low, tmax = high)
  
  return(w_data)
}

## score each location by percentage of days in desired temperature range
ScoreLocation <- function(w_data = weather_data, months,
  temp_max = 79, temp_min = 50) {
  
  w_data %>%
    mutate(temp_max = temp_max, temp_min = temp_min) %>%
    filter(as.numeric(month) %in% months) %>%
    mutate(suitable = (tmax <= temp_max) & (tmin >= temp_min)) %>%
    group_by(geoid, month) %>%
    summarize("suitable" = mean(as.numeric(suitable), na.rm = TRUE)) %>%
    group_by(geoid) %>%
    summarize("suitable" = mean(suitable, na.rm = TRUE) * 100)
}

## declare function to visualize data
GeneratePlot <- function(w_summary, l_map = location_map,
  s_map = state_map) {
  
  ## incorporate scores into mapping
  l_map <- l_map %>% left_join(w_summary, by = c("location" = "geoid"))
  
  ## initialize plotting object
  plot_object <- ggplot() +
    coord_map(projection = "sinusoidal", orientation = c(90, 0, -98.5),
      xlim = -98 + 1 + c(-1, 1) * 21
      ) +
    scale_fill_binned(breaks = c(50, 70), limits = c(0, 100),
      low = hsv(h = 3 / 12, s = 0.8, v = 0.0),
      high = hsv(h = 3 / 12, s = 0.8, v = 1.0),
      name = "Days In\nTemperature\nRange (%)") +
    scale_color_binned(breaks = c(50, 70), limits = c(0, 100),
      low = hsv(h = 9 / 12, s = 0.8, v = 0.0),
      high = hsv(h = 3 / 12, s = 0.8, v = 1.0),
      name = "Days In\nTemperature\nRange (%)") +
    xlab(NULL) + ylab(NULL) +
    geom_polygon(
      data = l_map,
      mapping = aes(x = X, y = Y, group = location, fill = suitable,
        color = suitable),
      size = 0.2
      ) +
    geom_polygon(
      data = s_map,
      mapping = aes(x = long, y = lat, group = group),
      color = "white", size = 0.6, fill = "transparent"
      ) +
    geom_polygon(
      data = s_map,
      mapping = aes(x = long, y = lat, group = group),
      color = "black", size = 0.3, fill = "transparent"
      )
  
  return(plot_object)
}

## GENERATE MAP AREA POLYGONS ==================================================

## assign all counties to a data location
data_locations <- county_data %>% filter(geoid %in% weather_data$geoid)
the_distance <- spDists(
  as.matrix(data_locations[, c("long", "lat")]),
  as.matrix(county_data[, c("long", "lat")]), longlat = TRUE) %>%
  apply(MARGIN = 2, which.min)
county_data$location <- data_locations$geoid[the_distance]

## generate sf-format polygon for each county
location_map <- county_map %>% select(long, lat, group) %>% as.data.frame()
location_map <- split(
  x = location_map[, c("long", "lat")], f = location_map$group) %>%
  lapply(as.matrix) %>%
  lapply(list) %>%
  lapply(st_polygon) %>%
  lapply(st_buffer, dist = 10^-2)

## merge polygons for each data location
data_locations <- tibble(group = names(location_map)) %>%
  left_join(unique(select(county_map, group, geoid)), by = "group") %>%
  left_join(unique(select(county_data, location, geoid)), by = "geoid")
location_map <- tapply(location_map, data_locations$location, list) %>%
  lapply(st_sfc) %>%
  lapply(st_combine) %>%
  lapply(st_union, by_feature = TRUE) %>%
  lapply(st_coordinates) %>%
  lapply(function(x){x[, 1:2]})

## convert to tidy format
location_map <- data.frame(
  "location" = rep(names(location_map), sapply(location_map, nrow)),
  do.call(what = rbind, args = location_map)) %>%
  as_tibble()

## TEST FUNCTIONS ==================================================

save(InterpolateTemperature, ScoreLocation, GeneratePlot,
  location_map, state_map, weather_data,
  file = "app_support.Rdata")

##########==========##########==========##########==========##########==========
