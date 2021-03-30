##########==========##########==========##########==========##########==========

## SET UP ======================================================================

## meta information
## author: Josh M.
## creation: 2020-03-04
## R version: v4.0.3
## description: calculates county-levelclimatic tendencies from weather station
   ## data

## environment set up
remove(list = objects())
options(width = 80, scipen = 2, digits = 6, dplyr.summarise.inform = FALSE)
library(tidyverse)
library(foreign)
library(sp)
library(parallel)

## READ IN DATA ================================================================

## load county data
county_data <- read.dbf("A_Inputs/tl_2020_us_county.dbf", as.is = TRUE) %>%
  as_tibble()
county_map <- map_data("county") %>% as_tibble()
state_data <- readRDS("B_Intermediates/state.RData")
population <- readRDS("B_Intermediates/population.RData")
state_map <- map_data("state") %>% as_tibble()

## load weather data
ReadWeather <- function(x) {
  x <- x %>%
    readRDS() %>%
    dplyr::group_by(datatype, date) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::summarize(value = mean(as.numeric(value)))
  return(x)
  }

weather_data <- setNames(
  list.files("B_Intermediates/noaa_data", full.names = TRUE),
  list.files("B_Intermediates/noaa_data", full.names = FALSE)
)

parallel_cluster <- makeCluster(max(detectCores() - 2, 1))
clusterExport(cl = parallel_cluster, varlist = list("%>%"))
weather_data <- parLapply(
  X = weather_data,
  fun = ReadWeather,
  cl = parallel_cluster
  )
stopCluster(parallel_cluster)
remove(parallel_cluster, ReadWeather)

## COMPILE WEATHER DATA ========================================================

## compile dataset chunks
weather_data <- tibble(
  "file" = rep(names(weather_data), sapply(weather_data, nrow)),
  do.call(what = rbind, args = weather_data)
)

## unpack file names; refine data
weather_data <- weather_data %>%
  mutate(
    county = str_remove_all(file, "_.*"),
    month = str_sub(date, 6, 7),
    value = as.numeric(value),
    date = str_sub(date, 1, 10)
    ) %>%
  select(-file) %>%
  pivot_wider(names_from = datatype, values_from = value) %>%
  mutate(
    GEOID = str_sub(county, 2)
    )
colnames(weather_data) <- str_to_lower(colnames(weather_data))

## inventory date
warning("Temporary expression - remove when no longer needed")
tapply(weather_data$tmax, weather_data[, c("month", "county")], length)

## SHAPE COUNTY DATA ===========================================================

## drop unneeded columns and join state postal abbreviations
county_data <- county_data %>%
  select(-MTFCC, -CSAFP, -CLASSFP, -LSAD, -NAMELSAD, -COUNTYNS, -METDIVFP) %>%
  select(-FUNCSTAT) %>%
  left_join(select(state_data, state, stusab), by = c("STATEFP" = "state")) %>%
  rename(state = stusab)
remove(state_data)

## join population data
population <- population %>%
  mutate(state_name = str_to_title(str_remove(NAME, ".*, ")))
county_data <- county_data %>%
  left_join(select(population, GEOID, state_name, population), by = "GEOID")
remove(population)

## fix irregularities in county names
RegularCountyNames <- function(x) {
    x <- str_replace(x, "^De ", "De")
    x <- str_replace(x, "^Du ", "Du")
    x <- str_replace(x, "^La ", "La")
    x <- str_remove_all(x, "[^A-Za-z0-9 ]")
    x <- str_replace_all(x, "Doa Ana", "Dona Ana")
    x <- str_replace_all(x, "Baltimore City", "Baltimore")
    x <- str_replace_all(x, "St Louis City", "St Louis")
    x <- str_replace_all(x, "Yellowstone National", "Yellowstone")
    x <- str_replace_all(x, "District of Columbia", "Washington")
    x <- str_to_title(x)
    return(x)
  }

county_map <- county_map %>%
  mutate(
    state_name = str_to_title(region),
    county_name = str_to_title(subregion),
    county_name = RegularCountyNames(county_name)
    )
county_data$NAME <- RegularCountyNames(county_data$NAME )
remove(RegularCountyNames)

## generate matching primary key for map data
county_map <- county_map %>%
  left_join(select(county_data, state, state_name), by = "state_name") %>%
  mutate(county_name = paste(county_name, state)) %>%
  mutate(
    county_name = str_replace(county_name, "Shannon SD", "Oglala Lakota SD"))
county_data <- mutate(county_data, county_name = paste(NAME, state))
county_map <- county_map %>%
  left_join(select(county_data, county_name, GEOID), by = "county_name")

## limit counties to just contiguous united states
county_data <- county_data %>%
  filter(!(state %in% c("AS", "GU", "MP", "PR", "VI", "AK", "HI")))
county_map <- county_map %>%
  filter(!(state %in% c("AS", "GU", "MP", "PR", "VI", "AK", "HI")))

## clean county data
county_data <- county_data %>%
  mutate(long = as.numeric(INTPTLON), lat = as.numeric(INTPTLAT)) %>%
  select(GEOID, county_name, state, population, long, lat, STATEFP, COUNTYFP,
    ALAND, AWATER)
colnames(county_data) <- str_to_lower(colnames(county_data))

county_map <- select(county_map, -state_name)
colnames(county_map) <- str_to_lower(colnames(county_map))

## EXPORT RESULTS ==============================================================

saveRDS(state_map,  file = "B_Intermediates/state_map.RData")
saveRDS(county_data, file = "B_Intermediates/county_data.RData")
saveRDS(county_map,  file = "B_Intermediates/county_map.RData")
saveRDS(weather_data,  file = "B_Intermediates/weather_data.RData")

##########==========##########==========##########==========##########==========
