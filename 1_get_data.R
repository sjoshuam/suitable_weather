##########==========##########==========##########==========##########==========

## SET UP ======================================================================

## meta information
## author: Josh M
## creation: 2021-03-03
## version: R v4.0.3
## description: pulls weather data from NOAA for each county and characterizes
   ## the climate in each

##  environment set-up
remove(list = objects())
options(width = 80, scipen = 2, digits = 6, dplyr.summarise.inform = FALSE)
source("../api_keys/noaa_api_key.R")
source("../api_keys/censusAPI.R")
library(tidyverse)
library(sp)
library(foreign)
library(httr)
library(jsonlite)
library(parallel)

## set data resolution (more is better; max value equals number of counties)
data_resolution <- 16 * 2

## PULL POPULATION DATA FROM CENSUS ============================================

##  retrieve state fips codes
state_url <- url("https://www2.census.gov/geo/docs/reference/state.txt")
state_codes <- state_url %>%
  readLines() %>%
  str_split("\\|") %>%
  simplify2array() %>%
  t() 
colnames(state_codes) <- str_to_lower(state_codes[1, ])
state_codes <- as_tibble(state_codes[-1, ])

close(state_url)
remove(state_url)

saveRDS(state_codes, file = "B_Intermediates/state.RData")

## remove areas outside the contiguous United States (CONUS)
state_codes <- state_codes %>%
  filter(!(stusab %in% c("AK", "AS", "GU", "HI", "MP", "PR", "UM", "VI")))

## construct API queries
state_codes$census_query <- paste0(
  "https://api.census.gov/data/2019/acs/acs5?",
  "get=NAME,B01001_001E&for=county:*&in=state:",
  state_codes$state,
  "&key=",
  census_api_key
  )

if (!file.exists("B_Intermediates/population.RData")) {

## pull data
APIpull <- function(x) {
  x <- url(x)
  y <- readLines(x)
  y <- jsonlite::fromJSON(y)
  colnames(y) <- y[1, ]
  y <- y[-1, ]
  y <- dplyr::as_tibble(y)
  close(x)
  y
}

population <- as.list(state_codes$census_query)

p_cluster <- makeCluster(floor(detectCores() * 0.8))
population <- parLapply(population, APIpull, cl = p_cluster)
stopCluster(p_cluster)

remove(APIpull, p_cluster)

## fix Washington DC
i <- which(sapply(population, ncol) == 1)
population[[i]] <- t(population[[i]])
colnames(population[[i]]) <- colnames(population[[i+1]])
remove(i)

## compile data
population <- do.call(what = rbind, args = population) %>%
  mutate(
    population = as.numeric(B01001_001E),
    GEOID = paste0(state, county)
    ) %>%
  select(-B01001_001E)

saveRDS(population, file = "B_Intermediates/population.RData")

} else {
  population <- readRDS(file = "B_Intermediates/population.RData")
  }

## DOWNLOAD COUNTY GEOGRAPHY DATA ==============================================

if (!file.exists("A_Inputs/tl_2020_us_county.dbf")) {

## pull cbsa data file
download.file(
  "https://www2.census.gov/geo/tiger/TIGER2020/COUNTY/tl_2020_us_county.zip",
  destfile = "temp.zip"
  )

## unpack files
unzip(
  zipfile = "temp.zip",
  files = "tl_2020_us_county.dbf",
  exdir = "A_Inputs"
  )

## delete spare files
file.remove("temp.zip")

} ## end of file check

## read in data
county_data <- read.dbf("A_Inputs/tl_2020_us_county.dbf", as.is = TRUE) %>%
  as_tibble() %>%
  select(STATEFP, COUNTYFP, GEOID, NAME, ALAND, AWATER, INTPTLON, INTPTLAT)

## merge population data
county_data <- county_data %>%
  left_join(select(population, GEOID, population), by = "GEOID")

##  thin out CBSA that are close together
county_data <- county_data %>%
  filter(!is.na(population)) %>%
  arrange(desc(population)) %>%
  mutate(
    lon = as.numeric(INTPTLON),
    lat = as.numeric(INTPTLAT)
    )

set.seed(6323)
county_data$neighbor <- spDists(as.matrix(select(county_data, lon, lat)),
  longlat = TRUE) %>%
  as.dist() %>%
  hclust(method = "ward.D2") %>%
  cutree(k = data_resolution) %>%
  duplicated()

county_data <- county_data %>%
  filter(!neighbor & population >= 10^4) %>%
  select(-INTPTLAT, -INTPTLON, - neighbor)

## merge state postal abbreviations
county_data <- county_data %>%
  left_join(select(state_codes, state, stusab), by = c("STATEFP" = "state")) %>%
  rename(state = stusab)

## DOWNLOAD NOAA DATA ==========================================================

## assemble constant api url components
the_url <- parse_url("https://www.ncdc.noaa.gov/cdo-web/api/v2/data")
the_url$query <- list(
  datasetid = "GHCND",
  units = "standard",
  limit = 1000,
  datatypeid = "TMIN,TMAX,PRCP,SNOW"
  )
the_url <- build_url(the_url)

## assemble variable api url components
DigitPad <- function(x){str_sub(x + 100, 2)}

query_list <- Sys.time() %>% str_sub(1, 4) %>% as.numeric() - 1
query_list <- expand.grid(
  "year" = (query_list - 29):query_list,
  "first_month" = DigitPad(seq(1:12))
  ) %>%
  as_tibble() %>%
  mutate(
    first_month = as.character(first_month),
    last_month = first_month,
    first_day = DigitPad(12),
    last_day = DigitPad(18)
    ) %>%
  mutate(
    start_date = paste(year, first_month, first_day, sep = "-"),
    end_date = paste(year, last_month, last_day, sep = "-"),
    date_key = seq(length(year))
    )

query_list <- left_join(
  query_list,
  expand_grid(
    "date_key" = seq(nrow(query_list)),
    "cbsa" = county_data$GEOID
    ),
  by = "date_key"
  ) %>%
  select(cbsa, start_date, end_date) %>%
  mutate(
    prim_key = paste0("c", cbsa, "_", str_remove_all(start_date, "-"))
    )

## assemble api query urls
query_list$url <- paste0(
  the_url,
  "&locationid=FIPS:",  query_list$cbsa,
  "&startdate=",   query_list$start_date,
  "&enddate=",     query_list$end_date
  )

## declare function to retrieve data from the NOAA API
query_header <- add_headers(token = noaa_api_key)

RetrieveNOAAdata <- function(api_url, qh = query_header) {
  
  ## calculate start time for rate limit governor
  start_time <- Sys.time()
  
  ## pull data
  the_data <- httr::GET(api_url, qh)
  if (the_data$status_code != 200){
    warning(paste("Status code:", the_data$status_code))
    return(NULL)
  }
  
  ## parse request
  the_request <- httr::parse_url(api_url)
  
  ## parse data
  the_data <- content(the_data, as = "parsed")
  if (length(the_data) == 0) return(NULL)
  if (the_data$metadata$resultset$count > 999) {
    warning(paste0("Some results omitted: ", the_data$metadata$resultset$count,
      " Results"))
  }
  the_data <- the_data$results
  the_data <- the_data[sapply(the_data, length) == 5]
  the_data <- sapply(the_data, unlist) %>%
    t() %>%
    as_tibble() %>%
    select(-attributes)
  
  ## save data to disk
  file_name <- substring(the_request$query$startdate, 1, 10) %>%
    stringr::str_remove_all("-")
  file_name <- the_request$query$locationid %>%
    paste(file_name, sep = "_") %>%
    stringr::str_replace_all("FIPS[:]", "c")
  file_name <- paste0("B_Intermediates/noaa_data/", file_name, ".RData")
  saveRDS(the_data, file = file_name)
  
  ## pause as needed to respect rate limit
  now_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  while (now_time <= 8.64) {
    now_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    }
}

## remove data from query list if it has already been retrieved
if (!file.exists("B_Intermediates/noaa_data")) {
  dir.create("B_Intermediates/noaa_data")
}

## prepare query list; exclude data already gathered
noaa_data <- setNames(as.list(query_list$url), 
  paste0(query_list$prim_key, ".RData"))
already_collected <- list.files("B_Intermediates/noaa_data")
noaa_data <- noaa_data[!(names(noaa_data) %in% already_collected)]

print(paste0("Time remaining: ",
  round(length(noaa_data) * (8.65/3600), 1),
  "hr"))

print(paste0("Completion time: ",
  Sys.time() + ceiling(length(noaa_data) * (8.65))
  ))

## retrieve data
TryWrapper <- function(x, f = RetrieveNOAAdata) try(f(x), silent = TRUE)
lapply(noaa_data, TryWrapper)

##########==========##########==========##########==========##########=========
