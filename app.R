##########==========##########==========##########==========##########==========

## SET UP ======================================================================

## meta-information
## author: Josh M
## creation: 2021-03-30
## version: 4.0.4 Lost Library Book
## description: generate shiny app to map out the weather

## load packages
library(tidyverse)
library(shiny)
library(mapproj)
library(rsconnect)

## set up environment
remove(list = objects())
options(dplyr.summarise.inform = FALSE, width = 80, scipen = 2, digits = 6)

## read in files
load("app_support.Rdata")

## UI === === === === === === === === === === === === === === === === === === ==
## UI === === === === === === === === === === === === === === === === === === ==

current_month <- as.numeric(substring(Sys.time() + (30 * 24 * 60 * 60), 6, 7))

outdoor_times <- tribble(
  ~month, ~am_start, ~am_end, ~pm_start, ~pm_end, ~schedule,
  11, 12, 12, 12, 19, "cold",
  12, 12, 12, 12, 19, "cold",
  01, 12, 12, 12, 19, "cold",
  02, 12, 12, 12, 19, "cold",
  03, 12, 12, 12, 19, "cold",
  
  04, 09, 12, 17, 20, "switch",
  10, 09, 12, 17, 20, "switch",
  
  05, 07, 11, 18, 20, "Hot",
  06, 07, 11, 18, 20, "Hot",
  07, 07, 11, 18, 20, "Hot",
  08, 07, 11, 18, 20, "Hot",
  09, 07, 11, 18, 20, "Hot"
  ) %>%
  filter(month == current_month)



the_ui <- fluidPage(
  h3("Roadtrip Planning Tool: Chances Of Temperate Weather"),
  p("This tool estimates the chances that you will encounter temperate weather",
  "if you go roadtripping in a specific month in a specific part of the country.",
  "You can specify the month using the pull down menu.",
    "On the map,",
    "brighter shades of green indicate a higher probability of temperate",
    "weather.  The probabilities are based on how often the weather was",
    "temperate in that part of the country in that month over the past 30",
    "years."
    ),
  p(
    "\"Temperate\" is a relative term.  Temperatures vary throughout",
    "the day/night cycle and different temperature ranges are preferable for",
    "different activities.  You can use the middle two sliders to specify",
    "the time ranges during which you would like temperate weather.  You can",
    "use the bottom slider to specify the range of temperatures that you would",
    "prefer. For example, by default, the tool assumes that temperate weather",
    "means that the weather means warmer than 40º Fairenheit but cooler than",
    "80º (bottom slider)."
    ),
  p("LIMITATIONS: This tool provides a rough approximation of weather conditions. It",
    "relies on NOAA'S temperature data for 128 counties across the",
    "contiguous United States and then generalizes across the rest of the United States.  Consequently,",
    "the tool does not provide fine-grained results for micro-climates, just",
    "rough approximations for broad regions.  For example, the tool will not",
    "show local temperature deviations due to elevation."
    )
  ,
  sidebarPanel(width = 3,
    selectInput(inputId = "month",
      label = "When will you travel?", selected = month.name[current_month],
      choices = month.name),

    sliderInput("morning", min = 0, max = 12, post = ":00",
      value = c(outdoor_times$am_start, outdoor_times$am_end),
      label = "When will you be outside before noon?"),
    sliderInput("afternoon", min = 12, max = 23, post = ":00",
      value = c(outdoor_times$pm_start, outdoor_times$pm_end),
      label = "When will you be outside after noon?"),
    
    sliderInput("temp", min = 10, max = 99, post = "ºF", value = c(40, 80),
      label = "What temperature range is acceptable?"),
  ),
  mainPanel(width = 9,
    plotOutput("map")
    )
  )

## SERVER === === === === === === === === === === === === === === === === === ==
## SERVER === === === === === === === === === === === === === === === === === ==

the_server <- function(input, output) {
  output$map <- renderPlot({
    before_noon <- input$morning[1]:input$morning[2]
    after_noon <- input$afternoon[1]:input$afternoon[2]
    if (input$morning[1] == input$morning[2]) before_noon <- NA
    if (input$afternoon[1] == input$afternoon[2]) afternoon <- NA      
    all_hours <- c(before_noon, after_noon)
    if (all(is.na(all_hours))){
      all_hours <- c(input$morning, input$afternoon)
      } else {all_hours <- all_hours[!is.na(all_hours)]}
    weather_summary <- InterpolateTemperature(hours = all_hours,
      w_data = weather_data)
    weather_summary <- ScoreLocation(months = input$month,
      w_data = weather_summary,
      temp_max = max(input$temp), temp_min = min(input$temp))
    weather_summary <- GeneratePlot(w_summary = weather_summary,
      l_map = location_map, s_map = state_map)
    weather_summary
    })
  }

## EXECUTE === === === === === === === === === === === === === === === === === =
## EXECUTE === === === === === === === === === === === === === === === === === =
shinyApp(the_ui, the_server)
##########==========##########==========##########==========##########==========

