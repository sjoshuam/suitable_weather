##########==========##########==========##########==========##########==========

## SET UP ======================================================================

## meta-information
## author: Josh M
## creation: 2021-03-30
## version: 4.0.4 Lost Library Book
## description: generate shiny app to check

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

current_month <- as.numeric(substring(Sys.time(), 6, 7))

the_ui <- fluidPage(
  h3("TITLE HERE"),
  p("EXPLAINER HERE"),
  sidebarPanel(width = 3,
    sliderInput("hours", min = 0, max = 23, post = ":00", value = c(10, 20),
      label = "When will you be outside?"),
    sliderInput("temp", min = 20, max = 110, post = "ºF", value = c(50, 80),
      label = "What temperature range is acceptable?"),
    checkboxGroupInput(
      width = 3,
      inputId = "months",
      label = "What month(s) will you travel?",
      choices = setNames(c(12, 1:2, 3:11), month.abb[c(12, 1:2, 3:11)]),
      selected = current_month
      )
    ),
  mainPanel(height = 9,
    plotOutput("map")
    )
  )

## SERVER === === === === === === === === === === === === === === === === === ==
## SERVER === === === === === === === === === === === === === === === === === ==

the_server <- function(input, output) {
  output$map <- renderPlot({
    weather_summary <- InterpolateTemperature(hours = input$hours,
      w_data = weather_data)
    weather_summary <- ScoreLocation(months = input$months,
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

