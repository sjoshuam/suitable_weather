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
    selectInput(inputId = "month",
      label = "When will you travel?", selected = month.name[current_month],
      choices = month.name),
    sliderInput("morning", min = 0, max = 12, post = ":00", value = c(10, 12),
      label = "When will you be outside before noon?"),
    sliderInput("afternoon", min = 12, max = 23, post = ":00", value = c(12, 20),
      label = "When will you be outside after noon?"),
    sliderInput("temp", min = 10, max = 99, post = "ºF", value = c(45, 80),
      label = "What temperature range is acceptable?")
    ),
  mainPanel(height = 9,
    plotOutput("map")
    )
  )

## SERVER === === === === === === === === === === === === === === === === === ==
## SERVER === === === === === === === === === === === === === === === === === ==

the_server <- function(input, output) {
  output$map <- renderPlot({
    before_noon <- ifelse(
      input$morning[1] == input$morning[2],
      NA, input$morning[1]:input$morning[2])
    after_noon <- ifelse(
      input$afternoon[1] == input$afternoon[2],
      NA, input$afternoon[1]:input$afternoon[2])
    all_hours <- c(before_noon, after_noon)
    all_hours <- ifelse(all(is.na(all_hours)),
      c(input$morning, input$afternoon), all_hours[!is.na(all_hours)])
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

