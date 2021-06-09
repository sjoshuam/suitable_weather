### Description

Will the weather in Peoria in October be suitable for road trippin g?  This
project creates an interactive tool for assessing likely weather conditions for
a specific location in a specific time of the year.  The tool provides useful
climate knowledge that can inform road trip planning.  It currently only covers
the contiguous United States.

### Repository Layout

This repository contains 3-4 directories.  For convenience, each directory name
has a unique letter prefix, such as "X_".

+ **A_Inputs** - Holds all source data files.  For coding purposes, I treat this
directory as read only.
+ **B_Intermediates** – Holds data files created as byproducts of the
computations performed.  This can include cached API pulls, temporary data caches for large computations, and data objects to be passed from one script to the next.
+ **C_Outputs** – Holds the final products for the project.
+ **D_Misc** – If needed, this directory will contain any other useful materials
for the project, such as source data documentation.

The scripts containing executable code are in the top level of the repository.
Each script has a unique numeric prefix, such as "2_", which indicates the order
in which to execute the scripts.

### Scripts (Actions, Inputs, and Outputs)

**1_get_data.R** - This script divides US counties into geographic clusters and
downloads historical weather data for all weather stations in one county in each
cluster.  The weather data comes from NOAA via api pull and pull is rate limited
to stay under request caps (which are surprisingly low).  The pull script is
robust to being arbitrarily stopped and will avoid downloading data is has
already downloaded.  The data_resolution object controls how many geographic
clusters are used.  More clusters generates better spatial data resolution but
makes the project's app run slower.  To keep data bulk down, the tool downloads
data for 10 days out of each month and then treats that week as representative of
the month. The 10 days are the 10th through the 20th of the month. In addition,
this script also makes a pull from the census API to gather basic information on
US counties.

Inputs: Data downloaded via API.  Requires an API key for the US census (county data)
and NOAA (weather data)

Outputs:
+ Each NOAA API pull generates an .RData file in B_Intermediates/noaa_data.
Each file holds a flat dataset containing temperature measurements for each
weather station in the focal county for a specified date range.
+ The census API pull generates B_Intermediates/population.RData, a dataset of
basic information about US counties,  such as population.

**2_prepare_data.R** - Compiles raw data download files into a single dataset.

Inputs:
+ Many small .Rdata files in B_Intermediates/noaa_data, each of which holds
a small tibble dataset.
+ Map data courtesy of R's map package.

Outputs:
+ B_Intermediates/state_map.RData - state polygons for the contiguous US states.
+ B_Intermediates/county_map.RData - county polygons for all counties in the
contiguous United States.
+ B_Intermediates/county_data.RData - basic dataset for US counties. Includes
name, state, centroid coordinates, population, surface area, and ANSI ID number.
+ B_Intermediates/weather_data.RData - Dataset of temperature measurements on
specific days at specific weather stations.

**3_analyse_data.R** -  Declares the core data analysis functions that will power
the tool and pre-calculates map polygons.  The functions calculate the (historic)
probability that the temperature at a certain time in a certain month will be
within a specified range.  The map polygon calculations simplify a map of over
3,000 county polygons into a small number of groups of counties. To improve
runtime, the tool only downloads data for a small, geographically stratified
sample of counties.  Each of new polygons groups together all counties that are
closest to the same sampled county.  This significantly improves tool runtime
because the tool needs to render far fewer polygons to refresh the map.  In
addition, this script also defines different ranges of hours that are ideal for
outside activity in the hotter and colder months of year.  In the summer, the
ranges favor the morning and evening.  In the winter, the ranges favor the
afternoon.

Inputs:
+ B_Intermediates/county_data.RData - Loaded for testing purposes (removal
candidate)
+ B_Intermediates/state_map.RData - Loaded for testing purposes (removal
candidate)
+ B_Intermediates/county_map.RData - The county polygons that will be merged into
groups to accelerate tool
+ B_Intermediates/weather_data.RData - Data on the historical temperature ranges
for each sampled county.

Outputs: A single file called app_support.RData, which holds the final data objects
and tool functions.  Packaging all needed data objects improves tool performance.
Objects include:
+ location_map - Polygons of groups of counties that are represented by the same
sampled county.
+ state_map - State polygons.
+ weather_data - Data on the historical temperature ranges
for each sampled county.
+ InterpolateTemperature() and ScoreLocation() - Projects the chances that the
temperature will fall within a specified range, given time range, month, and
location.  The InterpolateTemperature() function specifically takes daily min/max
temperatures and interpolate the temperature for each hour of the day.
+ GeneratePlot - Renders the projected probabilities of the weather falling
within a given temperature range.

**app.R** - This script is a shiny app, suitable for web hosting.  Users can
interact with the webpage-based tool to map the probability that the
weather will fall within specified temperature ranges in a specified month/time.

Inputs: app_support.RData, a package of objects and functions that support the
tool.

Outputs: An interactive webpage-based tool.  shinyapps.io hosts the live version
of the tool.

### Project Status and To-Dos

I am actively building the code for this project.  So far, the data pull code
(mostly API pulls) is complete but I have only downloaded a small segment of the
data.

- [ ] Determine if all inputs are necessary in 3_analyse_data.R
- [ ] Make sure gallery, README, and tool text are aligned
- [ ] Polish writing and grammar in all three sets of text
