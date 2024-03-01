# Packages ----------------------------------------------------------------

# Packages for Interactive Web application
library(shiny)
library(shinydashboard)
library(bs4Dash)
library(shinycssloaders)

# Packages for Data Cleaning/Processing
library(tidyverse)

# Packages for Chart Creation
library(psrcplot)
library(echarts4r)

# Packages for Map Creation
library(sf)
library(leaflet)

# Packages for Table Creation
library(DT)

# Package for Excel Data Creation
library(openxlsx)

# Run Modules Files ---------------------------------------------------------------------------
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)
source("functions.R")

# Page Information --------------------------------------------------------
left_panel_info <- read_csv("data/left_panel_information.csv", show_col_types = FALSE)
page_text <- read_csv("data/page_text.csv", show_col_types = FALSE)

# Inputs ---------------------------------------------------------------
wgs84 <- 4326
load_clr <- "#91268F"
latest_yr <- "2023"

# Data via RDS files ------------------------------------------------------
operator_data <- readRDS("data/operator_ferry_data.rds")
urban_area_data <- readRDS("data/urban_area_ferry_data.rds")
demographic_data <- readRDS("data/pums_ferry_data.rds")

# Visuals for App ---------------------------------------------------------
