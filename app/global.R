library(shiny)
library(shinydashboard)
library(knitr)
library(dplyr)
library(sparkline)
library(highcharter)
library(jsonlite)
library(DT)
library(lazyeval)
library(FSA)
library(rstudioapi)
library(shinyWidgets)
library(shinyBS)
library(tidyr)
library(shinyjs)
library(DBI)
library(leaflet)
library(htmltools)
library(httr)



#Definition de la table qui contient les informations des trackers

Tracker_fleet = data.frame(username = c('wil_trem', 'joh_deg', 'sea_go'))
Tracker_fleet$name = c('William', 'John', 'Sean')
Tracker_fleet$surname = c('Tremendous', 'Degun', 'Gonet')
Tracker_fleet$age = c(23,23,22)
Tracker_fleet$channel_id = c('1180519','1180520', '1198494')
Tracker_fleet$read_key = c('DFMZL2TN2KEC7YTY','W6T1VY3J04MII447','2ZS9GMFT2VKXDO6Y')
Tracker_fleet$write_key = c('1ZZG6G2TPICBV5CG','Z42PSIWFBYOQ75JZ','LWV8JQK8SORIKFPJ')
Tracker_fleet$longitude = c(0, 0,0)
Tracker_fleet$latitude = c(0, 0,0)
Tracker_fleet$pace = c(0, 0,0)
Tracker_fleet$sex = c('M','M','M')