library(shiny)
library(shinydashboard)
library(DT)
library(knitr)
library(kableExtra)
library(ggplot2)
library(scales)
library(ggthemes)
library(DT)
library(shinyjs)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(timevis)
library(leaflet)
library(shinyWidgets)
library(jsonlite)
library(httr)
library(RCurl)
library(rhandsontable)
library(RMySQL)
library(DBI)
library(lpSolve)
library(highcharter)
library(leaflet)
library(ggmap)
library(geosphere)
library(sp)


options(shiny.trace=TRUE)

register_google(key = read_file("/home/jlowhorn/ShinyApps/Hackathon/www/eric_google_api_key.txt"))

source("/home/jlowhorn/ShinyApps/Hackathon/www/Sidebar.R",  local = TRUE)

source("/home/jlowhorn/ShinyApps/Hackathon/www/Body.R",  local = TRUE)

ui <- dashboardPage(
  dashboardHeader(title="KNX HX",
                  
                  tags$li(a(
                    href = 'http://knoxvilletn.gov/government/city_departments_offices/public_service/solid_waste/solid_waste_management_facility',
                    img(src = 'logo@2x.png',
                        title = "Go to Waste Management Home Page", height = "30px"),
                    style = "padding-top:10px; padding-bottom:10px;"),
                    class = "dropdown",
                    tags$style(HTML("hr {border-top: 2px solid #000000;}"))
                  ),
                  
                  dropdownMenuOutput("messageMenu")),
  
  sidebar,
  body,
  
  shinyjs::useShinyjs()
)
server <- function(input, output,session) {
  
  hide(id = "loading-content", anim = TRUE,time=1,animType = "fade")    
  show("app-content")
  
  
  source("/home/jlowhorn/ShinyApps/Hackathon/www/Waste.R",  local = TRUE)
  
 	source("/home/jlowhorn/ShinyApps/Hackathon/www/NewDataInput.R",  local = TRUE)
  
  source("/home/jlowhorn/ShinyApps/Hackathon/www/EditData.R",  local = TRUE)
  
 # source("/home/jlowhorn/ShinyApps/Hackathon/www/ForecastTab.R",  local = TRUE)
  
  source("/home/jlowhorn/ShinyApps/Hackathon/www/MapTab.R",  local = TRUE)
  
  
 
}
shinyApp(ui, server)

