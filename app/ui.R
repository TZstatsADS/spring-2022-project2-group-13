if (!require("shiny")) install.packages("shiny")
library(shiny)
if (!require("shinydashboard")) install.packages("shinydashboard")
library(shinydashboard)
if (!require("plotly")) { install.packages("plotly")}
library(plotly)
if(!require(fontawesome)) devtools::install_github("rstudio/fontawesome")
if(!require(highcharter)) devtools::install_github("jbkunst/highcharter")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
# if(!require(alberusa)) remotes::install_git("https://git.sr.ht/~hrbrmstr/albersusa")
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
library(albersusa)



# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  # Application title
  skin = "purple",
  dashboardHeader(title ="NYC EXPRESS",titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home",tabName = "Home", icon = icon("home")),
      
      menuItem("Map", tabName = "Map", icon = icon("chart-bar"),
               startExpanded = TRUE,
               menuSubItem("Covid Cases",tabName = "a"),
               menuSubItem("Dig by yourself",tabName = "b")),
      menuItem("Analysis", tabName = "Analysis", icon = icon("tasks"))
  )),
  
    dashboardBody(
    tabItems(
        tabItem(tabName = "Home",
                h2("To be continued")
        ),
        
        tabItem(tabName = "a",
                box(width=12,
                    h2('Map - Covid Cases per US State'),
                    h4("The map shows the cumulative and death cases per state in US."),
                    h4("Please click on the select box to explore by yourself."),
                    br(),
                    
              fluidPage(
                
                # Application title
                # titlePanel("Map - Covid Cases per US State"),
                
                
                # Sidebar with a slider input for date
                sidebarLayout(
                  sidebarPanel(
                    selectInput("choosenstat",
                                "Select cases to display:",
                                choices = list("Cases" = "Cases",
                                               "Deaths" = "Deaths"), 
                                selected = ""),
                    sliderInput("choosendate",
                                "Select a date to display:",
                                min = as.Date("2020-01-21","%Y-%m-%d"),
                                value = as.Date("2020-11-01","%Y-%m-%d"),
                                max = as.Date(format(Sys.Date()-2,"%Y-%m-%d")),
                                timeFormat = "%m-%d")
                  ),
                  
                  # Show map
                  mainPanel(
                    h3(textOutput("TitleText")),
                    plotlyOutput("map"),
                    h5("Data source:", 
                       tags$a(href="https://github.com/nytimes/covid-19-data", 
                              "Coronavirus (Covid-19) Data in the United States, "),
                       tags$a(href="https://www.kaggle.com/peretzcohen/2019-census-us-population-data-by-state", 
                              "2019_Census_US_Population_Data_By_State_Lat_Long"))
                  )
                )
              ))),
              
              
      tabItem(tabName = "b", 
        fluidPage(
          actionButton("free_meals","Free Meals Locations",icon=icon("utensils",  lib = "font-awesome"),style="color: #fff; background-color: #94a4db; border-color: #94a4db"),
          actionButton("health","Mental Health Service",icon=icon("hospital",  lib = "font-awesome"),style="color: #fff; background-color: #94a4db; border-color: #94a4db"),
          actionButton("textile","Textile Drop-Off Locations",icon=icon("tshirt",  lib = "font-awesome"),style="color: #fff; background-color: #94a4db; border-color: #94a4db"),
          actionButton("job","Directory Of Job Centers",icon=icon("briefcase",  lib = "font-awesome"),style="color: #fff; background-color: #94a4db; border-color: #94a4db"),
          br(),
          p("The plot won't update until the button is clicked.",
            " Without the use of ", code("isolate()"),
            " in server.R, the plot would update whenever the slider",
            " changes."),
          leafletOutput("mymap", width="100%", height=800))
      ),
      tabItem(tabName = "Analysis",
              h2("To be continued")
      )
    )
  )
))

