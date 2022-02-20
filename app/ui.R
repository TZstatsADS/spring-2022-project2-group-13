#if (!require("shiny")) install.packages("shiny")
library(shiny)
#if (!require("shinydashboard")) install.packages("shinydashboard")
library(shinydashboard)
#if (!require("plotly")) { install.packages("plotly")}
library(plotly)
#if(!require(fontawesome)) devtools::install_github("rstudio/fontawesome")
#if(!require(highcharter)) devtools::install_github("jbkunst/highcharter")
#if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
# if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
library(albersusa)
library(leaflet)
library(lubridate)
library(googleVis)
library(plotly)



# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  # Application title
  skin = "purple",
  dashboardHeader(title ="NYC EXPRESS",titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home",tabName = "Home", icon = icon("home")),
      
      menuItem("Map", tabName = "Map", icon = icon("map"),
               startExpanded = TRUE,
               menuSubItem("Covid Cases",tabName = "a"),
               menuSubItem("Dig by yourself",tabName = "b")),
      menuItem("Analysis", tabName = "Analysis", icon = icon("chart-bar"),
               startExpanded = TRUE,
               menuSubItem("Covid New Case", tabName = "New"),
               menuSubItem("NYC transportation",tabName = "both"),
               menuSubItem("Only Subways",tabName = "subway"),
               menuSubItem("Only Buses", tabName = "bus"))
          )
  ),
  
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
      
      
      tabItem(tabName = "Interac_plot",
              box(width=20,
                  h4("This bar plot shows the percentage of people in different aspect"),
                  h4("Please click on the select box to see more specific distribution by issues."),
                  h4("The left bar plot is the percentage before the beginning of Pandemic"),
                  h4('The right part is the percentage after the Pandemic'),
                  br(),
                  
                  
                  fluidPage(
                    sidebarLayout(
                      sidebarPanel(width = 12,
                        
                        selectInput("count", 
                                    label = "Mental Health Issues",
                                    choices = c("Physical Health" = 1,
                                                "Halp on Task" = 2,  
                                                "Relation Happiness" = 3,
                                                "Emotional Support" = 4,
                                                "Mental Health" = 5,
                                                "Physical activity" = 6,
                                                "Rest time" = 7,
                                                "Alcohol Comsumption" = 8,
                                                "Tabacco Comsumption" = 9), 
                                    selected = "Physical Health")),
                      mainPanel(
                        plotlyOutput("bar_plt")
                      ))
                  )
                  
              ),
              
              box(
                width=6,
                h4("Please click on the select box to see more specific distribution of positive antibody tests number & rate."),
                br(),
                
                mainPanel(
                  fluidRow(
                    tabsetPanel(id = "tpanel",
                                type = "tabs",
                                tabPanel("number of test positive", plotlyOutput("plot1")),
                                tabPanel("percent of test positive", plotlyOutput("plot2")))
                  )
                ))
      ),
      
      tabItem(tabName = "subway", 
              fluidRow(htmlOutput("ggv_timeline_subway"), width=50, height=700)),
      
      tabItem(tabName = "bus",
              fluidRow(htmlOutput("ggv_timeline_bus"), width=50, height=700)
    ),
    tabItem(tabName = 'both',
            fluidRow(htmlOutput("ggv_timeline"), width=50, height=700)),
    tabItem(tabName = "New",
            fluidRow(htmlOutput("ggv_timeline_new"), width=50, height=700))
))
))

