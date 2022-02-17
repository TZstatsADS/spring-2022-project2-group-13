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



# Define UI for application that draws a histogram
shinyUI(dashboardPage(
  # Application title
  skin = "purple",
  dashboardHeader(title ="NYC EXPRESS",titleWidth = 350),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",tabName = "dashboard", icon = icon("dashboard")),
      
      menuItem("Map", tabName = "Map", icon = icon("chart-bar"),
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
        tabItem(tabName = "dashboard",
                h2("To be continued")
        ),
        
        tabItem(tabName = "a", 
              fluidPage(
                
                # Application title
                titlePanel("Map - Covid Cases per US State"),
                
                
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
              )),
              
              
      tabItem(tabName = "b", 
        fluidPage(
          actionButton("free_meals","Free Meals Locations",icon=icon("utensils",  lib = "font-awesome")),
          actionButton("health","Mental Health Service",icon=icon("utensils",  lib = "font-awesome")),
          actionButton("textile","Textile Drop-Off Locations",icon=icon("utensils",  lib = "font-awesome")),
          actionButton("job","Directory Of Job Centers",icon=icon("utensils",  lib = "font-awesome")),
          br(),
          p("The plot won't update until the button is clicked.",
            " Without the use of ", code("isolate()"),
            " in server.R, the plot would update whenever the slider",
            " changes."),
          leafletOutput("mymap", width="100%", height=800))
      ),
      
      
      tabItem(tabName = "Interac_plot",
              box(width=6,
                  h4("The bar chart below shows that Brooklyn and Queens have the most positive antibody cases."),
                  h4("Please click on the select box to see more specific distribution by borough."),
                  br(),
                  
                  
                  fluidPage(
                    sidebarLayout(
                      sidebarPanel(
                        
                        selectInput("count", 
                                    label = "Antibody by Borough in Bar Plot",
                                    choices = c("Number of Positive People" = 1,
                                                "Number of People Tested" = 2,  
                                                "Percentage of Positive Cases" = 3,
                                                "Test Rate" = 4), 
                                    selected = "Number of Positive People")),
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

