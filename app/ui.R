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

linebreaks <- function(n){HTML(strrep(br(), n))}

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
               menuSubItem("Mental Health", tabName = "Interac_plot"),
               menuSubItem("Social Methods", tabName = "Interac_plot1"),
               menuSubItem("Covid New Case", tabName = "New"),
               menuSubItem("NYC transportation",tabName = "both"),
               menuSubItem("Only Subways",tabName = "subway"),
               menuSubItem("Only Buses", tabName = "bus"))
          )
  ),
  
    dashboardBody(
    tabItems(
        tabItem(tabName = "Home",
                tags$img(src = "https://richardkleincpa.com/wp-content/uploads/2014/01/new-york-city-street-wallpaper-copy.jpg",
                         style = 'position: absolute; opacity: 0.4;'),
                fluidPage(
                  fluidRow(width = 20, height=140,
                           linebreaks(5),
                           h1(style = "text-align: center; font-size = 70px;color:black",strong('Mental Health During the COVID-19 Pandemic')),
                           br(),
                           tags$blockquote(h2(style = "text-align: justify; font-size = 35px;color:#8f5dff",
                                              em('One in five'))),
                           tags$blockquote(h2(style = "text-align: justify; font-size = 35px;color:black",' New Yorkers experiences mental illness in a given year.')),
                           br(),
                           tags$blockquote(h2(style = "text-align: justify; font-size = 35px;color:#8f5dff",
                                              em('Hundreds of thousands'))),
                           tags$blockquote(h2(style = "text-align: justify; font-size = 35px;color:black",'of these New Yorkers are not connected to care.')),
                           hr(),
                           tags$blockquote(h2(style = "text-align: justify; font-size = 35px;color:black",'We are looking toward a city
                                              where more New Yorkers might be affacted by Covid-19 on mental health. 
                                              We are doing this by analyzing their job status and income status, 
                                              which are among the factors that may lead to mental problems. Similarly, 
                                              the use of drugs can also reflect mental state of citizens. Finally, we examined how older adults may 
                                              have changed their frequency of contact with others via various mode since the pandemic started, 
                                              and how these choices may be impacting their mental health by using data from the National Social Life, 
                                              Health and Aging Project (NSHAP).
                                              These key analysis might lead to some interesting insights.')),
                           hr(),
                           linebreaks(30)
                  ))),
        
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
              fluidPage(
              box(width=20,
                  h4("This bar plot shows the percentage of people in different aspect"),
                  h4("Please click on the select box to see more specific distribution by issues."),
                  h4("The left bar plot is the percentage before the beginning of Pandemic"),
                  h4('The right part is the percentage after the Pandemic'),
                  br(),
                  
                  
                  
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
                                    selected = "Physical Health")
                        ),
                      mainPanel(
                        plotlyOutput("bar_plt")
                      ))
                  
                  
              ),
              
      )),
      
      
      tabItem(tabName = "Interac_plot1",
              fluidPage(
                box(width=20,
                    h4("This bar plot shows the percentage of people in different aspect"),
                    h4("Please click on the select box to see more specific distribution by issues."),
                    h4("The left bar plot is the percentage before the beginning of Pandemic"),
                    h4('The right part is the percentage after the Pandemic'),
                    br(),
                    
                    
                    
                    sidebarLayout(
                      sidebarPanel(width = 12,
                                   
                                   selectInput("count1", 
                                               label = "SOCIAL",
                                               choices = c("Phone call" = 1,
                                                           "Email" = 2,  
                                                           "Video call" = 3,
                                                           "In person meet" = 4), 
                                               selected = "Physical Health"),
                                   
                                   radioButtons("count2", 
                                                label = HTML('<FONT color="red"><FONT size="5pt">Welcome</FONT></FONT><br> <b>Whom to connect ?</b>'),
                                                choices = list("Family connection" = 1, "Friends connection" = 2),
                                                selected = 1,
                                                inline = T,
                                                width = "100%")),
                      mainPanel(
                        plotlyOutput("bar_plt1")
                      ))
                    
                    
                ),
                
              )),
      
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

