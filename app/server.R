if (!require("shiny")) install.packages("shiny")
library(shiny)
if (!require("shinydashboard")) install.packages("shinydashboard")
library(shinydashboard)
shinyServer(function(input, output) {
if (!require("leaflet")) { install.packages("leaflet")}
library(leaflet)
if (!require("dplyr")) { install.packages("dplyr")}
library(dplyr)
if (!require("tidyverse")) { install.packages("tidyverse")}
library(tidyverse)
if (!require("DT")) { install.packages("DT")}
library(DT)
if (!require("ggplot2")) { install.packages("ggplot2")}
library(ggplot2)
if (!require("lubridate")) { install.packages("lubridate")}
library(lubridate)
if (!require("plotly")) { install.packages("plotly")}
library(plotly)
if (!require("hrbrthemes")) { install.packages("hrbrthemes")}
library(hrbrthemes)
if (!require("highcharter")) { install.packages("highcharter")}
library(highcharter)
if (!require("RColorBrewer")) { install.packages("RColorBrewer")}
library(RColorBrewer)
if(!require(fontawesome)) devtools::install_github("rstudio/fontawesome")
if (!require("geojsonio")) { install.packages("geojsonio")}
library(geojsonio)
if (!require("readr")) { install.packages("readr")}
library(readr)
remotes::install_git("https://git.sr.ht/~hrbrmstr/albersusa")
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
library(albersusa) 
if (!require("lubridate")) {install.packages("lubridate")}
if (!require("googleVis")) {install.packages("googleVis")}
library(lubridate)
library(googleVis)
    
    
    
    
# setwd('/Users/users/Desktop/test/test11')
###map for covid

my_map_theme <- function(){
    theme(panel.background=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank())
}

us_states <- usa_sf("laea")

us_states_covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", col_types = cols(date = col_date(format = "%Y-%m-%d")))
# us_states_covid$state
state_pop_millions <- read.csv('2019_Census_US_Population_Data_By_State_Lat_Long.csv')
# state_pop_millions
state_pop_millions <- state_pop_millions %>%
    rename(`state` = 'STATE',
           `population_millions` = 'POPESTIMATE2019')

covid_data <- us_states_covid %>%
    left_join(state_pop_millions, c("state" = "state")) %>%
    mutate(case_rate = (cases*1000000/population_millions)) %>%
    mutate(death_rate = (deaths*1000000/population_millions))

fullmap <- function(myrate, mydate){
    if(myrate == "Cases"){
        testday <- covid_data %>%
            filter(date == as.Date(mydate))
        
        map <- us_states %>%
            left_join(testday, c("name"="state"))
        
        covid_map <- ggplot(map) +
            geom_sf(aes(fill=case_rate)) +
            scale_fill_continuous(low=munsell::mnsl("5P 7/12"), high =munsell::mnsl("5P 2/12"), name = "cases per million people") 
        
        covid_map2 <- map %>%
            mutate(text = paste("<b>",name,"</b>\n", round(case_rate, digits = 2))) %>%
            ggplot() +
            geom_sf(aes(fill=case_rate, text=text), color="black") +
            scale_fill_continuous(low=munsell::mnsl("5P 7/12"), high =munsell::mnsl("5P 2/12"), name = "Cases per Million") +
            my_map_theme()
        
        
        covid_map3 <- ggplotly(covid_map2,tooltip = "text") %>%
            style(hoveron = "fills")
        
        covid_map3
    }
    else {
        testday <- covid_data %>%
            filter(date == as.Date(mydate))
        
        map <- us_states %>%
            left_join(testday, c("name"="state"))
        
        covid_map <- ggplot(map) +
            geom_sf(aes(fill=death_rate)) +
            scale_fill_continuous(low=munsell::mnsl("5P 7/12"), high =munsell::mnsl("5P 2/12"), name = "Deaths per Million")
        
        covid_map2 <- map %>%
            mutate(text = paste("<b>",name,"</b>\n", round(death_rate, digits = 2))) %>%
            ggplot() +
            geom_sf(aes(fill=death_rate, text=text), color="black") +
            scale_fill_continuous(low=munsell::mnsl("5P 7/12"), high =munsell::mnsl("5P 2/12"), name = "Deaths per Million") +
            my_map_theme()
        
        
        covid_map3 <- ggplotly(covid_map2,tooltip = "text") %>%
            style(hoveron = "fills")
        
        covid_map3
    }
}

output$TitleText <- renderText(paste("Cumulative Covid ",input$choosenstat, "per Million as of ", input$choosendate))

output$map <- renderPlotly({
    fullmap(input$choosenstat,input$choosendate)
})







### Map ###
output$mymap <- renderLeaflet({ 
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
        htmlwidgets::onRender(
            "function(el, x) {
                    L.control.zoom({ position: 'bottomright' }).addTo(this)
                }"
        ) %>%
        addProviderTiles("CartoDB.Voyager") %>%
        setView(lng = -73.935242, lat = 40.730610, zoom = 11)})

# #testing covid
# df_testing<-read.csv('https://data.cityofnewyork.us/resource/7a57-qgkz.csv',stringsAsFactors = FALSE)
# 
# observeEvent(input$testing, {
#     proxy <- leafletProxy("mymap", data = df_testing)
#     proxy %>% clearControls()
#     
#     # clear the map
#     leafletProxy("mymap", data = df_testing) %>%
#         clearShapes() %>%
#         clearMarkers() %>%
#         addProviderTiles("CartoDB.Voyager") %>%
#         fitBounds(-74.354598, 40.919500, -73.761545, 40.520024)
#     
#     leafletProxy("mymap", data = df_testing) %>%
#         addAwesomeMarkers(~longitude, ~latitude,
#                           icon = awesomeIcons(markerColor = "blue",
#                                               text = fa("hospital")), label= ~assigned_vendor,
#                           popup = paste(
#                               "<b>Address:</b>", df_health$street_1,", ", df_health$city,  "<br>",
#                               "<b>Phone:</b>", df_health$phone, "<br>",
#                               "<b>Website:</b>", df_health$website, "<br>"))
# })




# free meals
df_meals<-read.csv('https://data.cityofnewyork.us/resource/sp4a-vevi.csv', stringsAsFactors = FALSE)

observeEvent(input$free_meals, {
    proxy <- leafletProxy("mymap", data = df_meals)
    palette_fm = c("#aebbff","#92b2ff", "#8ad3ff")
    color1 <- colorFactor(palette =palette_fm, df_meals$accessibility)
    proxy %>% clearControls()
    
    # clear the map
    leafletProxy("mymap", data = df_meals) %>%
        clearShapes() %>%
        clearMarkers() %>%
        addProviderTiles("CartoDB.Voyager") %>%
        fitBounds(-74.354598, 40.919500, -73.761545, 40.520024)
    
    leafletProxy("mymap", data = df_meals)%>%
        clearShapes() %>%
        addProviderTiles("CartoDB.Voyager") %>%    
        addCircleMarkers(~longitude, ~latitude, radius=10,
                         color = ~color1(accessibility),
                         label = paste(df_meals$schoolname, ', ', df_meals$siteaddress,', ', df_meals$city))%>%
        addLegend("bottomright",
                  pal = color1,
                  values = df_meals$accessibility,
                  title = "Status",
                  opacity = 1)
})



# health center button
df_health<-
    read.csv("https://data.cityofnewyork.us/resource/8nqg-ia7v.csv", stringsAsFactors = FALSE)

observeEvent(input$health, {
    proxy <- leafletProxy("mymap", data = df_health)
    proxy %>% clearControls()
    
    # clear the map
    leafletProxy("mymap", data = df_health) %>%
        clearShapes() %>%
        clearMarkers() %>%
        addProviderTiles("CartoDB.Voyager") %>%
        fitBounds(-74.354598, 40.919500, -73.761545, 40.520024)
    
    leafletProxy("mymap", data = df_health) %>%
        addAwesomeMarkers(~longitude, ~latitude,
                          icon = awesomeIcons(markerColor = "blue",
                                              text = fa("hospital")), label= ~name_1,
                          popup = paste(
                              "<b>Address:</b>", df_health$street_1,", ", df_health$city,  "<br>",
                              "<b>Phone:</b>", df_health$phone, "<br>",
                              "<b>Website:</b>", df_health$website, "<br>"))
})


# textile
df_textile<-read.csv('https://data.cityofnewyork.us/resource/qnjm-wvu5.csv')

observeEvent(input$textile, {
    proxy <- leafletProxy("mymap", data = df_textile)
    proxy %>% clearControls()
    
    # clear the map
    leafletProxy("mymap", data = df_textile) %>%
        clearShapes() %>%
        clearMarkers() %>%
        addProviderTiles("CartoDB.Voyager") %>%
        fitBounds(-74.354598, 40.919500, -73.761545, 40.520024)
    
    leafletProxy("mymap", data = df_textile) %>%
        addAwesomeMarkers(~longitude, ~latitude,
                          icon = awesomeIcons(markerColor = "blue",
                                              text = fa("tshirt")), label= ~items_accepted,
                          popup = paste(
                              "<b>Address:</b>", df_textile$address,", ", df_textile$borough,  "<br>",
                              "<b>Phone:</b>", df_textile$public_phone, "<br>",
                              "<b>Email:</b>", df_textile$public_email, "<br>"))
})

# job center button
df_job<-read.csv('https://data.cityofnewyork.us/resource/9d9t-bmk7.csv', stringsAsFactors = FALSE)

observeEvent(input$job, {
    proxy <- leafletProxy("mymap", data = df_job)
    proxy %>% clearControls()
    
    # clear the map
    leafletProxy("mymap", data = df_job) %>%
        clearShapes() %>%
        clearMarkers() %>%
        addProviderTiles("CartoDB.Voyager") %>%
        fitBounds(-74.354598, 40.919500, -73.761545, 40.520024)
    
    leafletProxy("mymap", data = df_job) %>%
        addAwesomeMarkers(~longitude, ~latitude,
                          icon = awesomeIcons(markerColor = "blue",
                                              text = fa("briefcase")), label= ~facility_name,
                          popup = paste(
                              "<b>Address:</b>", df_job$street_address,", ", df_job$city,  "<br>",
                              "<b>Phone:</b>", df_job$phone_number_s, "<br>",
                              "<b>Comment:</b>", df_job$comments, "<br>"))
})

#



})


# Subway data
shinyServer(function(input, output) {
    data_by_day <- read.csv("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/data-by-day.csv", stringsAsFactors = FALSE)
    data_subway <- read.csv("https://new.mta.info/document/20441")
    subway <- data_subway%>%select(Date, `Subways..Total.Estimated.Ridership`)%>%
        mutate(Date = ymd(as.Date(Date, "%m/%d/%Y")))%>%
        mutate(weekday = weekdays(Date))%>%filter(weekday != "Saturday")%>%
        filter(weekday != "Sunday")%>%
        rename(ridership = `Subways..Total.Estimated.Ridership`)%>%
        select(-weekday)
    
    bus <- data_subway%>%select(Date, `Buses..Total.Estimated.Ridership`)%>%
        mutate(Date = ymd(as.Date(Date, "%m/%d/%Y")))%>%
        rename(ridership = `Buses..Total.Estimated.Ridership`)%>%
        mutate(weekday = weekdays(Date))%>%filter(weekday != "Saturday")%>%
        filter(weekday != "Sunday")%>%
        select(-weekday)
    
    both <- data_subway%>%select(Date, `Subways..Total.Estimated.Ridership`,
                                 `Buses..Total.Estimated.Ridership`)%>%
        mutate(Date = ymd(as.Date(Date, "%m/%d/%Y")))%>%
        rename(Bus = `Buses..Total.Estimated.Ridership`,
               Subway = `Subways..Total.Estimated.Ridership`)%>%
        mutate(weekday = weekdays(Date))%>%filter(weekday != "Saturday")%>%
        filter(weekday != "Sunday")%>%
        select(-weekday)
    subway_perc <- data_subway%>%select(Date, `Subways..Total.Estimated.Ridership`)%>%
        mutate(Date = ymd(as.Date(Date, "%m/%d/%Y")))%>%
        rename(ridership = `Subways..Total.Estimated.Ridership`)%>%
        mutate(perc = lead(ridership,1))%>%na.omit%>%
        mutate(perc = (ridership-perc)/perc *100)%>%select(-ridership)
    both_perc <- data_by_day%>%select(date_of_interest, CASE_COUNT)%>%
        mutate(Date = ymd(as.Date(date_of_interest,"%m/%d/%Y")))%>%
        mutate(perc_covid = lead(CASE_COUNT,1))%>%na.omit%>%
        mutate(perc_covid = (CASE_COUNT-perc_covid)/perc_covid *100)%>%
        inner_join(subway_perc)%>%
        select(Date, perc_covid,perc)%>%
        pivot_longer(cols = c("perc_covid","perc"))
    
    g_timeline_subway = reactive({subway})
    g_timeline_bus = reactive({bus})
    g_timeline_both = reactive({both%>%pivot_longer(cols = c("Subway", "Bus"))})
    

    output$ggv_timeline_subway = renderGvis({
        gvisAnnotationChart(g_timeline_subway() ,
                             datevar="Date",
                             numvar="ridership",
                            options=list(
                                title= "Subway",
                                width= '95%',
                                height= 500))
        
    
})

    output$ggv_timeline_bus = renderGvis({
        gvisAnnotationChart(g_timeline_bus() ,
                            datevar="Date",
                            numvar="ridership",
                            options=list(
                                title= "Bus",
                                width= '95%',
                                height= 500))
        
        
    })
    output$ggv_timeline = renderGvis({
        gvisAnnotationChart(g_timeline_both() ,
                            datevar="Date",
                            numvar="value",
                            idvar = "name",
                            options=list(
                                title= "Bus",
                                width= '95%',
                                height= 500))
 
    })
    
    g_timeline_covid_sub <- reactive({data_by_day%>%select(date_of_interest, CASE_COUNT)%>%
            mutate(Date = ymd(as.Date(date_of_interest,"%m/%d/%Y")))%>%
            inner_join(subway)%>%select(Date,CASE_COUNT,ridership)%>%
            rename(New_Case = CASE_COUNT)%>%
            pivot_longer(cols = c("New_Case", ridership))})
    
    g_timeline_covid <- reactive({data_by_day%>%select(date_of_interest, CASE_COUNT)%>%
            mutate(Date = ymd(as.Date(date_of_interest,"%m/%d/%Y")))})
        

    output$ggv_timeline = renderGvis({
        gvisAnnotationChart(g_timeline_both() ,
                            datevar="Date",
                            numvar="value",
                            idvar = "name",
                            options=list(
                                title= "Bus",
                                width= '95%',
                                height= 500))
        
    })
    
    output$ggv_timeline_new = renderGvis({
        gvisAnnotationChart(g_timeline_covid() ,
                            datevar="Date",
                            numvar="CASE_COUNT",
                            options=list(
                                title= "Bus",
                                width= '95%',
                                height= 500))
        
    })

})

