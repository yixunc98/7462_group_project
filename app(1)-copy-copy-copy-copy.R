#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(leaflet)
library(lubridate)
library(RColorBrewer)
library(clifro)
library(hexbin)
library(leaflet)
library(splitstackshape)
library(bslib)
library(wesanderson)

# input data
weather <- read.csv("/Users/jiayulin/Desktop/weather_new.csv") # generated in the web: https://www.visualcrossing.com/weather/weather-data-services
city <- read.csv("/Users/jiayulin/Desktop/CityGeo.csv")        # manually collected from web
info <- read.csv("/Users/jiayulin/Desktop/City_Info.csv")      # manually collected from web

# clean the dataset and create new variables
info <- info[1:30,1:12] # delet the NAs
city <-city[,-1]
city <- na.omit(city)
usa_can_cities <- unlist(lapply(city['city'],as.character))
weather$year = year(weather$datetime)
weather$month = month(weather$datetime)
weather$day = day(weather$datetime)
weather_split <- cSplit(weather,"name",",")
weather_split$city <- weather_split$name_1
weather_split$season <- case_when(weather_split$month %in% c(1, 2, 12) ~ "winter",
                                  weather_split$month %in% c(3, 4, 5) ~ "spring",
                                  weather_split$month %in% c(6, 7, 8) ~ "summer",
                                  weather_split$month %in% c(9, 10, 11) ~ "fall") 
avg_period_choices <- c("2-year","Annual")
years <- c(2022:2023)


# subtitle of shiny app
header <- dashboardHeader(
  
  ## add the name of the app
  title = "Weather App",
  dropdownMenu(
    type = "messages",
    
    ## add two external web links in the menu bar
    messageItem(
      from = "Find More!",
      message = "You can find more weather data here!",
      href = "https://www.visualcrossing.com/weather/weather-data-services"
    ),
    messageItem(
      from = "Google map!",
      message = "Find your way.",
      href = "https://www.google.com/maps"
  )
))

sidebar <- dashboardSidebar(
  selectizeInput(inputId="avg_period",
                 label="Averaging Period",
                 choices=avg_period_choices),
  conditionalPanel(
    condition = "input.avg_period=='Annual'",
    selectizeInput(inputId="year_period",
                   label="Time Period Selection",
                   choices=years)
  ),
  radioButtons("pol","Histogram Display:",
               c("Temperature " = "temp",
                 "Temperature Feels Like" = "feel",
                 "Humidity" = "Hum"))
)

body <- dashboardBody( 
  leafletOutput("map", height = "350px"),
  fluidRow(
    tabBox(
      ## format the main tab
      title = "Find",
      id = "tabset1", 
      height = "1000px",
      width = "800px",
      ## format the tab1: weather condition
      tabPanel("(●’◡’●)ﾉ Weather Condition",
               valueBoxOutput("weather_box1",width = 4),
               valueBoxOutput("clearday1",width = 4),
               valueBoxOutput("rainday1",width = 4),
               valueBoxOutput("weather_box2",width = 4),
               valueBoxOutput("clearday2",width = 4),
               valueBoxOutput("rainday2",width = 4),
               valueBoxOutput("weather_box3",width = 4),
               valueBoxOutput("clearday3",width = 4),
               valueBoxOutput("rainday3",width = 4),
               valueBoxOutput("weather_box4",width = 4),
               valueBoxOutput("clearday4",width = 4),
               valueBoxOutput("rainday4",width = 4),
               plotOutput("dist")),
      
      ## format the tab2: famous spots
      tabPanel("(●’◡’●)ﾉ Famous Spots", 
               textOutput("space3"),
               textOutput("citybox"),
               textOutput("space1"),
               textOutput("number"),
               textOutput("space2"),
               textOutput("spot1"),
               textOutput("intro1"),
               textOutput("spot2"),
               textOutput("intro2"),
               textOutput("spot3"),
               textOutput("intro3"),
               textOutput("spot4"),
               textOutput("intro4"),
               
               ## add style of the text in tab2
                tags$head(tags$style("#citybox{color: #1344a0;
                                 font-size: 40px;
                                 font-weight: bold;
                                 }",
                                    "#number{color: Grey;
                                 font-size: 20px;
                                 font-weight: bold;
                                 }",
                                    "#spot1{color: CornflowerBlue;
                                 font-size: 20px;
                                 
                                 font-weight: bold;
                                 background-color: #fffefa;
                                 }",
                                    "#intro1{color: LightSteelBlue;
                                 font-size: 18px;
                                 font-style: italic;
                                 font-weight: bold;
                                 }",
                                    "#spot2{color: CornflowerBlue;
                                 font-size: 20px;
                                 font-weight: bold;
                                 background-color: #fffefa;
                                 }",
                                    "#intro2{color: LightSteelBlue;
                                 font-size: 18px;
                                 font-style: italic;
                                 font-weight: bold;
                                 }" ,
                                    "#spot3{color: CornflowerBlue;
                                 font-size: 20px;
                                 font-weight: bold;
                                 background-color: #fffefa;
                                 }",
                                    "#intro3{color: LightSteelBlue;
                                 font-size: 18px;
                                 font-style: italic;
                                 font-weight: bold;
                                 }" ,
                                    "#spot4{color: CornflowerBlue;
                                 font-size: 20px;
                                 font-weight: bold;
                                 background-color: #fffefa;
                                 }",
                                    "#intro4{color: LightSteelBlue;
                                 font-size: 18px;
                                 font-style: italic;
                                 font-weight: bold;
                                 }" ,
                                    "#space1{color: white;
                                 font-size: 15px;
                                 font-style: italic;
                                 font-weight: bold;
                                 }" ,
                                    "#space2{color: white;
                                 font-size: 6px;
                                 font-style: italic;
                                 font-weight: bold;
                                 }" ,
                                    "#space3{color: white;
                                 font-size: 6px;
                                 font-style: italic;
                                 font-weight: bold;
                                 }" 
                                    
               )
               ))
    )
)
)


ui <- dashboardPage(
    skin = "black", 
  header = header,
  sidebar = sidebar,
  body = body, 
 # tags$head( tags$style(HTML(".skin-black .main-sidebar {background-color:#9dd0e1;}")) ) 
 ## here you can change more appearance of the app
  )


server <- function(input, output) {
  ## select a period of interest
  avg_per <- reactive({input$avg_period})
  ## define the selected peiod as 'time_per'
  time_per <- reactive({if (avg_per()=="Annual") {
    input$year_period 
  }  else {
    "All"
  }
  })
  ## output map
  output$map <- renderLeaflet({
    leaflet(data = city) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude, label = ~city, popup = ~city)

  })
  ## city name selected by clicking on the map (defined by lat and lng)
  select_city <- reactive({lat <- input$map_marker_click$lat
                           lng <- input$map_marker_click$lng
                           select_city <- city[city$latitude == lat,]$city})
  ## filter the dataset that satisfied the selection of the period
  data_chose <- reactive({
    if (avg_per()=="Annual") {
      weather_split %>% filter(year==time_per()) 
    }  else {
      weather_split 
    }
  })
  ## filter the city's information dataset that satisfied the selection from clicking the map
  info_chose <- reactive({
    lat <- input$map_marker_click$lat
    lng <- input$map_marker_click$lng
    info_chose <- info[info$latitude == lat,]})
  ## Generate the output of the mean value of the selection of (temp, temp feels like, humidity) in tab1 (box part) in certain season
  ### Note that valid colors here are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  output$weather_box1 <- renderValueBox({
    valueBox(
      value = round(mean(data_chose()[data_chose()$city == select_city()& data_chose()$season == "winter" ,]$temp),2),
      subtitle = "Mean value in Winter",
      icon = icon("snowflake"),
      color = "light-blue"
    )
  })
  output$weather_box2 <- renderValueBox({
    valueBox(
      value = round(mean(data_chose()[data_chose()$city == select_city()& data_chose()$season == "spring",]$temp),2),
      subtitle = "Mean value in Spring",
      icon = icon("mountain"),
      color = "olive"
    )
  })
  output$weather_box3 <- renderValueBox({
    valueBox(
      value = round(mean(data_chose()[data_chose()$city == select_city()& data_chose()$season == "summer",]$temp),2),
      subtitle = "Mean value in Summer",
      icon = icon("leaf"),
      color = "red"
    )
  })
  output$weather_box4 <- renderValueBox({
    valueBox(
      value = round(mean(data_chose()[data_chose()$city == select_city()& data_chose()$season == "fall",]$temp),2),
      subtitle = "Mean temperature in Fall",
      icon = icon("apple"),
      color = "yellow"
    )
  })
  ## generate the the output in tab1 (box part), showing the percent of the clear day or rain/cloudy day in certain season
  ### clear day for winter
  output$clearday1 <- renderValueBox({
    valueBox(
      value = round(nrow(data_chose()[data_chose()$city == select_city() & 
                                    data_chose()$season == "winter" & 
                                    data_chose()$conditions == "Clear",])/
                 nrow(data_chose()[data_chose()$city == select_city() & data_chose()$season == "fall",]),2),
      subtitle = "Percent of clear day in Winter",
      icon = icon("sun"),
      color = "light-blue"
    )
  })
  ### clear day for spring
  output$clearday2 <- renderValueBox({
    valueBox(
      value = round(nrow(data_chose()[data_chose()$city == select_city() & 
                                        data_chose()$season == "spring" & 
                                        data_chose()$conditions == "Clear",])/
                      nrow(data_chose()[data_chose()$city == select_city() & data_chose()$season == "fall",]),2),
      subtitle = "Percent of clear day in Spring",
      icon = icon("sun"),
      color = "olive"
    )
  })
  ### clear day for summer
  output$clearday3 <- renderValueBox({
    valueBox(
      value = round(nrow(data_chose()[data_chose()$city == select_city() & 
                                        data_chose()$season == "summer" & 
                                        data_chose()$conditions == "Clear",])/
                      nrow(data_chose()[data_chose()$city == select_city() & data_chose()$season == "fall",]),2),
      subtitle = "Percent of clear day in Summer",
      icon = icon("sun"),
      color = "red"
    )
  })
  ### clear day for fall
  output$clearday4 <- renderValueBox({
    valueBox(
      value = round(nrow(data_chose()[data_chose()$city == select_city() & 
                                        data_chose()$season == "fall" & 
                                        data_chose()$conditions == "Clear",])/
                      nrow(data_chose()[data_chose()$city == select_city() & data_chose()$season == "fall",]),2),
      subtitle = "Percent of clear day in Fall",
      icon = icon("sun"),
      color = "yellow"
    )
  })
  ### rainy/cloudy day for winter
  output$rainday1 <- renderValueBox({
    valueBox(
      value = round(nrow(data_chose()[data_chose()$city == select_city() & 
                                        data_chose()$season == "winter" & 
                                        data_chose()$conditions %in% c("Rain, Partially cloudy", "Partially cloudy"),])/
                      nrow(data_chose()[data_chose()$city == select_city() & data_chose()$season == "fall",]),2),
      subtitle = "Percent of rain/cloudy day in Winter",
      icon = icon("umbrella"),
      color = "light-blue"
    )
  })
  ### rainy/cloudy day for spring
  output$rainday2 <- renderValueBox({
    valueBox(
      value = round(nrow(data_chose()[data_chose()$city == select_city() & 
                                        data_chose()$season == "spring" & 
                                        data_chose()$conditions %in% c("Rain, Partially cloudy", "Partially cloudy"),])/
                      nrow(data_chose()[data_chose()$city == select_city() & data_chose()$season == "fall",]),2),
      subtitle = "Percent of rain/cloudy day in Spring",
      icon = icon("umbrella"),
      color = "olive"
    )
  })
  ### rainy/cloudy day for summer
  output$rainday3 <- renderValueBox({
    valueBox(
      value = round(nrow(data_chose()[data_chose()$city == select_city() & 
                                        data_chose()$season == "summer" & 
                                        data_chose()$conditions %in% c("Rain, Partially cloudy", "Partially cloudy"),])/
                      nrow(data_chose()[data_chose()$city == select_city() & data_chose()$season == "fall",]),2),
      subtitle = "Percent of rain/cloudy day in Summer",
      icon = icon("umbrella"),
      color = "red"
    )
  })
  ### rainy/cloudy day for fall
  output$rainday4 <- renderValueBox({
    valueBox(
      value = round(nrow(data_chose()[data_chose()$city == select_city() & 
                                        data_chose()$season == "fall" & 
                                        data_chose()$conditions %in% c("Rain, Partially cloudy", "Partially cloudy"),])/
                      nrow(data_chose()[data_chose()$city == select_city() & data_chose()$season == "fall",]),2),
      subtitle = "Percent of rain/cloudy day in Fall",
      icon = icon("umbrella"),
      color = "yellow"
    )
  })
  ## 
  observeEvent(input$map_marker_click,{
    lat <- input$map_marker_click$lat
    lng <- input$map_marker_click$lng
    select_city <- city[city$latitude == lat,]$city
    print(select_city)}
  ) 
  ## generate the interaction figure for temp/temp feels like/humidity in tab1
  observeEvent(input$map_marker_click, {
    output$dist <- renderPlot({
      switch(input$pol,
             ### figure for temperature
             temp = data_chose() %>% 
               filter(city == select_city()) %>%
               group_by(month) %>%
               mutate(mean_temp = mean(temp)) %>%
               ggplot(aes(factor(month), mean_temp)) +
               geom_jitter(aes(factor(month), temp, color = temp), alpha = 0.2) +
               geom_crossbar(aes(xmin = month-0.3, xmax = month+0.3),
                             size=0.5,col="gray24", width = .01) +
               labs(title="Temperature Change", 
                    subtitle= "Plot of temperature in month",
                    caption="gray bar: the mean temperatue in month",
                    x="Month",
                    y="Temperature (°C)") +
               theme_bw() +
               scale_color_gradient(low="blue", high="red"),
            ### figure for temperature feels like
             feel = data_chose() %>% 
               filter(city == select_city()) %>%
               group_by(month) %>%
               mutate(mean_feelslike = mean(feelslike)) %>%
               ggplot(aes(factor(month), mean_feelslike)) +
               geom_jitter(aes(factor(month), feelslike, color = feelslike), alpha = 0.2) +
               geom_crossbar(aes(xmin = month-0.3, xmax = month+0.3),
                             size=0.5,col="gray24", width = .01) +
               labs(title="Temperature Feels Like Change", 
                    subtitle= "Plot of temperature feels like in month",
                    caption="gray bar: the mean temperatue feels like in month",
                    x="Month",
                    y="Temperature (°C)") +
               theme_bw() +
               scale_color_gradient(low="blue", high="red"),
             ### Figure for humidity
             Hum = data_chose() %>% 
               filter(city == select_city()) %>%
               group_by(month) %>%
               mutate(mean_humidity = mean(humidity)) %>%
               ggplot(aes(factor(month), mean_humidity)) +
               geom_jitter(aes(factor(month), humidity, color = humidity), alpha = 0.2) +
               geom_crossbar(aes(xmin = month-0.3, xmax = month+0.3),
                             size=0.5,col="gray24", width = .01) +
               labs(title="Temperature Humidity Change", 
                    subtitle= "Plot of humidity in month",
                    caption="gray bar: the mean humidity in month",
                    x="Month",
                    y="Humidity (%)") +
               theme_bw() +
               scale_color_gradient(low="white", high="blue")
             )
    })
  })
  ## Generate the text output in tab2
  ### city's name
  output$citybox <- renderText({
    paste("✧*", select_city(), "✧*")
  })
  ### Number of Visitors in 2019
  output$number <- renderText({
      paste("Number of Visitors in 2019 (before pandemic) : ",info_chose()$`visitor.number.before.pandemic..2019.`)
  })
  ### name of suggested spot 1
  output$spot1 <- renderText({
    paste("☆ Must go :",info_chose()$plot1)
  })
  ### information of the suggested spot 1
  output$intro1 <- renderText({
    paste("· · · · · · · · · · · · ", info_chose()$plot1_txt)
  })
  ### name of suggested spot 2
  output$spot2 <- renderText({
    paste("☆ Must go :",info_chose()$plot2)
  })
  ### information of the suggested spot 2
  output$intro2 <- renderText({
    paste("· · · · · · · · · · · · ", info_chose()$plot2_txt)
  })
  ### name of suggested spot 3
  output$spot3 <- renderText({
    paste("☆ Must go :",info_chose()$plot3)
  })
  ### information of the suggested spot 3
  output$intro3 <- renderText({
    paste("· · · · · ·  · · · · · · ", info_chose()$plot3_txt)
  })
  ### name of suggested spot 4
  output$spot4 <- renderText({
    paste("☆ Must go :",info_chose()$plot4)
  })
  ### information of the suggested spot 4
  output$intro4 <- renderText({
    paste("· · · · · · · · · · · · ", info_chose()$plot4_txt)
  })
  ### add some space between texts
  output$space1 <- renderText({
    paste("· · · · · · · · · · · · · · · · · · · · · · · · ")
  })
  ### add some space between texts
  output$space2 <- renderText({
    paste("· · · · · · · · · · · · · · · · · · · · · · · · ")
  })
  ### add some space between texts
  output$space3 <- renderText({
    paste("· · · · · · · · · · · · · · · · · · · · · · · · ")
  })

}


# Run the application 
shinyApp(ui = ui, server = server)

