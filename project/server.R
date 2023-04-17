#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

# Select just cities in USA and Canada for plotting
temp_sub <- read.csv(file="NewYork2.csv")
city <- temp_sub[temp_sub$name,]
# Read in preprocessed data (temperature, descriptions, wind direction and speed)

pal_temp <- colorNumeric(palette = c("#6600ff","yellow","#cc0000"), domain = c(10,105))
pal_feel <- colorNumeric(palette = c("#6600ff","yellow","#cc0000"), domain = c(0,70))
pal_humi <- colorNumeric(palette = c("#6600ff","yellow","#cc0000"), domain = c(0,35))

# Server function begins here
shinyServer(function(input, output, session) {
  avg_per <- reactive({input$avg_period})
  time_per <- reactive({if (avg_per()=="Annual") {
    input$year_period 
  } else if (avg_per()=="Monthly") {
    input$month_period
  } else {
    "All"
  }
  })
  start_hour <- reactive({input$hrofday[1]})
  end_hour <- reactive({input$hrofday[2]})
  select_city <- reactive({input$map_marker_click$id})
  output$cityBox <- renderUI({
    if (is.null(select_city())) {
      infoBox("Selected City","None - select from map",icon=icon("city"),fill=TRUE,width=12)
    } else {
      infoBox("Selected City",select_city(),icon=icon("city"),fill=TRUE,width=12)
    }
  })
  output$sunrise <- renderUI({
    if (is.null(select_city())) {
      infoBox("Time of Sunrise","None - select from map",icon=icon("sun"),fill=TRUE,width=12)
    } else {
      infoBox("Time of Sunrise",select(filter(city,City==select_city()),Climate),icon=icon("sun"),fill=TRUE,width=12)
    }
  })
  temp_avg <- reactive({
    if (avg_per()=="Annual") {
      temp_sub %>% filter(Year==time_per()) %>% group_by(City) %>%
        summarize(Temp_avg=mean(Temp,na.rm=TRUE),O3_avg=mean(O3,na.rm=TRUE),NO2_avg=mean(NO2,na.rm=TRUE)) %>% 
        merge(city,by="City")
    } else if (avg_per()=="Monthly") {
      temp_sub %>% filter(grepl(as.character(time_per()),Month) ) %>%
        group_by(City) %>% summarize(Temp_avg=mean(Temp,na.rm=TRUE),O3_avg=mean(O3,na.rm=TRUE),
                                     NO2_avg=mean(NO2,na.rm=TRUE)) %>% merge(city,by="City")
    } else {
      temp_sub %>%  group_by(City) %>% 
        summarize(Temp_avg=mean(Temp,na.rm=TRUE),O3_avg=mean(O3,na.rm=TRUE),NO2_avg=mean(NO2,na.rm=TRUE)) %>% 
        merge(city,by="City")
    }
  })
  temp_city <- reactive({
    if (avg_per()=="Annual") {
      temp_sub %>% 
        filter(Year==time_per() & 
                 grepl(as.character(select_city()),City))
    } else if (avg_per()=="Monthly") {
      temp_sub %>% filter(grepl(as.character(time_per()),Month) &
                            grepl(as.character(select_city()),City))
    } else {
      temp_sub %>% filter(grepl(as.character(select_city()),City))
    }
  })
  output$sunset <- renderUI({
    if (is.null(select_city())) {
      infoBox("Time of Sunset",0,icon=icon("moon"),fill=TRUE,width=12)
    } else {
      infoBox("Time of Sunset",nrow(temp_city()),icon=icon("moon"),fill=TRUE,width=12)
    }
  })
  output$completeBox <- renderUI({
    if (is.null(select_city())) {
      infoBox("","N/A",icon=icon(""),fill=TRUE,width=12)
    } else {
      if (avg_per()=="Annual") {
        infoBox("",round(100*nrow(temp_city())),
                icon=icon(""),fill=TRUE,width=12)
      } else if (avg_per()=="Monthly") {
        if (time_per()=="February") {
          infoBox("",round(100*nrow(temp_city())),
                  icon=icon(""),fill=TRUE,width=12)
        } else if (time_per() %in% c("April","June","September","November")) {
          infoBox("",round(100*nrow(temp_city())),
                  icon=icon(""),fill=TRUE,width=12)
        } else {
          infoBox("",round(100*nrow(temp_city())),
                  icon=icon(""),fill=TRUE,width=12)
        }
      } else {
        infoBox("",round(100*nrow(temp_city())),
                icon=icon(""),fill=TRUE,width=12)
      }
    }    
  })

}
)
 