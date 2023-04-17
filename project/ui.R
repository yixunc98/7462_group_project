#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

city <- read.csv(file="NewYork2.csv")

avg_period_choices <- c("2-year","Annual","Monthly")

years <- c(2022:2023)
months <- c("January","February","March","April","May","June",
            "July","August","September","October","November","December")

shinyUI(dashboardPage(
  skin="black", 
  dashboardHeader(title = "Weather App"),
  dashboardSidebar(
    selectizeInput(inputId="avg_period",
                   label="Averaging Period",
                   choices=avg_period_choices),
    conditionalPanel(
      condition = "input.avg_period=='Annual'",
      selectizeInput(inputId="year_period",
                     label="Time Period Selection",
                     choices=years)
    ),
    conditionalPanel(
      condition = "input.avg_period=='Monthly'",
      selectizeInput(inputId="month_period",
                     label="Time Period",
                     choices=months)
    ),
    radioButtons("pol","Map and Histogram Display:",
                 c("Temperature (deg F)" = "temp",
                   "Temperature Feels Like (deg F)" = "O3",
                   "Humidity" = "NO2"))
  ),
  dashboardBody(
    tags$head(tags$style(HTML('.info-box {min-height: 76px;} .info-box-icon {height: 76px; line-height: 76px;} .info-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
    fluidRow(
      box(title="Weather Condition",status="info",solidHeader=TRUE,
          fluidRow(uiOutput("cityBox")),
          fluidRow(uiOutput("sunrise")),
          fluidRow(uiOutput("sunset")),
          fluidRow(uiOutput("completeBox"))
          ,width=4,height=420),
      box(leafletOutput("map"),width=8)
    ),
    fluidRow(
      box(title="Famous Spots",status="primary",solidHeader=TRUE,plotOutput("dist"),width=12),
      
    )
  )
)
)
