library(shiny)
library(DT)
library(shinydashboard)
library(leaflet)
library(plotly)
library(dplyr)
library(ggplot2)

dashboardPage(
  dashboardHeader(title="Malaria in Colombia"),
  dashboardSidebar(
    sliderInput("Year", "Year:", 2007, 2016,c(2012,2016)),
                   radioButtons("x_var", "",
                                choices=list("Colombia"= "Total",
                                             "Departments"="Nombre.Dpto")), htmlOutput("filmuni") ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Documentation", includeMarkdown("./PA3.md")),
      tabPanel("APP",
    box(plotlyOutput("distPlot"),title = "Cases Plot",width = 11),
    box(dataTableOutput("table"),title = "Table of values",width=6),
    box(leafletOutput("map"),title = "Map of Malaria Cases by Department",width=6)
      )
    )
    )
                
)