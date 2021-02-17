#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(raster)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Dry Fuel Moisture Code and VPD"),

    # Sidebar with a slider input for number of bins
    fluidRow(
        column(2,
            selectInput("rasterLayer",
                        "Data Display:",
                        choices=c("DFMC","VPD","VPD Trend"),
                        selected="DFMC")
        ),

        # Show a plot of the generated distribution
        column(10,
               leafletOutput("mapPlot"))
        )
    )
)
