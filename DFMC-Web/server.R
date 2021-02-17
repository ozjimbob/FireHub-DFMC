#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(raster)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    df_list <- list.files("../DFMC",full.names = TRUE)
    dfmc <- raster(df_list[length(df_list)])
    vpd_list <- list.files("../VPD",full.names = TRUE)
    vpd <- raster(vpd_list[length(vpd_list)])  
    vpd_trend <- raster("../VPDd/VPD_trend.tif")
    
    raster_to_show <- reactive({
        if(input$rasterLayer == "DFMC"){
            r <- dfmc
            
        }
        if(input$rasterLayer == "VPD"){
            r <- vpd
            
        }
        if(input$rasterLayer == "VPD Trend"){
            r <- vpd_trend
            
        }
        return(r)
    })
    
    palette_to_show <- reactive({
        if(input$rasterLayer == "DFMC"){
           p <- colorNumeric(c("red","orange","yellow","green"),0:40)
        }
        if(input$rasterLayer == "VPD"){
          p <- colorNumeric(c("blue","yellow","red"),0:8)
        }
        if(input$rasterLayer == "VPD Trend"){
            p <- colorNumeric(c("green","white","red"),c(-.5,.5))
        }
        return(p)
    })
    
    mapPlot <- reactive({
        leaflet() %>% addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addRasterImage(raster_to_show(),
        colors=palette_to_show(),
        opacity=0.8
        )})
    
    output$mapPlot <- renderLeaflet(mapPlot())

})
