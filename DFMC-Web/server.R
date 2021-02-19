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
    if(.Platform$OS.type=="unix"){
        rrt = "/root/"
    }else{
        rrt = "../"
    }
    df_list <- list.files(paste0(rrt,"DFMC"),full.names = TRUE)
    dfmc <- raster(df_list[length(df_list)])
    vpd_list <- list.files(paste0(rrt,"VPD"),full.names = TRUE)
    vpd <- raster(vpd_list[length(vpd_list)])  
    vpd_trend <- raster(paste0(rrt,"VPDd/VPD_trend.tif"))
    
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
          p <- colorNumeric(c("blue","yellow","red"),0:10)
        }
        if(input$rasterLayer == "VPD Trend"){
            p <- colorNumeric(c("darkgreen","green","white","coral","red"),c(-.7,.7))
        }
        return(p)
    })
    
    palette_vals<- reactive({
        if(input$rasterLayer == "DFMC"){
            p <- c(0,10,20,30,40)
        }
        if(input$rasterLayer == "VPD"){
            p <- c(0,1,2,3,4,5,6,7,8,9,10)
        }
        if(input$rasterLayer == "VPD Trend"){
            p <- c(-0.7,-0.4,-0.2,0,0.2,0.4,0.7)
        }
        return(p)
    })
    
    mapPlot <- reactive({
        leaflet() %>% addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addRasterImage(raster_to_show(),
        colors=palette_to_show(),
        opacity=0.8
        ) %>% addLegend(pal=palette_to_show(),values=palette_vals())})
    
    output$mapPlot <- renderLeaflet(mapPlot())

})
