library(shiny)
library(leaflet)
library(leafletR)
library(sp)
library(ggplot2)

load("neighborhoods.RData")

shinyUI(navbarPage(title="Data Explorer",id="nav",
                   
 tabPanel(title="Interactive map",

  div(class="outer",
                                
    tags$head(
      
      # include custom CSS and Javascript
      includeCSS("styles.css"),
      includeScript("gomap.js")
      
    ),
    
    leafletOutput("map", width="100%", height="100%"),
    
    absolutePanel(
      top=80,left=10,width=400,draggable=TRUE,
      wellPanel(
        
        id="controls",draggable=TRUE,top="auto",bottom="auto",left="auto",right="auto",width=400,
        height="auto",
        
        h2("About this data"),
        
        h5("This interactive map displays isochrones (lines of equal travel time) at 5 and 10 minute
           intervals based upon travel survey data from the Puget Sound Regional Council. Regions
           colored in grey do not have travel data orginiating from that neighborhood."),
        
        h2("Explore this data"),
        
        h5("Click on a neighborhood to learn about traveling from there!"),
        
        # for future messages the user might want to know
        textOutput(outputId="explorerMessages")
        
        )),
    
    absolutePanel(
      right=10,top=40,width=500,draggable=TRUE,
      wellPanel(
        
        id="controls",top="auto",bottom="auto",left="auto",right="auto",width=400,
        height="auto",
        
        h3(),
        uiOutput("info"),
        tags$hr(),
        plotOutput("distPlot"),
        textOutput("description")
      )
    ),
                                
    tags$div(id="cite",HTML("Travel survey data collected by: 
                            <a href=\'http://www.psrc.org/'>Puget Sound Regional Council.</a><br>
                            Unofficial neighborhood boundaries made available from:
                            <a href=\'https://data.seattle.gov/dataset/Neighborhoods/2mbt-aqqx'>City of Seattle.</a>"))
    
    )
  
  )
 
))