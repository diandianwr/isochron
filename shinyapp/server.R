library(shiny)
library(leaflet)
library(leafletR)
library(sp)
library(ggplot2)

load("neighborhoods.RData")

shinyServer(function(input, output, session) {
  
  values <- reactiveValues(highlight = c(), clicked = c())
  
  palCols <- ifelse(neighborhoods$S_HOOD %in% car$neighborhood,"#31a354","#636363")
  
  # initiate leaflet map
  output$map <- renderLeaflet({
    leaflet::leaflet(neighborhoods) %>%
      addTiles(
        urlTemplate = "http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png",
        attribution = "Map tiles by <a href=\"https://cartodb.com\">CartoDB</a>, 
                       under <a href=\"http://creativecommons.org/licenses/by/3.0\">CC BY 3.0</a>. 
                       Data by <a href=\"http://openstreetmap.org\">OpenStreetMap</a>, 
                       under <a href=\"http://creativecommons.org/licenses/by-sa/3.0\">
                       CC BY SA</a>."
      ) %>%
      setView(lng=-122.3331,lat=47.6225,zoom=12) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(color="#252525",weight=2,opacity=.4,
                  fillColor=palCols,fillOpacity=0.1,
                  layerId=as.character(neighborhoods$S_HOOD)) %>%
      addLegend("bottomright",colors=c("deepskyblue","mediumseagreen"),labels=c("5 minutes","10 minutes"),
                title="Travel Times",opacity=1)
  })
  
  # update ui for neighborhood info, to be called upon mouseover
  updateUI<-function() {
    renderUI({
      if (is.null(values$highlight)) {
        return(tags$div(
          tags$strong("Hover over a neighborhood.")
          ))
      }
      
      if (!(values$highlight %in% neighborhoods$S_HOOD)) {
        return(tags$div(
          tags$strong("Hover over a neighborhood.")
        ))
      } else {
        name <- neighborhoods$S_HOOD[values$highlight == neighborhoods$S_HOOD]
        return(tags$div(
          tags$strong(name)
        ))
      }
    })
  }
  
  # update text output for neighborhood info, to be called upon click
  updateText<-function(dist1,dist2,sd1,sd2,obs) {
    if (is.null(values$clicked)) {
      output$description<-renderText({
        paste0("")
      })
    } 
    if (length(dist1)<1) {
      output$description<-renderText({
        paste0("")
      })
    } else {
      output$description<-renderText({
        paste0("The average distance traveled in 5 minutes from routes originating in ",
               values$clicked," is ",round(dist1,0)," meters. The average distance traveled in 10 minutes is ",
               round(dist2,0)," meters. There are ",obs, " recorded trips from this location. The standard deviation is ",
               round(sd1,0)," meters and ",
               round(sd2,0)," meters, respectively.")
      })
    }
  }
  
  # update map, to be called upon click
  updateMap<-function() {
    if (is.null(values$clicked)) {
      
    } else {
        map.buffer5 <- try(buffers5[values$clicked == buffers5$S_HOOD,],silent=TRUE)
        map.buffer10 <- try(buffers10[values$clicked == buffers10$S_HOOD,],silent=TRUE)
        if("try-error" == class(map.buffer5)) {

        } else {
            leafletProxy("map",data=neighborhoods) %>%
              addPolylines(data=map.buffer5,color="deepskyblue",weight=2,opacity=0.8,
                           layerId=paste0(as.character(neighborhoods$S_HOOD),".lines5")) %>%
              addPolylines(data=map.buffer10,color="mediumseagreen",weight=2,opacity=0.8,
                           layerId=paste0(as.character(neighborhoods$S_HOOD),".lines10"))
        }
    }
  }
  
  observe({
    values$highlight <- input$map_shape_mouseover$id
    output$info<-updateUI()
  })
  
  observe({
    values$clicked <- input$map_shape_click$id
    
    updateMap()
    
    line5 <- car$dis5mean[car$neighborhood==values$clicked]
    line10 <- car$dismean10[car$neighborhood==values$clicked]
    sd5 <- car$dis5sd[car$neighborhood==values$clicked]
    sd10 <- car$dis10sd[car$neighborhood==values$clicked]
    obs <- car$count.y[car$neighborhood==values$clicked]
    
    # ggplot code
    output$distPlot<-renderPlot({
      cols<-c("5 minutes"="deepskyblue2","10 minutes"="mediumseagreen")
      g<-ggplot(data=car,aes(x=dis5mean,y=..density..)) +
        stat_density(aes(fill="5 minutes"),alpha=0.4,size=1) +
        stat_density(aes(x=dismean10,fill="10 minutes"),alpha=0.4,size=1) +
        geom_vline(xintercept = line5, linetype = "longdash", colour = "deepskyblue2", size = 1) +
        geom_vline(xintercept = line10, linetype = "longdash", colour = "mediumseagreen", size = 1) +
        labs(title=paste("Distribution of average distance traveled in 5 and 10 minutes\norginiating from", values$clicked),
             x="Distance",y="Density") +
        xlim(0,8200) +
        theme(axis.text.y=element_blank(),legend.position="bottom") +
        scale_fill_manual(name="",values=cols) +
        guides(fill = guide_legend(reverse = TRUE))
      print(g)
      
    })
    
    updateText(line5,line10,sd5,sd10,obs)
    
  })
  
})