setwd("/Users/Wei/Documents/2016 Winter/Github/isochron")
dist <- read.csv("trip_5_10car.csv")

# define peakhour 
dist$time_start <- strptime(dist$time_start.x,format="%m/%d/%Y %H:%M:%OS")
dist$group <- paste0(substr(dist$time_start,12,13))
dist$group <- as.numeric(dist$group)

for(i in 1:length(dist$group)){
  if((dist$group[i] >=7 &&  dist$group[i] <9)){
    dist$hour[i] <- "peakhour"
  } else if((dist$group[i] >=16 && dist$group[i] <19)){
    dist$hour[i] <- "peakhour"
  }  else{
    dist$hour[i] <- "other"
  }
}

peak <- subset(dist, dist$hour == "peakhour")
other <- subset(dist, dist$hour == "other")

hist(peak$dis10)
hist(other$dis10)


# travel distance analysis
library(shiny)
library(leaflet)
library(ggplot2)

# UI
ui <- fluidPage(
#  leafletOutput("map", width = "100%", height = 600),
   absolutePanel(width = "50%", height = 50,
                plotOutput('plot'),
                plotOutput('plot2')))

# SERVER
server <- function(input, output, session){
  # input neighborhood x
  neigh <- subset(dist, dist$neighborhood.x == "")
  obs <- observe( 
  # 5 min distance
    output$plot <- renderPlot({
      ggplot(neigh, x= neigh$dis5, y = frequency(neigh$dis5), color = neigh$hour, xlab = "Distance", ylab = "Frequency") + geom_line() 
     }),
  # 10 min distance
    output$plot2 <- renderPlot({
      ggplot(neigh, x= neigh$dis10, y = frequency(neigh$dis10), color = neigh$hour, xlab = "Distance", ylab = "Frequency") + geom_line() 
    })
    )
}

shinyApp(ui,server)