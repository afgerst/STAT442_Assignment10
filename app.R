
# https://afgerst.shinyapps.io/Assignment10/


# Creating the app
library(shiny) 
# For Creating reactive plots
library(ggplot2)
# Themes for the shiny app
library(shinythemes)
# Data to make clickable map
library(maps)
# Filtering data
library(dplyr)

# Data sets used in the app
# Long/Lat data for states
usaMap <- map_data("state")
accident <- read.csv("accident.csv")



# Set up for clickable U.S. Map
# Copied from: https://gl-li.github.io/study-notes/shiny_click_on_a_map.html with some minor changes
which_state <- function(mapData, long, lat) {
  
  # calculate the difference in long and lat of the border with respect to this point
  mapData$long_diff <- mapData$long - long
  mapData$lat_diff <- mapData$lat - lat
  
  # only compare borders near the clicked point to save computing time
  mapData <- mapData[abs(mapData$long_diff) < 20 & 
                       abs(mapData$lat_diff) < 15, ]
  
  # calculate the angle between the vector from this clicked point to border and c(1, 0)
  vLong <- mapData$long_diff
  vLat <- mapData$lat_diff
  mapData$angle <- acos(vLong / sqrt(vLong^2 + vLat^2))
  
  # calculate range of the angle and select the state with largest range
  rangeAngle <- tapply(mapData$angle, 
                       mapData$region, 
                       function(x) max(x) - min(x))
  return(names(
    sort(
      rangeAngle, decreasing = TRUE))[1])
}




# Creating function for the USA map to make later code cleaner
plotMap <- ggplot(usaMap, 
                  aes(x = long, 
                      y = lat, 
                      group = group)) + 
  geom_polygon(fill = "white", 
               color = "black") +
  theme_void()


ui <- fluidPage(
    
    theme = shinytheme("flatly"),
  
    titlePanel("U.S. Car Accident Fatalities, 2021"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("weather_input",
                    "Select Weather:",
                    choices = unique(accident$WEATHERNAME),
                    multiple = TRUE,
                    selected = "Rain")
        
      ),
      mainPanel(
        plotOutput("map", 
                   click = "clickMap", 
                   width = 600, 
                   height = 375),
        plotOutput("weather", 
                   width = 430, 
                   height = 275)
      )
   )
)
    
    
  

server <- function(input, output, session) {
  # intital plots
  output$map <- renderPlot({
    plotMap
  })
  
  output$weather <- renderPlot({
    g <- accident %>% 
      filter(WEATHERNAME %in% input$weather_input)
    g1<- ggplot(g, mapping = 
                  aes(x = WEATHERNAME, y = FATALS, 
                      color = WEATHERNAME)) +
      labs(x = "Weather Type") +
      geom_jitter(show.legend = FALSE)
    g1
  })
  
  # plot after click
  observeEvent(input$clickMap, {
    xClick <- input$clickMap$x
    yClick <- input$clickMap$y
    state <- which_state(usaMap, xClick, yClick)
    output$map <- renderPlot(
      plotMap + 
        geom_polygon(data = usaMap[usaMap$region == state,], fill = "beige") +
        annotate("text", x = xClick, y = yClick, label = state, 
                 color = "firebrick", size = 7, fontface = "bold")
    )
    output$weather <- renderPlot({
      g <- accident %>% 
        filter(WEATHERNAME %in% input$weather_input
               # There used to be a function to filter based on which state was
               # clicked, but when included it just clears all data from the 
               # jitter plot
               )
      g1<- ggplot(g, mapping = 
                    aes(x = WEATHERNAME, y = FATALS, 
                        color = WEATHERNAME)) +
        labs(x = "Weather Type") +
        geom_jitter(show.legend = FALSE)
      g1 
      # Things change on the jitter plot when states are clicked, the 
      # Changes just don't make any sense given the data :'(
    })
  })
}

shinyApp(ui, server)