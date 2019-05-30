#load libraries
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

#import data
data <- read.csv("worldearthquakes.csv")

#categorize earthquake depth

data$depth_type <- ifelse(data$depth <= 70, "shallow", 
ifelse(data$depth <= 300 | data$depth >70, "intermediate", 
ifelse(data$depth > 300, "deep", "other")))



ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
    sliderInput("range", "Magnitudes", min(data$mag), max(data$mag),
      value = range(data$mag), step = 0.1
    ),
    selectInput("colors", "Color Scheme",
      rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
    ),
    checkboxInput("legend", "Show legend", TRUE),
    checkboxInput("markers", "Depth", FALSE),
    checkboxInput("heat", "Heatmap", FALSE)
  )
)

server <- function(input, output, session) {

  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    data[data$mag >= input$range[1] & data$mag <= input$range[2],]
  })

  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, data$mag)
  })

#define the color pallate for the magnitidue of the earthquake
 pal <- colorNumeric(
    palette = c('gold', 'orange', 'dark orange', 'orange red', 'red', 'dark red'),
    domain = data$mag)
  

#define the color of for the depth of the earquakes
 pal2 <- colorFactor(
    palette = c('blue', 'yellow', 'red'),
    domain = data$depth_type
  )
  

#create the map
  output$mymap <- renderLeaflet({
    leaflet(data) %>% 
      setView(lng = -99, lat = 45, zoom = 2)  %>% #setting the view over ~ center of North America
      addTiles() %>% 
      addCircles(data = data, lat = ~ latitude, lng = ~ longitude, weight = 1, radius =~sqrt(data$mag)*25000, popup = ~as.character(data$mag), label = ~as.character(paste0("Magnitude: ", sep = " ", data$mag)), color = ~pal(data$mag), fillOpacity = 0.7)

  })
  
#next we use the observe function to make the checkboxes dynamic. If you leave this part out you will see that the checkboxes, when clicked on the first time, display our filters...But if you then uncheck them they stay on. So we need to tell the server to update the map when the checkboxes are unchecked.

  observe({
    proxy <- leafletProxy("mymap", data = data)
    proxy %>% clearMarkers()
    if (input$markers) {
      proxy %>%addCircleMarkers(stroke = FALSE, color = ~pal2(data$depth_type), fillOpacity = 0.2,      label = ~as.character(paste0("Magnitude: ", sep = " ", data$mag))) %>%
        addLegend("bottomright", pal = pal2, values = data$depth_type,
                  title = "Depth Type",
                  opacity = 1)}
    else {
      proxy %>% clearMarkers() %>% clearControls()
    }
  })
  
  observe({
    proxy <- leafletProxy("mymap", data = data)
    proxy %>% clearMarkers()
    if (input$heat) {
      proxy %>%  addHeatmap(lng=~longitude, lat=~latitude, intensity =~data$mag, blur =  10, max = 0.05, radius = 15) 
      }
    else{
      proxy %>% clearHeatmap()
      }
    
    
  })

  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()

    leafletProxy("mymap", data = filteredData()) %>%
      clearShapes() %>%
      addCircles(radius = ~sqrt(data$mag)*25000, weight = 1, color = "#777777",
        fillColor = ~pal(data$mag), fillOpacity = 0.7, popup = ~paste(data$mag)
      )
  })

  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("mymap", data = data)

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
        pal = pal, values = ~data$mag
      )
    }
  })
  
}

shinyApp(ui, server)