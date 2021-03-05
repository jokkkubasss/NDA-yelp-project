#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


shinyServer(function(input, output) {
  
  filtered_data <- reactive({
    
    # here happens the filtering...
    dt.biz[unlist(lapply(dt.biz$categories, check_membership, keys = input$categories)) &
             avg_stars >= input$rating & 
             if (length(input$price_cats) == 0) 1 else price_range %in% input$price_cats]
  
  })

  
  output$lv_map <- renderLeaflet({
    dt <- data()
    
    leaflet(data = dt) %>%
      setView(lng = -115.1666, lat = 36.1465,  zoom = 11) %>%
      addProviderTiles(providers$OpenStreetMap)

      
  }) 
  # observer for filtering the map
  observe({
    leafletProxy("lv_map", data = filtered_data()) %>%
      clearMarkers() %>%
      addMarkers(lng = ~longitude, 
                 lat = ~latitude,
                 popup = ~paste("Name: ", business_name, "<br>", 
                                "Rating: ", avg_stars, "<br>", 
                                "Number of reviews: ", review_count_business, "<br>"
                                ))
  })
  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'black')
    
    
  })
  
  
  
})
