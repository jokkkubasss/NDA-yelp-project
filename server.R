#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)


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
  
  # Descriptive General
  filtered.user.data <- reactive({
    
    # Here, we filter the user data. 
    dt.unique.users <- dt.vegas.full[, .(user_id = unique(user_id)), by = 
                                       .(average_user_stars,review_count_user,
                                         fans)]
    # Somehow, this user hasn't reviewed anything, so we're taking him/her out.
    dt.unique.users <- dt.unique.users[-c(240992), ,]
    
  })
  
    output$sum_fans <- renderPrint({
      summary(dt.unique.users$fans)
      
    }) 
  
    output$sum_user_reviews <- renderPrint({
      summary(dt.unique.users$review_count_user)
      
    })
    
    output$sum_user_stars <- renderPrint({
      summary(dt.unique.users$average_user_stars)
      
    })
 
    output$review_count_business <- renderPrint({
      summary(dt.biz$review_count_business)
      
    })
      
    output$star_business <- renderPrint({
      summary(dt.biz$avg_stars)  
    })
    
})
