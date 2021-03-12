#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyServer(function(input, output) {
  filtered_data <- reactive({
    # filtering for the map
    dt.biz[unlist(lapply(dt.biz$categories, check_membership, keys = input$categories)) &
             avg_stars >= input$rating &
             if (length(input$price_cats) == 0)
               1
           else
             price_range %in% input$price_cats]
    
  })
  
  # generating the map
  output$lv_map <- renderLeaflet({
    dt <- data()
    
    leaflet(data = dt) %>%
      setView(lng = -115.1666,
              lat = 36.1465,
              zoom = 11) %>%
      addProviderTiles(providers$OpenStreetMap)
    
    
  })
  # observer for refreshing the map
  observe({
    leafletProxy("lv_map", data = filtered_data()) %>%
      clearMarkers() %>%
      addMarkers(
        lng = ~ longitude,
        lat = ~ latitude,
        popup = ~ paste(
          "Name: ",
          business_name,
          "<br>",
          "Rating: ",
          avg_stars,
          "<br>",
          "Number of reviews: ",
          review_count_business,
          "<br>"
        )
      )
  })
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x,
         breaks = bins,
         col = 'darkgray',
         border = 'black')
    
    
  })
  
  # Descriptive General
  filtered.user.data <- reactive({
    # Here, we filter the user data.
    dt.unique.users <-
      dt.vegas.full[, .(user_id = unique(user_id)), by =
                      .(average_user_stars, review_count_user,
                        fans)]
    # Somehow, this user hasn't reviewed anything, so we're taking him/her out.
    dt.unique.users <- dt.unique.users[-c(240992), , ]
    
  })
  
  # Business Descriptive Data filtering
  filtered_biz_reviews_data_plot <- reactive({
    if (input$biz_neighborhood != "All") {
      dt.biz.nb[review_count_business >= input$min_reviews &
                  price_range %in% c(input$price_cats_biz) &
                  neighborhood_v == input$biz_neighborhood, avg_stars]
    }
    else
      dt.biz.nb[review_count_business >= input$min_reviews &
                  price_range %in% c(input$price_cats_biz), avg_stars]
    
  })
  
  filtered_biz_reviews_data_table <- reactive({
    if (input$biz_neighborhood != "All") {
      dt.biz.nb[review_count_business >= input$min_reviews &
                  price_range %in% c(input$price_cats_biz) &
                  neighborhood_v == input$biz_neighborhood, categories, by = c(
                    "business_name",
                    "avg_stars",
                    "price_range",
                    "review_count_business",
                    "neighborhood_v"
                  )]
    }
    else
      dt.biz.nb[review_count_business >= input$min_reviews &
                  price_range %in% c(input$price_cats_biz), categories ,
                by = c(
                  "business_name",
                  "avg_stars",
                  "price_range",
                  "review_count_business",
                  "neighborhood_v"
                )]
    
  })
  
  # General Descriptives Summary Statistics
  
  output$table_sum_stats <- renderTable({
    dt.sum.new.new},
    rownames = TRUE)
  
  output$table_vegas_full <- DT::renderDataTable({
    dt.vegas.full.2
    
  })

  # Plots and filtering for Descriptives
  
  output$hist_business_stars <- renderPlot({
    ggplot() + geom_histogram(aes(x = filtered_biz_reviews_data_plot()),
                              binwidth = 0.5, color = "black", 
                              fill = "darkred") +
      labs(x = "Star Rating", y = "Number of destinations",
           title = " ")
  })
  
  output$table_biz_distribution <- DT::renderDataTable(
    filtered_biz_reviews_data_table() %>%
      arrange(desc(avg_stars),
              desc(review_count_business)) %>%
      rename(
        Rating = avg_stars,
        Name = business_name,
        PriceRange = price_range,
        Reviews = review_count_business,
        Neighborhood = neighborhood_v,
        Categories = categories
      )
  )
  
  # Business to make network reactive
  
  business.network.nodes <- reactive({
    
    if(length(input$businesses_nodes) < 1) {
      return(g.biz)
    } else {
      return(make.graph.neighborhoods(input$businesses_nodes))
    }
  })
  
  nodes.reactive <- reactive({
    df.nodes <- business.network.nodes()$nodes
  })
  
  links.reactive <- reactive({
    df.links <- if (nrow(business.network.nodes()$links) == 0) {
      data.frame(source = 0,               
                 target = 0,
                 value = 0)
    } else {
      df.links <- business.network.nodes()$links
    }
  })
  
  # Reviewer Filter to make network reactive
  
  reviewers.network.nodes <- reactive({
    
    if(length(input$reviewers_nodes) < 1) {
      return(g.rev)
    } else {
      return(make.graph.neighborhoods.r(input$reviewers_nodes))
    }
  })
  
  nodes.reactive.r <- reactive({
    df.nodes.r <- reviewers.network.nodes()$nodes
  })
  
  links.reactive.r <- reactive({
    df.links.r <- if (nrow(reviewers.network.nodes()$links) == 0) {
      data.frame(source = 0,               
                 target = 0,
                 value = 0)
    } else {
      df.links.r <- reviewers.network.nodes()$links
    }
  })
  
  
  
  # businesses network
  # The networks
  output$force <- renderForceNetwork({
    forceNetwork(
      Links = links.reactive(),
      Nodes = nodes.reactive(), 
      Source = 'source',
      Target = 'target',
      NodeID = 'name',
      #Value = 'value',
      Group = 'group',
      fontSize = 20,
      height = 600,
      #linkWidth = networkD3::JS("function(d) { return Math.sqrt(d.value); }"),
      linkDistance = 100,#networkD3::JS("function(d) { return Math.pow(d.value, -1/5)*10; }"),
      linkWidth = 1,
      radiusCalculation = JS("Math.sqrt(d.nodesize)+4"), 
      bounded = TRUE,
      zoom = TRUE,
      Nodesize = "betweenness"
      
    )
  })
  
  output$business_network_dt <- renderDataTable(
    if(length(input$businesses_nodes) < 1) {
      dt.ntw.attrs
    } else {
      dt.ntw.attrs[dt.ntw.attrs$name %in% input$businesses_nodes, ]
    },
    rownames = FALSE
  )
  
  output$force.r <- renderForceNetwork({
    forceNetwork(
      Links = links.reactive.r(),
      Nodes = nodes.reactive.r(), 
      Source = 'source',
      Target = 'target',
      NodeID = 'name',
      Group = 'group',
      fontSize = 20,
      height = 600,
      linkDistance = 100,
      linkWidth = 1,
      radiusCalculation = JS("Math.sqrt(d.nodesize)+4"), 
      bounded = TRUE,
      zoom = TRUE,
      Nodesize = "betweenness"
    )
  })
  
  output$dt.reviewers.network <- renderDataTable(
    if(length(input$reviewers_nodes) < 1) {
      dt.ntw.attrs.r
    } else {
      dt.ntw.attrs.r[dt.ntw.attrs.r$name %in% input$reviewers_nodes, ]
    },
    rownames = FALSE
    
  )
  
  # Link Prediction / Recommendation
  
  # LV map
  output$lv_map_2 <- renderLeaflet({
    dt <- data()
    
    leaflet(data = dt) %>%
      setView(lng = -115.1666,
              lat = 36.1465,
              zoom = 11) %>%
      addProviderTiles(providers$OpenStreetMap)
  })
  
  # Data filtering, only including neighbors of a business
  
  
  recommendation_data <- reactive({
    dt.biz.nb[business_name 
              %in% neighbors(g.businesses, input$pref_destination)$name]
    
  })
  
  # Markers
  
  observe({
    leafletProxy("lv_map_2", data = recommendation_data()) %>%
      clearMarkers() %>%
      addMarkers(
        lng = ~ longitude,
        lat = ~ latitude,
        popup = ~ paste(
          "Name: ",
          business_name,
          "<br>",
          "Rating: ",
          avg_stars,
          "<br>",
          "Number of reviews: ",
          review_count_business,
          "<br>"
        )
      )
  })
  
})





  