#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$head(# Include custom CSS
    includeCSS("www/styles.css")),
  
  # Application title
  tags$div(class = "title", titlePanel("Las Vegas Party Planning")),
  navbarPage(
    "",
    navbarMenu(title = "Welcome!",
               tabPanel("About Las Vegas Party Planning", fluid = TRUE,
                        titlePanel("About Las Vegas Party Planning"),
                        sidebarLayout(
                          sidebarPanel(
                            p("Welcome to Las Vegas Party Planning!
                               With this app, you can find your perfect party, bar, and dining destinations for your trip to Las Vegas.
                               In the Plan Your Trip tab, you can use our interactive map to select your favorite categories, minimum amount of stars, and price categories.
                               In the Explore The Data tab, you can find the descriptives of the data that we used.
                                       The tab include general descriptives, and interactive plots with filters in which you can see the data in detail.
                                       The See The Network tab gives you the ability to see our businesses and user networks in great detail.
                                       Lastly, in the Predict your Party! tab allows users to predict their next favorite place to party in Vegas, based on the attributes of their preference.")
                          ), 
                          mainPanel(
                            img(src = "lasvegas_sign.jpg", height = 400, width =
                                  600)
                          )
                        )),
                        
                            
                              
                               
               tabPanel("About The Team", fluid = TRUE,
                        column(4,
                               br(),
                               h4("About The Team")
                               ))),
    
    #Plan your trip tab (map)
    tabPanel(
      title = "Plan Your Trip",
      leafletOutput('lv_map'),
      absolutePanel(
        id = "controls",
        fixed = FALSE,
        top = 160,
        left = "auto",
        right = 20,
        bottom = "auto",
        width = 300,
        height = "auto",
        selectInput(
          "categories",
          label = "Enter categories",
          choices = cats.for.select,
          multiple = TRUE,
          selectize = TRUE
        ),
        sliderInput(
          "rating",
          "Minimum stars ratings:",
          min = 1,
          max = 5,
          value = 4,
          step = 0.1
        ),
        checkboxGroupInput(
          "price_cats",
          label = "Price category $",
          choices = c("Low", "Medium", "High"),
          inline = TRUE
        )
      )
      
    ),
    # Explore the data tab
    navbarMenu(title = "Explore The Data",
        tabPanel("General Descriptives", fluid = TRUE,
                 titlePanel("General Descriptives"),
                 mainPanel(
                   p("Welcome to the General Descriptives Page!
                      Our dataset is based on data gathered directly from Yelp. 
                      In this dataset, there are 1542 businesses of a number of different categories, spread throughout different areas in Las Vegas.
                      Each of these businesses has its own unique ID, neighborhood, exact location, average star rating, review count, and price range."),
                   p("In addition, the dataset is comrpised of 481,312 unique user reviews.
                      Each individual review holds information on its usefulness, how funny, or how cool other users thought the reviews were."),
                   p("The dataset also holds information specifically on the users.
                      There is data on the average star rating that a user gives, their review count, the usefulness, funniness, and coolness of their reviews, and the number of fans that certain reviewers have."),
                 )),
        tabPanel("Business Descriptives"),
        tabPanel("Reviewer Descriptives")),
  
    
    # Network tab
    navbarMenu(title = "See The Network",
               tabPanel("Network Descriptives",
                        titlePanel("Network Descriptives"),
                        sidebarLayout(
                          sidebarPanel('Filters Go here'
                          ),
                          mainPanel('Graph goes here')
                        )),
               tabPanel("Business Network"),
               tabPanel("Reviewer Network")),
    
    #predict
    tabPanel(title = "Predict your next party!")
  )
))
