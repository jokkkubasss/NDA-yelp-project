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
library(shinythemes)



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinytheme("united"),
  tags$head(HTML('<link rel="icon" href="www/icon_new.png" 
                type="image/png" />'),# Include custom CSS
    includeCSS("www/styles.css")),
  
  # Application title
  tags$div(class = "title"),
  navbarPage(
    "Vegas Party Planning",
    navbarMenu(
      title = "Welcome!",
      tabPanel(
        "Welcome to Las Vegas Party Planning",
        fluid = TRUE,
        tags$div(
          class = 'main_page',
        titlePanel("Welcome to Las Vegas Party Planning"),
        sidebarLayout(sidebarPanel(
          p(
            "Welcome to Las Vegas Party Planning!
            With this app, you can find your perfect party, bar, and dining destinations for your trip to Las Vegas.
            In the Plan Your Trip tab, you can use our interactive map to select your favorite categories, minimum amount of stars, and price categories.
            This will allow users to easily select the destinations that they like. 
            In addition, it allows for businesses to easily see who their main competitors are in certain categories.
            In the Explore The Data tab, you can find the descriptives of the data that we used.
            This tab includes summary statistics, and an overview of some of our most important variables. 
            In addition, you can find interactive plots on reviewers and businesses. For example, you can see which neighborhoods have which kinds of restaurants here. 
            The See The Network tab gives you the ability to see our businesses and reviewers networks in great detail.
            We have two bipartite graphs. In the businesses graph, you can see the destinations which were visited by the best Yelp reviewers, and are most centrally located. 
            This can serve as an indicator which restaurants you cannot miss!
            In addition, in the reviewers network, you can find who of the most popular reviewers have visited the same places. 
            It gives an indication of which reviewers are the key players in rating our party destinations in the Las Vegas area.
            Lastly, in the Predict your Party! tab allows you to predict your next favorite place to party in Vegas, based on your personal preferences."
          )
          ),
          mainPanel(
            img(
              src = "lasvegas_sign.jpg",
              height = 600,
              width =
                900
            )
          )))
        ),
      
      
      
      
      tabPanel(
        "About The Team",
        fluid = TRUE,
        titlePanel("About The Team"),
        column(
          2,
          h4("Ting Fung Lee"),
          p(
            "Graduated in the bachelor International Business at the Rijksunviersiteit in Groningen, Ting Fung decided to further develop his interest in Business Intelligence by doing the Master BIM at the RSM.
            Next to his studies, Ting Fung is participating in the CleanTech Challenge, where students are working on circular business models."
          )
          ),
        column(2,
               h4("Sawan Mahabier")),
        column(2,
               h4("Jokubas Krasauskas")),
        column(
          2,
          h4("Renate Schroder"),
          p(
            "Renate completed her BSc in International Business at the RuG. Afterwards, she chose to further her interests in the strategic use of data by studying BIM.
            Next to her studies at the RSM, she's also chairing a committee for the Rotterdam Consulting Club, and part of a consulting project for Restaurant Brands International.
            Lastly, she is a BIM Honours Student."
          )
          )
          )
        ),
    
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
    navbarMenu(
      title = "Explore The Data",
      tabPanel(
        "General Descriptives",
        fluid = TRUE,
        titlePanel("General Descriptives"),
        mainPanel(
          p(
            "Welcome to the General Descriptives Page!
            Our dataset is based on data gathered directly from Yelp.
            In this dataset, there are 1542 businesses of a number of different categories, spread throughout different areas in Las Vegas.
            Each of these businesses has its own unique ID, neighborhood, exact location, average star rating, review count, and price range."
          ),
          p(
            "In addition, the dataset is comrpised of 481,312 unique user reviews.
            Each individual review holds information on its usefulness, how funny, or how cool other users thought the reviews were."
          ),
          p(
            "The dataset also holds information specifically on the 255,778 reviewers.
            There is data on the average star rating that a user gives, their review count, the usefulness, funniness, and coolness of their reviews, and the number of fans that certain reviewers have."
          ),
          p(
            "In the Summary Statistics Table, you can find summary statistics on some of the most interesting variables in the dataset.
            It's noticeable that the mean and median star ratings for both reviewers and businesses are high, indicating that reviewers tend to give positive reviews overall.
            In addition, the user review count and the user fans both have high maxima, but a very low median and average in comparison.
            Lastly, this effect seems to be similar for business reviews, although weaker.
            Below, you can find a table with the first 100 observations in our dataset, with the most interesting variable names listed.
            You can pick the number of rows that you are interested in viewing."
          ),
          p(
            "In the drop down menu in the navgiation bar, you can navigate towards other interesting, interactive descriptives, that delve deeper into our data than what you can find here.
            "
          ),
          h4("Summary Statistics Table"),
          DT::dataTableOutput("table_sum_stats"),
          h4("Dataset Table"),
          DT::dataTableOutput("table_vegas_full")
          )
          ),
      tabPanel(
        "Business Descriptives",
        fluid = TRUE,
        titlePanel("Business Descriptives"),
        br(),
        sidebarLayout(
          sidebarPanel(
            "The histogram on the right displays the distribution of destinations based on their average rating. Here are options to filter the histogram on minimum amount of reviews, neighborhood and price range.
            Below the histogram, the top 10 destinations are displayed depending on the filters chosen. The destinations are first sorted by rating, then by the number of reviews.",
            width = 5,
            br(),
            br(),
            sliderInput(
              inputId = "min_reviews",
              label = h5("Minimum reviews"),
              value = 0,
              min = 0,
              max = 2000,
              step = 100
            ),
            selectInput(
              inputId = "biz_neighborhood",
              label = h5("Neighborhood"),
              choices = c(
                "All",
                "Anthem",
                "Centennial",
                "Downtown",
                "North Las Vegas",
                "South East",
                "South West",
                "Spring Valley",
                "S. Summerlin",
                "Summerlin",
                "Sunrise",
                "Whitney"
              )
            ),
            checkboxGroupInput(
              "price_cats_biz",
              label = h5("Price category"),
              choices = c("Low", "Medium", "High"),
              inline = TRUE,
              selected = c("Low", "Medium", "High")
            ),
            br(),
            h5("General observations"),
            p(
              "In the plot it is evident that the majority of the destinations have a rating around 4 stars,
              which also does not seem to change when filtering on a higher amount of reviews.
              An interesting observation is that there are relatively more destinations rated higher than 4 stars in the lowest price category compared to the other price categories.
              The reason for this is likely that expectations become higher as destinations fall in higher price categories."
            )
            ),
          mainPanel(
            h3("Rating distribution"),
            width = 5,
            plotOutput("hist_business_stars"),
            tableOutput("table_biz_distribution")
          )
            )
        ),
      tabPanel("Reviewer Descriptives")
          ),
    
    
    # Network tab
    navbarMenu(
      title = "See The Network",
      tabPanel("Network Descriptives",
               titlePanel("Network Descriptives")),
      tabPanel("Business Network",
               tags$div(
                 class = 'main_page',
                 titlePanel("Well reviewed businesses"),
                 p(
                   'This page shows the bars, restaurants, and nightlife that were reviewed by the most popular reviewers in Las Vegas. 
                   One business is connected to another business if they were reviewed by the same reviewer.
                   The reviewers were selected on the basis of the usefulness of their reviews, and on the basis of how many fans the reviewers have.
                   The size of each destination node depends on its betweenness centrality.
                   The higher the betweenness, the shorter the path to other nodes in the network.
                   In essence, betweenness shows how important a restaurant is when it pertains to their connections to other restaurants.'
                 ),
                 sidebarLayout(sidebarPanel(
                   selectInput('business_node_single',
                               label = 'Choose a single restaurant',
                               choices = dt.ntw.attrs$name),
                     p(h5('Select a Destination')),
                     p('Here, you can select the bars, restaurants, and nightlife that were reviewed by the best reviewers.
                       If you select one of them, you will see automatically which other business was also reviewed by the same reviewers. 
                       If you choose multiple destinations at once, you will see whether the two businesses connect through reviewers or not.
                       If they connect, you can easily see through which destination.'),
                   selectInput('businesses_nodes',
                               label = 'Select your favourite destination',
                               multiple = TRUE,
                               #selected = dt.ntw.attrs$name,
                               selectize = TRUE,
                               choices = dt.ntw.attrs$name),
                   DT::dataTableOutput("business_network_dt")
                 ), mainPanel(forceNetworkOutput('force')))
                )),
      tabPanel("Reviewer Network",
               tags$div(
                 class = 'main_page',
                 titlePanel("Most popular reviewers"),
                 p('This page shows the most popular reviewers in our Las Vegas dataset by their unique user IDs. The data was pre-filtered based on the usefulness of reviews, and the number of fans reviewers have.
                   One reviewer is linked to another if they reviewed the same businesses. The size of the reviewer nodes is dependent on the betweenness of a node. 
                   The higher the betweenness, the shorter the path to other nodes in the network.'),
                 sidebarLayout(
                   sidebarPanel(
                     p(h5('How to select reviewers')),
                     p('Here, you can select the most interesting reviewers, and see their direct neighbors.
                       By selecting more than one reviewer, you will see the chosen reviewers, their direct neighbors, and if and how they are connected to one another.'),
                     selectInput('reviewers_nodes',
                                 label = "Select interesting reviewers",
                                 multiple = TRUE,
                                 selectize = TRUE,
                                 choices = dt.ntw.attrs.r$name),
                     DT::dataTableOutput("dt.reviewers.network")
                   ),
                   mainPanel(forceNetworkOutput('force.r'))
                   )
                 )
               ),
 
    
    
    #predict
    tabPanel(title = "Predict your next party!")
    )
)))