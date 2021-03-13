#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinytheme("united"),
  tags$head(# Include custom CSS
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
        tags$div(class = 'main_page',
                 #titlePanel("About the app"),
                 
                 column(6, span(
                   htmlOutput("welcome_message")
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
      leafletOutput('lv_map',
                    height = 900),
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
          tableOutput('table_sum_stats'),
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
            width = 4,
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
          mainPanel(tabsetPanel(
            tabPanel(
              "Plot",
              h3("Rating distribution"),
              width = 5,
              plotOutput("hist_business_stars")
            ),
            tabPanel("Table",
                     DT::dataTableOutput("table_biz_distribution"))
          ))
            )
      ),
      tabPanel(
        "Reviewer Descriptives",
        titlePanel("Reviewer Descriptives"),
        sidebarLayout(
          sidebarPanel(
            p(
              "On this page, you can select for the number of fans, review count, usefulnes, funny, and cool counts that reviewers have.
              Selecting a range for each category, an overview of users that meet these requirements and their average ratings is provided.
              Likewise, a table with their IDs and all the specific information regarding them is shown and the second tab."
            ),
            make_ui(dt.unique.users$fans, "Fans"),
            make_ui(dt.unique.users$review_count_user, "Review"),
            make_ui(dt.unique.users$votes_useful_review, "Useful"),
            make_ui(dt.unique.users$votes_funny_review, "Funny"),
            make_ui(dt.unique.users$votes_cool_review, "Cool")
            ),
          mainPanel(
            tabsetPanel(
              type = "tabs",
              tabPanel(
                "Plot",
                br(),
                p(
                  "Using the sliders to create subsets leads to noteworthy mentions. First, it is noticeable that there is a large sample population for reviewers that have 0 fans.
                  Also noteworthy is that the distribution of reviewers with 0 fans have an average rating of 5."
                ),
                br(),
                p(
                  "However, if you filter reviewers with a minimum requirement of 50 fans, or filter them with on minimum number of reviews, the distribution is failr normally distributed.
                  This can imply that users with an average rating of 5 are not actively rating businesses as seriously or have only rated a limited amount of times. If you up these numbers,
                  the average rating of users  is normally distributed around an average rating of 4. This probably has to do with the fact that these reviewers do take rating businesses more seriously."
                ),
                br(),
                p(
                  "Lastly, it is also noticeable that there are little reviewers that score on the counts on funny, usefull, and cool, leading to small sample populations. However, the average rating of users do fairly have normal distributions."
                ),
                plotOutput("ReviewersPlot")
                ),
              tabPanel("Table"),
              br(),
              tableOutput("ReviewersTable")
                )
          )
        )
        )),
      
      
      # Network tab
      navbarMenu(
        title = "See The Network",
        tabPanel("Network Descriptives",
                 titlePanel("Network Descriptives")),
        tabPanel(
          "Business Network",
          tags$div(
            class = 'custom_container',
            titlePanel("Well reviewed businesses"),
            p(
              'This page shows the bars, restaurants, and nightlife that were reviewed by the most popular reviewers in Las Vegas.
              One business is connected to another business if they were reviewed by at least 15 reviewers.
              The reviewers were selected on the basis of the usefulness of their reviews, and on the basis of how many fans the reviewers have.
              The size of each destination node depends on its betweenness centrality.
              The higher the betweenness, the shorter the path to other nodes in the network. To put it simply, the larger the node, the more reviewers
              have passed through that particular business.
              In essence, betweenness shows how important a restaurant is when it pertains to their connections to other restaurants.'
            ),
            sidebarLayout(
              sidebarPanel(
                p(h5('Select a Destination')),
                p(
                  'Here, you can select the bars, restaurants, and nightlife that were reviewed by the best reviewers.
                  If you select one of them, you will see automatically which other business was also reviewed by the same reviewers.
                  If you choose multiple destinations at once, you will see whether the two businesses connect through reviewers or not.
                  If they connect, you can easily see through which destination.'
                ),
                selectInput(
                  'businesses_nodes',
                  label = 'Select your favourite destination',
                  multiple = TRUE,
                  selectize = TRUE,
                  choices = dt.ntw.attrs$name
                ),
                DT::dataTableOutput("business_network_dt")
                ),
              mainPanel(forceNetworkOutput('force'))
            )
            )
      ),
      tabPanel(
        "Reviewer Network",
        tags$div(
          class = 'custom_container',
          titlePanel("Most popular reviewers"),
          p(
            'This page shows the most popular reviewers in our Las Vegas dataset by their unique user IDs. The data was pre-filtered based on the usefulness of reviews, and the number of fans reviewers have.
            One reviewer is linked to another if they reviewed the same businesses. The size of the reviewer nodes is dependent on the betweenness of a node.
            The higher the betweenness, the shorter the path to other nodes in the network.'
          ),
          sidebarLayout(
            sidebarPanel(
              p(h5('How to select reviewers')),
              p(
                'Here, you can select the most interesting reviewers, and see their direct neighbors.
                By selecting more than one reviewer, you will see the chosen reviewers, their direct neighbors, and if and how they are connected to one another.'
              ),
              selectInput(
                'reviewers_nodes',
                label = "Select interesting reviewers",
                multiple = TRUE,
                selectize = TRUE,
                choices = dt.ntw.attrs.r$name
              ),
              DT::dataTableOutput("dt.reviewers.network")
              ),
            mainPanel(forceNetworkOutput('force.r'))
          )
        )
        ),
      
      
      
      #predict
      tabPanel(
        title = "Predict your next party!",
        titlePanel("Find recommendations based on your favorite places!"),
        leafletOutput("lv_map_2", height = 900),
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
            "pref_destination",
            label = "Recommendation based on a destination",
            choices = l.business,
            selected = NULL
          ),
          selectInput(
            'similarities',
            label = 'Choose similarity index',
            choices = c('Jaccard', 'Dice', 'Adamic-Adar')
          ),
          sliderInput(
            'similarity_thresh',
            label = 'Similarity threshold',
            value = 0.7,
            min = 0,
            max = 1,
            step = 0.01
          )
          
          
        )
      )
    )
    )
    )
  )
