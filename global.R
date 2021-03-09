#### This file contains all the necessary files that we will be using. ####
### RUN BEFORE RUNNING APP ###
library(shiny)
library(ggplot2)
library(data.table)
library(tidyverse)
library(DT)
library(data.table)
library(networkD3)
library(igraph)

# load the business categories
cats.for.select <- readRDS("business_categories.rds")

# load the business dataset
dt.biz <- readRDS('bussinesses.rds')

# load the reviews dataset 
dt.vegas.full <- readRDS('vegas_full.rds')

# load business dataset with neighborhoods (which leaves out 1 business)
dt.biz.nb <- readRDS('business_neighborhoods.rds')


# Here, we filter the user data. 
dt.unique.users <- dt.vegas.full[, .(user_id = unique(user_id)), by = 
                                   .(average_user_stars,review_count_user,
                                     fans)]

# Somehow, one user hasn't reviewed anything, so we're taking him/her out.
dt.unique.users <- dt.unique.users[-c(240992), ,]

# Here, we filter the data for the general descriptives table.

dt.vegas.full.2 <- dt.vegas.full[sample(nrow(dt.vegas.full), 100),
                                 .(user_id, business_id, stars, business_name, 
                                   avg_stars, review_count_business,
                                   review_count_user, fans, neighborhood_v)]



### Graph Creation

# graph for the business network (filtered to include only top centiles)
dt.vegas.graph <- dt.vegas.full[dt.vegas.full$fans > 1750]

l.business <- (unique(dt.vegas.graph$business_name))
l.unique.reviewers <- dt.vegas.graph[business_name %in% l.business, 
                                     unique(user_id)]

dt.reviewers <- dt.vegas.graph[user_id %in% l.unique.reviewers, 
                               list(name = unique(user_id), type = TRUE)]
dt.businesses <- dt.vegas.graph[user_id %in% l.unique.reviewers,
                                list(name = unique(business_name), type = FALSE)]

dt.vertices <- rbind(dt.reviewers, dt.businesses)

# Create the graph based on reviewers and businesses.
g.bi.vegas <- graph_from_data_frame(dt.vegas.graph[user_id %in% 
                                                     l.unique.reviewers, 
                                                   list(user_id, business_name)],
                                    directed = FALSE, vertices = dt.vertices)

#### Network D3

g.businesses <- bipartite.projection(g.bi.vegas, multiplicity = TRUE)$proj1

E(g.businesses)$weight

V(g.businesses)$nghd <- 'group one' 

g.biz <- igraph_to_networkD3(g.businesses, group = V(g.businesses)$nghd)

V(g.businesses)$degree <- degree(g.businesses)

g.biz$betWeennes <- betweenness(g.businesses)
g.biz$nodes$betweenness <- betweenness(g.businesses)
order(g.biz$links$value)

forceNetwork(Links = g.biz$links, 
             Nodes = g.biz$nodes, 
             Source = 'source', 
             Target = 'target', 
             NodeID = 'name', 
             Group = 'group', 
             linkDistance = 200,
             linkWidth = E(g.businesses)$weight,
             Nodesize = "betweenness")



plot(g.businesses, vertex.label = NA)

# Function that checks membership of each a string of items against a row containing these items
check_membership <- function(keys, item) {
  if (sum(keys %in% strsplit(item, ", ")[[1]]) == length(keys)) {
    TRUE
  } else {
    FALSE
  } 
}
