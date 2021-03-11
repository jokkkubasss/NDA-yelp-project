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

# for Jokubas:
setwd('C:/Users/jkras/Desktop/UNI/NDA/YELP/NDA-yelp-project')

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


# First we filter for the reviews that are considered usefull
dt.vegas.graph <- dt.vegas.full[review_count_user > 500 
                                & votes_useful_review > 5]


# Then we create unique lists for the network
l.business <- (unique(dt.vegas.graph$business_name))
l.unique.reviewers <- dt.vegas.graph[business_name %in% l.business, 
                                     unique(user_id)]

# Turn them into Dt's
dt.reviewers <- dt.vegas.graph[user_id %in% l.unique.reviewers, 
                               list(name = unique(user_id), type = TRUE)]
dt.businesses <- dt.vegas.graph[user_id %in% l.unique.reviewers,
                                list(name = unique(business_name), type = FALSE)]
# Create the vertices
dt.vertices <- rbind(dt.reviewers, dt.businesses)

# Create the graph based on reviewers and businesses.
g.bi.vegas <- graph_from_data_frame(dt.vegas.graph[user_id %in% 
                                                     l.unique.reviewers, 
                                                   list(user_id, business_name)],
                                    directed = FALSE, vertices = dt.vertices)

#### Main businesses bipartite network
g.businesses <- bipartite.projection(g.bi.vegas, multiplicity = TRUE)$proj1

# Remove nodes without edges
g.businesses.filtered <- induced_subgraph(g.businesses,
                                          vids = which(degree(g.businesses) != 0))


# The clustering coefficient is quite large for this network
transitivity(g.businesses, type = "average")


#### Network D3
# Removing weak edges
g.businesses.d3.filtered <- delete_edges(g.businesses.filtered, 
                                         E(g.businesses.filtered)[weight < 15])


# Removing disconnected nodes
g.businesses.d3 <- induced_subgraph(g.businesses.d3.filtered,
                                    vids = which(degree(g.businesses.d3.filtered) != 0))

# adding dimensions
V(g.businesses.d3)$degree <- degree(g.businesses.d3)
V(g.businesses.d3)$betweenness <- betweenness(g.businesses.d3)
#V(g.businesses.d3)$closeness <- closeness(g.businesses.d3)
V(g.businesses.d3)$evcent <- evcent(g.businesses.d3)$vector

# turning it into a dataframe
dt.ntw.attrs <- as_data_frame(g.businesses.d3, 'vertices')

dt.ntw.attrs <- data.table(dt.ntw.attrs)


cols <- names(dt.ntw.attrs)[2:4]
dt.ntw.attrs[,(cols) := round(.SD,3), .SDcols=cols]



# Adding a group, since it is required.
V(g.businesses.d3)$nghd <- 'group one' 

g.biz <- igraph_to_networkD3(g.businesses.d3,
                             group = V(g.businesses.d3)$nghd)


# creating the betweenness dimension for D3 object
g.biz$nodes$betweenness <-  betweenness(g.businesses.d3)



g.biz$links$value

normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

make.graph(test)

test

# function that returns the filtered NetworkD3 object  
make.graph <- function(items.to.filter) {
  
  g.original <- induced_subgraph(g.businesses.d3, 
                                 vids = V(g.businesses.d3)[name %in% items.to.filter])
  g.temp <- igraph_to_networkD3(g.original, 
                                group = c(rep('test', length(items.to.filter))))
  g.temp$nodes$betweenness <- betweenness(g.original)
  
  return(g.temp)
}




make.graph.neighborhoods <- function(items.to.filter) {
  
  g.original <- make_ego_graph(g.businesses.d3,
                             order = 1,
                             nodes = V(g.businesses.d3)[name %in% items.to.filter])
  g.union <- make.g.union(g.original)
  
  
  g.temp <- igraph_to_networkD3(g.union, 
                                group = c(rep('test', length(V(g.union)))))
  
  g.temp$nodes$betweenness <- betweenness(g.union)
  
  return(g.temp)
    
}

# function that unionizes multiple igraphs
make.g.union <- function(graphs) {
  result <- get.edgelist(make_empty_graph())
  for (subgraph in graphs) {
    result <- rbind(result, get.edgelist(subgraph))
  } 
  return(graph_from_edgelist(unique(result)))
}


# Function that checks membership of each a string of items against a row containing these items
check_membership <- function(keys, item) {
  if (sum(keys %in% strsplit(item, ", ")[[1]]) == length(keys)) {
    TRUE
  } else {
    FALSE
  } 
}
