#### This file contains all the necessary files that we will be using. ####
### RUN BEFORE RUNNING APP ###

library(leaflet)
library(shinythemes)
library(shiny)
library(ggplot2)
library(data.table)
library(tidyverse)
library(DT)
library(data.table)
library(networkD3)
library(igraph)
library(pastecs)

library(lubridate)
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

# Dataset for reviewers network	
dt.unique.users.all <- dt.vegas.full[!duplicated(dt.vegas.full$user_id), ]	
dt.unique.users.reviewers <- dt.unique.users.all[, list(user_id, 
                                                        fans, 
                                                        average_user_stars, 
                                                        review_count_user, 
                                                        votes_useful_review, 
                                                        votes_funny_review, 
                                                        votes_cool_review)]	


# Here, we filter the data for the general descriptives tables.

dt.summary <- dt.vegas.full[, .(stars, avg_stars, review_count_business,
                                fans, average_user_stars)]

dt.sum <- summary(dt.summary)
dt.sum <- as.data.table(dt.sum)
dt.sum.new <- rename(dt.sum,
                  Variable = V2,
                  SummaryStats = N)
dt.sum.new <- dt.sum.new[, .(Variable, SummaryStats)]

attach(dt.summary)
scores <-cbind(stars, avg_stars, review_count_business, fans, average_user_stars)
options(scipen=100)
options(digits=2)
dt.sum.new.new <- stat.desc(scores, basic = F)

dt.vegas.full.2 <- dt.vegas.full[sample(nrow(dt.vegas.full), 100),
                                 .(user_id, business_id, stars, business_name, 
                                   avg_stars, review_count_business,
                                   review_count_user, fans, neighborhood_v)]


dt.unique.users.all <- dt.vegas.full[!duplicated(dt.vegas.full$user_id), ]	

dt.unique.users <- dt.unique.users.all[, list(user_id, 
                                              fans, 
                                              average_user_stars, 
                                              review_count_user, 
                                              votes_useful_review, 
                                              votes_funny_review, 
                                              votes_cool_review)]
### Graph Creation

# graph for the business network (filtered to include only top centiles)
# graph for the business network (filtered to include only top percentiles)

dt.vegas.graph <- dt.vegas.full[dt.vegas.full$fans > 999]


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

# currently unused 
normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}


# function that returns the filtered NetworkD3 object (you can use this one or the one below. 
# Just copy it and alter the 'g.businesses.d3' into a graph of reviewers)
make.graph <- function(items.to.filter) {
  
  g.original <- induced_subgraph(g.businesses.d3, 
                                 vids = V(g.businesses.d3)[name %in% items.to.filter])
  g.temp <- igraph_to_networkD3(g.original, 
                                group = c(rep('test', length(items.to.filter))))
  g.temp$nodes$betweenness <- betweenness(g.original)
  
  return(g.temp)
}


# function that creates a connection of neighborhoods of selected nodes
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

# function that unionizes multiple igraphs (used in the function above)
make.g.union <- function(graphs) {
  result <- get.edgelist(make_empty_graph())
  for (subgraph in graphs) {
    result <- rbind(result, get.edgelist(subgraph))
  } 
  return(graph_from_edgelist(unique(result)))
}


#### Main reviewers bipartite network
g.reviewers <- bipartite.projection(g.bi.vegas, multiplicity = TRUE)$proj2

# Remove nodes without edges
g.reviewers.filtered <- induced_subgraph(g.reviewers,
                                          vids = which(degree(g.reviewers) != 0))

# Clustering coefficient of the reviewers network is slightly higher.

transitivity(g.reviewers, type = "average")

# Network d3 reviewers

# Removing weak edges
g.reviewers.d3.filtered <- delete_edges(g.reviewers.filtered, 
                                         E(g.reviewers.filtered)[weight < 15])


# Removing disconnected nodes
g.reviewers.d3 <- induced_subgraph(g.reviewers.d3.filtered,
                                    vids = which(degree(g.reviewers.d3.filtered) != 0))

# adding dimensions
V(g.reviewers.d3)$degree <- degree(g.reviewers.d3)
V(g.reviewers.d3)$betweenness <- betweenness(g.reviewers.d3)
V(g.reviewers.d3)$evcent <- evcent(g.reviewers.d3)$vector

# turning it into a dataframe
dt.ntw.attrs.r <- as_data_frame(g.reviewers.d3, 'vertices')

dt.ntw.attrs.r <- data.table(dt.ntw.attrs.r)

cols <- names(dt.ntw.attrs.r)[2:4]
dt.ntw.attrs.r[,(cols) := round(.SD,3), .SDcols=cols]

# Adding a group, since it is required.
V(g.reviewers.d3)$nghd <- 'group two' 

g.rev <- igraph_to_networkD3(g.reviewers.d3,
                             group = V(g.reviewers.d3)$nghd)

# creating the betweenness dimension for D3 object
g.rev$nodes$betweenness <-  betweenness(g.reviewers.d3)

# function for reviewers

# function that creates a connection of neighborhoods of selected nodes
make.graph.neighborhoods.r <- function(items.to.filter) {
  
  g.original <- make_ego_graph(g.reviewers.d3,
                               order = 1,
                               nodes = V(g.reviewers.d3)[name %in% items.to.filter])
  g.union <- make.g.union(g.original)
  
  
  g.temp <- igraph_to_networkD3(g.union, 
                                group = c(rep('test', length(V(g.union)))))
  
  g.temp$nodes$betweenness <- betweenness(g.union)
  
  return(g.temp)
  
}


make.g.union <- function(graphs) {
  result <- get.edgelist(make_empty_graph())
  for (subgraph in graphs) {
    result <- rbind(result, get.edgelist(subgraph))
  } 
  return(graph_from_edgelist(unique(result)))
}

# Calculating the similarities for the main network
e.sim.jaccard <- similarity(g.businesses, method = "jaccard")
E(g.businesses)$jaccard <- e.sim.jaccard[E(g.businesses)]

e.sim.dice <- similarity(g.businesses, method = "dice")
E(g.businesses)$dice <- e.sim.dice[E(g.businesses)]

e.sim.invlogweighted <- similarity(g.businesses, method = "invlogweighted")
E(g.businesses)$invlogweighted <- normalize(e.sim.invlogweighted[E(g.businesses)])


### Functions to filter and find similar vertices
get.similar.nodes.jaccard <- function(source_node, threshold) {
  sim.edges <- E(g.businesses)[from(V(g.businesses)[source_node])]
  as_ids(head_of(g.businesses, sim.edges[jaccard >= threshold]))
}

get.similar.nodes.dice <- function(source_node, threshold) {
  sim.edges <- E(g.businesses)[from(V(g.businesses)[source_node])]
  as_ids(head_of(g.businesses, sim.edges[dice >= threshold]))
}

get.similar.nodes.invlogweighted <- function(source_node, threshold) {
  sim.edges <- E(g.businesses)[from(V(g.businesses)[source_node])]
  as_ids(head_of(g.businesses, sim.edges[invlogweighted >= threshold]))
}

# for testing
#sim.edges <- E(g.businesses)[from(V(g.businesses)["Jamba Juice"])]
#unique(as_ids(head_of(g.businesses, sim.edges[jaccard >= 0.9])))


# Function that checks membership of each a string of items against a row containing these items
check_membership <- function(keys, item) {
  if (sum(keys %in% strsplit(item, ", ")[[1]]) == length(keys)) {
    TRUE
  } else {
    FALSE
  } 
}

# function for filtering user desriptives
filter_var <- function(x, val) {
  if (is.numeric(x)) {
    !is.na(x) & x >= val[1] & x <= val[2]
  } else if (is.factor(x)) {
    x %in% val
  } else {
    # No control, so don't filter
    TRUE
  }
}


