setwd("C:/Users/jkras/Desktop/UNI/NDA/YELP/NDA-yelp-project")
library(data.table)
library(igraph)
library(ggplot2)
library(dplyr)

dt.las.vegas.closed <- read.csv("../las_vegas_complete.csv")
dt.las.vegas.closed <- as.data.table(dt.las.vegas.closed)

dt.las.vegas.neighborhoods <- read.csv("neighborhoods.csv")
dt.las.vegas.neighborhoods <- as.data.table(dt.las.vegas.neighborhoods)

# First, delete those stores that are not open 

dt.las.vegas.full <- dt.las.vegas.closed[dt.las.vegas.closed$is_open == 1,,]

# Second, match the neighborhoods to the postal codes. 

dt.las.vegas.full <- merge(dt.las.vegas, dt.las.vegas.neighborhoods, by = "postal_code")

# We will have two networks. First, one that focuses on businesses, and how they are connected through reviewers. (One business connects to another, if a reviewer reviewed both)
# Second, one where reviewers are connected if they both rated a certain business. 
# To make the network workable, we'll use only those reviewers that have a relatively high number of reviews, and fans.
# In essence, two projections of one bipartite network of businesses and reviewers. First, I'll get the necessary data, and reduce it.

ggplot(dt.las.vegas.full, aes(x = fans)) + geom_histogram(binwidth = 1)
quantile(dt.las.vegas.full$fans, .95)

ggplot(dt.las.vegas.full, aes(x = review_count_user)) + 
  geom_histogram(binwidth = 5)
quantile(dt.las.vegas.full$review_count_user, .95)

ggplot(dt.las.vegas.full, aes(x = votes_useful_user)) + geom_histogram()
quantile(dt.las.vegas.full$votes_useful_user, .95)

# I chose a lower quantile for businesses, because we've got less of them overall.

ggplot(dt.las.vegas.full, aes(x = review_count_business)) + geom_histogram()
quantile(dt.las.vegas.full$review_count_business, .80)

dt.vegas.graph <- dt.las.vegas.full[dt.las.vegas.full$fans > 70 & 
                                      dt.las.vegas.full$review_count_user > 613 
                                    & dt.las.vegas.full$votes_useful_user > 1907
                                    & dt.las.vegas.full$review_count_business > 1833, ]

length(unique(dt.las.vegas.full[dt.las.vegas.full$fans > 1000]$business_id))

summary(dt.las.vegas.full$fans)

ggplot(dt.las.vegas.full, aes(x = fans)) + scale_y_log10() + geom_histogram()

length(unique(dt.vegas.graph$business_id))
dt.vegas.graph <- dt.vegas.graph[, .(user_id, business_name, business_id,
                                     neighborhood_v)]

# First, I'll make two lists. One for the businesses, and one with the corresponding reviewers.

l.business <- (unique(dt.vegas.graph$business_name))
head(l.business, 20)

l.unique.reviewers <- dt.vegas.graph[business_name %in% l.business, 
                                     unique(user_id)]
head(l.unique.reviewers, 20)

# Second, I'll create the vertices for the graph. 
# Maybe use the degrees to show most interconnected nodes.

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

# Check whether it worked well
is_bipartite(g.bi.vegas)

# It did: it's a bipartite graph. Now, onto the projections. 

g.businesses <- bipartite.projection(g.bi.vegas)$proj1

summary(g.businesses)

g.users <- bipartite.projection(g.bi.vegas)$proj2

summary(g.users)

# Let's plot them. 

plot(g.businesses, vertex.label = NA)

#### Network D3

V(g.businesses)$nghd <- 'Test' 

g.biz <- igraph_to_networkD3(g.businesses, group = V(g.businesses)$nghd)

forceNetwork(Links = g.biz$links, Nodes = g.biz$nodes, Source = 'source', 
             Target = 'target', NodeID = 'name', Group = 'group', linkDistance = 200)

head(g.biz$nodes)


plot(g.users, vertex.label = NA)

# Calculate similarity of the businesses to predict next business
cocitation(g.businesses, v = V(g.businesses))

# Get a graph of predicted edges
m.predicted.edges <-
  as.matrix(cocitation(g.businesses, v = V(g.businesses)) * 
              (1-get.adjacency(g.businesses)))

g.predicted.edges <-
  graph_from_adjacency_matrix(m.predicted.edges,
                              mode = "undirected",
                              weighted = TRUE)

# Weights account for the similarity (I think)

E(g.predicted.edges)$width <-
  E(g.predicted.edges)$weight * 10

plot(g.predicted.edges, vertex.label = NA)

# Calculate similarity of the reviewers to predict best reviewer for business
cocitation(g.users, v = V(g.users))

# Get a graph of predicted edges
m.predicted.users.edges <-
  as.matrix(cocitation(g.users, v = V(g.users)) * 
              (1-get.adjacency(g.users)))

g.predicted.users.edges <-
  graph_from_adjacency_matrix(m.predicted.users.edges,
                              mode = "undirected",
                              weighted = TRUE)

E(g.predicted.users.edges)$width <-
  E(g.predicted.users.edges)$weight * 3

plot(g.predicted.users.edges, vertex.label = NA)

