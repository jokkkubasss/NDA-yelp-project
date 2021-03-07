#### This file contains all the necessary files that we will be using. ####
### RUN BEFORE RUNNING APP ###

library(data.table)

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




# Function that checks membership of each a string of items against a row containing these items
check_membership <- function(keys, item) {
  if (sum(keys %in% strsplit(item, ", ")[[1]]) == length(keys)) {
    TRUE
  } else {
    FALSE
  } 
}

cats.for.select

length(unique(dt.biz$business_id))

