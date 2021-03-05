#### This file contains all the necessary files that we will be using. ####
### RUN BEFORE RUNNING APP ###

library(data.table)

# load the business categories
cats.for.select <- readRDS("business_categories.rds")

# load the business dataset
dt.biz <- readRDS('bussinesses.rds')

# load the reviews dataset 


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

