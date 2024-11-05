## COUNT CHARACTERS FUNCTION

## Function for counting the characters of each species name in a list
count_characters <- function(data) {
  # Check for character vector
  if(!is.character(data)) {
    stop("Data must be a character vector")
  }
  # Remove space or underscore?
  characters <- sapply(data, function(name) nchar(gsub("[ _]", "", name)))
  return(characters)
}

# Testing with beaks dataset (MATCH)
library(dplyr)
birds <- read.csv("Data/algo_design_data.rda")
match <- algo_design_data$Species_beaks

count_characters(match)

# Testing with Jetz dataset (FOCAL)
tree <-read.csv("Data/algo_design_data.rda")
focal <- algo_design_data$Jetz_tree$tip.label

count_characters(focal)
