### COUNT CHARACTERS FUNCTION

# Load data
load("Data/algo_design_data.rda")

## Function for counting the characters of each species name in a list
count_characters <- function(data) {
  if(!is.character(data)) {
    stop("Data must be a character vector")
  }
  # Remove space or underscore?
  characters <- sapply(data, function(name) nchar(gsub("[ _]", "", name)))
  return(characters)
}

# Testing (focal)
characters_beaks <- algo_design_data$Species_beaks
count_characters(characters_beaks)

characters_traits <- algo_design_data$Species_traits
count_characters(characters_traits)

characters_islands <- algo_design_data$Species_islands
count_characters(characters_islands)

# Testing (match)
characters_jetz <- algo_design_data$Jetz_tree$tip.label
count_characters(characters_jetz)

characters_clements <- algo_design_data$Clements_tree$tip.label
count_characters(characters_clements)
