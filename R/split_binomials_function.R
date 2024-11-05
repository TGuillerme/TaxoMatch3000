### Function to split binomials

# Load data
load("algo_design_data.rda")

## The function itself...

# Function to split species names into genus and species names, regardless of
# whether these names are separated by an underscore or a space
split_binomials <- function(species_list) {
  df <- data.frame(do.call(rbind, strsplit(species_list, "[ _]")))
  colnames(df) <- c("Genus", "Species")
  return(df)
}

## Does the function work?

# For focal lists?
beaks_names_split <- split_binomials(algo_design_data$Species_beaks)
traits_names_split <- split_binomials(algo_design_data$Species_traits)
islands_names_split <- split_binomials(algo_design_data$Species_islands)

# For match lists?
jetz_names_split <- split_binomials(algo_design_data$Jetz_tree$tip.label)
clements_names_split <- split_binomials(algo_design_data$Clements_tree$tip.label)

# Says there are only 9993 observations for the species traits df after splitting.
# This does not match the 9995 in the original list. However, inspecting the end
# of the list shows that two entries are empty...
tail(algo_design_data$Species_traits)
# Therefore I think the function is fine!
