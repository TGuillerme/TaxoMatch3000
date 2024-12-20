# Load data
load("data/algo_design_data.rda")

# Load libraries
library(dplyr)
library(tidyr)
library(stringdist)

# To test this function I've created 2 separate df's containing missmatches:
# 1) A list of species from the dataset that don't have matches in the tree yet (focal)
# 2) A list of species from the tree that don't have matches in the dataset yet (tree)
# We want genus and species to already be separated but I've just put in
# some rough code to do that below so that we can test the lv function. Can replace with general function for this later.

# Create a list of species in Species_beaks that don't have a match in the tree
FocalMissmatches_df <- data.frame(Genus_Species = algo_design_data$Species_beaks, stringsAsFactors = FALSE) %>%
  filter(!Genus_Species %in% algo_design_data$Clements_tree$tip.label) %>%
  separate(Genus_Species, into = c("Genus", "Species"), sep = "_", remove = FALSE)

# Create a list of species from the tree that didn't have a match in Species_beaks
TreeMissmatches_df <- data.frame(Genus_Species = algo_design_data$Clements_tree$tip.label, stringsAsFactors = FALSE) %>%
  filter(!Genus_Species %in% algo_design_data$Species_beaks) %>%
  separate(Genus_Species, into = c("Genus", "Species"), sep = "_", remove = FALSE)


#### Function to find the most similar pairs based on Levenshtein distance ####

# The function takes 4 arguments: 2 dataframes to compare against, the column name to compare from, 
# and whether you want to include or remove duplicates


# The function finds the most similar species pairs between two data frames based
# on the Levenshtein distance. It iterates over each species in focal_df
# and finds the most similar species in tree_df. 
# you can specify if you'd like to match based on the 'genus' or the 'species', or whether you'd like to match
# based on a minimised combined score (so genus similarity + species similarity)
# since the function finds the closest match for each species in focal_df independently, it is possible for multiple
# species in focal_df to be matched to the same species in tree_df. The function will flag where this 
# is the case and you can optionally choose to remove duplicates, keeping only the matches with the smallest lv
# It is probably worth double checking duplicates by calling duplicates <- attr(most_similar_pairs_df, "duplicates")

find_most_similar_pairs <- function(focal_df, tree_df, genus_column, species_column,
                                    match_type = "combined", remove_duplicates = FALSE){
  # Ensure both dataframes have the specified columns as character vectors.
  focal_df <- focal_df %>%
    mutate(across(all_of(c(genus_column, species_column)), as.character))
  tree_df <- tree_df %>%
    mutate(across(all_of(c(genus_column, species_column)), as.character))
  
  # Function to find the most similar match for a single species
  find_similar <- function(focal_genus, focal_species) {
    # Calculate the Levenshtein distances for genus and species separately
    genus_distances <- stringdist::stringdist(focal_genus, tree_df[[genus_column]], method = "lv")
    species_distances <- stringdist::stringdist(focal_species, tree_df[[species_column]], method = "lv")
    
    # Determine the matching score based on the match_type argument
    matching_distances <- switch(match_type,
                                 genus = genus_distances, # if 'genus' is selected, use genus_distances
                                 species = species_distances, # if 'species' is selected use species_distances
                                 genus_distances + species_distances) #if 'combined' is selected, use the sum of both distances
    
    # Find the index of the minimum matching distance
    min_index <- which.min(matching_distances)
    
    # Get the most similar match
    similar_match <- tree_df[min_index, , drop = FALSE]
    
    # Return the match as a dataframe row
    data.frame(
      Genus_Focal = focal_genus,
      Species_Focal = focal_species,
      Genus_Tree = similar_match[[genus_column]],
      Species_Tree = similar_match[[species_column]],
      Genus_Distance = genus_distances[min_index],
      Species_Distance = species_distances[min_index],
      Combined_Distance = matching_distances[min_index],
      stringsAsFactors = FALSE
    )
  }
  
  # Apply the function to each row in focal_df
  similar_pairs <- do.call(rbind, mapply(find_similar, focal_df[[genus_column]], focal_df[[species_column]], SIMPLIFY = FALSE))
  
  # Add a column to flag duplicates in the tree matches
  similar_pairs <- similar_pairs %>%
    group_by(Genus_Tree, Species_Tree) %>%
    mutate(Duplicate_Tree_Match = n() > 1) %>%
    ungroup()
  
    # Extract duplicates before removing them
  duplicates <- similar_pairs %>%
    filter(Duplicate_Tree_Match == TRUE)

  # Remove duplicates if the option is set to TRUE.
  if (remove_duplicates) {
    similar_pairs <- similar_pairs %>%
      group_by(Genus_Tree, Species_Tree) %>%
      filter(if (match_type == "genus") {
                Genus_Distance == min(Genus_Distance)
              } else if (match_type == "species") {
                Species_Distance == min(Species_Distance)
              } else {
                Combined_Distance == min(Combined_Distance)
              }) %>%
      ungroup() %>%
      distinct(Genus_Tree, Species_Tree, .keep_all = TRUE)
  }
 
  # Add the duplicates as an attribute to the similar_pairs data frame
  attr(similar_pairs, "duplicates") <- duplicates
  
  return(similar_pairs)
}

# Test
 most_similar_pairs_df <- find_most_similar_pairs(FocalMissmatches_df, TreeMissmatches_df,
                                                      "Genus", "Species", remove_duplicates = TRUE,
                                                      match_type = "Combined")

# It looks like a combined score of 1 or 2 is always a typo/latin change
# 3 gets a bit more tricky but could indicate a change in genus, typo's, both or a wrong match. 
# Could 3's where the species is a perfect match be mostly indicative of a change in genus?

# Extract the duplicates from the result
# duplicates <- attr(most_similar_pairs_df, "duplicates")
