# Load data
load("data/algo_design_data.rda")

# Load libraries
library(dplyr)
library(tidyr)
library(stringdist)

## To test this function I've created 2 separate df's:
## 1) A list of species from the dataset that don't have matches in the tree yet (focal)
## 2) A list of species from the tree that don't have matches in the dataset yet (tree)
## We want genus and species to already be separated but I've just put in
## some rough code to do that below so that we can test the lv function. Can replace with general function for this later.

## Create a list of species in Species_beaks that don't have a match in the tree
FocalMissmatches_df <- data.frame(Genus_Species = algo_design_data$Species_beaks, stringsAsFactors = FALSE) %>%
  filter(!Genus_Species %in% algo_design_data$Clements_tree$tip.label) %>%
  separate(Genus_Species, into = c("Genus", "Species"), sep = "_", remove = FALSE)

## Create a list of species from the tree that didn't have a match in Species_beaks
TreeMissmatches_df <- data.frame(Genus_Species = algo_design_data$Clements_tree$tip.label, stringsAsFactors = FALSE) %>%
  filter(!Genus_Species %in% algo_design_data$Species_beaks) %>%
  separate(Genus_Species, into = c("Genus", "Species"), sep = "_", remove = FALSE)

#### Function to find the most similar pairs based on Levenshtein distance ####

find_most_similar_pairs <- function(focal_df, tree_df, column_name, remove_duplicates = FALSE) {
  # Ensure both dataframes have the specified column as a character vec.
  focal_df <- focal_df %>%
  mutate(across(all_of(column_name), as.character))
  tree_df <- tree_df %>%
  mutate(across(all_of(column_name), as.character))
  
  # Function to find the most similar match for a single species
  find_similar <- function(focal_species) {
    # Calculate the Levenshtein distances to all species in tree_df
    distances <- stringdist::stringdist(focal_species, tree_df[[column_name]], method = "lv")
    
    # Find the index of the minimum distance
    min_index <- which.min(distances)
    
    # Get the most similar match
    similar_match <- tree_df[min_index, , drop = FALSE]
    
    # Return the match as a dataframe row
     data.frame(
      Species_Focal = focal_species,
      Species_Tree = similar_match[[column_name]],
      Levenshtein_Distance = distances[min_index],
      stringsAsFactors = FALSE
    )
  }
  
  # Apply the function to each species in focal_df
  similar_pairs <- do.call(rbind, lapply(focal_df[[column_name]], find_similar))
  
  # Add a column to flag duplicates in the tree matches
  similar_pairs <- similar_pairs %>%
    group_by(Species_Tree) %>%
    mutate(Duplicate_Tree_Match = n() > 1) %>%
    ungroup()
  
  # Add a column to flag duplicates in the focal matches
  similar_pairs <- similar_pairs %>%
    group_by(Species_Focal) %>%
    mutate(Duplicate_Focal_Match = n() > 1) %>%
    ungroup()

    # Remove duplicates if the option is set to TRUE.
    # It will still flagh where there WAS a duplicate,
    # but only returns the most similar pair. 
  if (remove_duplicates) {
    similar_pairs <- similar_pairs %>%
      group_by(Species_Focal) %>%
      filter(Levenshtein_Distance == min(Levenshtein_Distance)) %>%
      ungroup() %>%
      group_by(Species_Tree) %>%
      filter(Levenshtein_Distance == min(Levenshtein_Distance)) %>%
      ungroup()
  }

  return(similar_pairs)
}

# TEST USE on FocalMissmatches and TreeMissmatches
most_similar_pairs_df_full <- find_most_similar_pairs(FocalMissmatches_df, TreeMissmatches_df,
"Genus_Species", remove_duplicates = TRUE)

# Test for just species
most_similar_pairs_df_species <- find_most_similar_pairs(FocalMissmatches_df, TreeMissmatches_df,
"Species", remove_duplicates = TRUE)

# This seems to work! 
# To do:
# Display Genus_Species from focal and tree 
# try a more complex version where we calculate it for both genus and species, and then combine scores across both columns.
# Also would like to be able to display the conflicting duplicate matches.