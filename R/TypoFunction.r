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
# on the Levenshtein distance. It basically iterates over each species in focal_df
# and find the most similar species in tree_df (each species of focal_df is matched
# to one species in tree_df). But since the function finds the closest match for each
# species in focal_df independently, it is possible for multiple species in focal_df
# to be matched to the same species in tree_df. The function will flag where this is the case
# and you can optionally choose to remove duplicates, keeping only the matches with the smallest lv
# It is probably worth double checking duplicates before choosing to remove them though, and checking both 
# the genus and species


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

    # Remove duplicates if the option is set to TRUE.
    # It will still flagh where there WAS a duplicate,
    # but only returns the most similar pair. 
  if (remove_duplicates) {
    similar_pairs <- similar_pairs %>%
      group_by(Species_Tree) %>%
      filter(Levenshtein_Distance == min(Levenshtein_Distance)) %>%
      ungroup()
  }

  return(similar_pairs)
}

# TEST USE on FocalMissmatches and TreeMissmatches
most_similar_pairs_df_full <- find_most_similar_pairs(FocalMissmatches_df, TreeMissmatches_df,
"Genus_Species", remove_duplicates = FALSE)

# Test for just species
most_similar_pairs_df_species <- find_most_similar_pairs(FocalMissmatches_df, TreeMissmatches_df,
"Species", remove_duplicates = FALSE)



## A version of the above function that now also takes both the species and genus as an argument, and an
# argument 'match_type', where you can specify if you'd like to match based on the 'genus' or the 'species',
# or whether you'd like to match based on a minimised combined score (so genus similarity + species similarity)

find_most_similar_pairs2 <- function(focal_df, tree_df, genus_column, species_column, match_type = "combined", remove_duplicates = FALSE) {
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
    if (match_type == "genus") {
      matching_distances <- genus_distances
    } else if (match_type == "species") {
      matching_distances <- species_distances
    } else {
      # Default to combined score
      matching_distances <- genus_distances + species_distances
    }
    
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
      Matching_Distance = matching_distances[min_index],
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
  
  # Remove duplicates if the option is set to TRUE.
  if (remove_duplicates) {
    similar_pairs <- similar_pairs %>%
      group_by(Genus_Focal, Species_Focal) %>%
      filter(Matching_Distance == min(Matching_Distance)) %>%
      ungroup() %>%
      group_by(Genus_Tree, Species_Tree) %>%
      filter(Matching_Distance == min(Matching_Distance)) %>%
      ungroup()
  }
  
  return(similar_pairs)
}

most_similar_pairs_df_full <- find_most_similar_pairs2(FocalMissmatches_df, TreeMissmatches_df,
                                                      "Genus", "Species", remove_duplicates = FALSE,
                                                       match_type = "combined")
# It looks like a combined score of 1 or 2 is always a typo/latin change
# 3 gets a bit more tricky but could indicate a change in genus, typo's, both or a wrong match. 
# Could 3's where the species is a perfect match be mostly indicative of a change in genus?