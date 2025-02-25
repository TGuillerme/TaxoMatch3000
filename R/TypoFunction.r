#' Match species pairs based on Levenshtein distance
#' 
#' @author Katherine Assersohn
#' 
#' @description This function finds the most similar species pairs between two data frames based on the Levenshtein distance. 
#' 
#' @param focal_df A data frame containing the focal species. Should have species and genus as separate columns.
#' @param tree_df A data frame containing the tree species to match with. Should have species and genus as separate columns.
#' @param genus_column The column in `focal_df` and `tree_df` that contains the genus names.
#' @param species_column The column in `focal_df` and `tree_df` that contains the species names.
#' @param match_type The type of match to perform. Can be one of 'genus', 'species', or 'combined' (default). If 'combined', the function uses the sum of the genus and species distances to create a combined score.
#' @param remove_duplicates Indicates whether duplicate matches should be removed. Default is `FALSE`. If TRUE, only the closest match is retained.
#' 
#' @return A data frame with columns for the focal species and its closest tree match based on Levenshtein distance, 
#'         along with the corresponding genus, species, and combined distances. Duplicates are flagged and can be removed 
#'         based on the `remove_duplicates` argument.
#' 
#' @example
#' most_similar_pairs_df <- find_most_similar_pairs(focal, match,
#'   genus_column = "Genus", species_column = "Species", remove_duplicates = TRUE, match_type = "combined")
#' 
#' # Extract the duplicates from the result
# duplicates <- attr(most_similar_pairs_df, "duplicates")

#' # Extract exact matches where the combined distance is zero
#' exact_matches <- most_similar_pairs_df %>% filter(Combined_Distance == 0)
#' 
#' # Extract matches with a combined distance less than or equal to 2
# close_matches <- most_similar_pairs_df %>% filter(Combined_Distance > 0 & Combined_Distance <= 2)

# Extract matches with a combined distance greater than or equal to 3
# no_matches <- most_similar_pairs_df %>% filter(Combined_Distance >= 3)
#' 
#' @import dplyr
#' @import tidyr
#' @import stringdist
#' @import progress
#' 
#' @export
# Load libraries
library(dplyr)
library(tidyr)
library(stringdist)
library(progress)

find_most_similar_pairs <- function(focal_df, tree_df, genus_column, species_column,
                                    match_type = "combined", remove_duplicates = FALSE) {
  
  # Ensure both dataframes have the specified columns as character vectors.
  focal_df <- focal_df %>%
    mutate(across(all_of(c(genus_column, species_column)), as.character))
  tree_df <- tree_df %>%
    mutate(across(all_of(c(genus_column, species_column)), as.character))
  
  # Initialize progress bar
  pb <- progress_bar$new(
    total = nrow(focal_df),   # Total number of species in focal_df
    format = "  Processing [:bar] :percent (:current/:total) species",
    clear = FALSE,            # Don't clear the progress bar after completion
    width = 60
  )
  
  # Function to find the most similar match for a single species
  find_similar <- function(focal_genus, focal_species) {
    # Calculate the Levenshtein distances for genus and species separately
    genus_distances <- stringdist::stringdist(focal_genus, tree_df[[genus_column]], method = "lv")
    species_distances <- stringdist::stringdist(focal_species, tree_df[[species_column]], method = "lv")
    
    # Determine the matching score based on the match_type argument
    matching_distances <- switch(match_type,
                                 genus = genus_distances,   # if 'genus' is selected, use genus_distances
                                 species = species_distances,   # if 'species' is selected use species_distances
                                 genus_distances + species_distances)   # if 'combined' is selected, use the sum of both distances
    
    # Find the index of the minimum matching distance
    min_index <- which.min(matching_distances)
    
    # Get the most similar match
    similar_match <- tree_df[min_index, , drop = FALSE]
    
    # Update the progress bar
    pb$tick()
    
    # Return the match as a dataframe row
    data.frame(
      Genus_Focal = focal_genus,
      Species_Focal = focal_species,
      Genus_Match = similar_match[[genus_column]],
      Species_Match = similar_match[[species_column]],
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
    group_by(Genus_Match, Species_Match) %>%
    mutate(Duplicate_Tree_Match = n() > 1) %>%
    ungroup()
  
  # Extract duplicates before removing them
  duplicates <- similar_pairs %>%
    filter(Duplicate_Tree_Match == TRUE)
  
  # Remove duplicates if the option is set to TRUE.
  if (remove_duplicates) {
    similar_pairs <- similar_pairs %>%
      group_by(Genus_Match, Species_Match) %>%
      filter(if (match_type == "genus") {
        Genus_Distance == min(Genus_Distance)
      } else if (match_type == "species") {
        Species_Distance == min(Species_Distance)
      } else {
        Combined_Distance == min(Combined_Distance)
      }) %>%
      ungroup() %>%
      distinct(Genus_Match, Species_Match, .keep_all = TRUE)
  }
  
  # Add the duplicates as an attribute to the similar_pairs data frame
  attr(similar_pairs, "duplicates") <- duplicates
  
  return(similar_pairs)
}
