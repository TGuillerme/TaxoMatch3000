### Function to rank species name comparisons by number of mismatches
# For detecting typos or small adjustments in spelling


## Load data
load("algo_design_data.rda")


## The function itself...

# Made sense to count then rank mismatches within the same function??

# Requires stringdist package to calculate mismatches (levenshtein distance).
# Levenshtein distance is the minimum number of single-character edits required 
# to change one string into the other.

rank_mismatches <- function(species_list1, species_list2, max_mismatch = 2) {

  # Include progress bar
  pb <- txtProgressBar(min = 0, max = length(species_list1), style = 3)
  
  # Create an empty results data frame
  results <- data.frame(Species1 = character(), Species2 = character(), Mismatches = integer(), stringsAsFactors = FALSE)
  
  # Loop through each species in list1
  for (i in seq_along(species_list1)) {
    # Calculate mismatches for each species in list2
    distances <- stringdist::stringdist(species_list1[i], species_list2, method = "lv")
    
    # Exclude exact matches and filter based on the max mismatch threshold (more
    # than 2 mismatches unlikely to be simple typo)
    filtered_indices <- which(distances > 0 & distances <= max_mismatch)
    
    # Store results
    for (j in filtered_indices) {
      results <- rbind(results, data.frame(Species1 = species_list1[i], Species2 = species_list2[j], Mismatches = distances[j]))
    }
    
    # Update progress bar
    setTxtProgressBar(pb, i)
  }
  
  # Close the progress bar
  close(pb)
  
  # Sort and return results
  results <- results[order(results$Mismatches), ]
  return(results)
}


### Does the function work?
ranked_mismatches <- rank_mismatches(algo_design_data$Species_beaks, algo_design_data$Clements_tree$tip.label)

View(ranked_mismatches)
