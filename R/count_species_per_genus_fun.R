 ## input dataframe should be genera and species as separate columns
 count_species_per_genus <- function(data) {
  # Check if the required columns exist
  
  # use aggregate function to count unique species per genus
  count <- aggregate(Species ~ Genus, data = data, FUN = function(x) length(unique(x)))
  
  # Rename the count column for clarity
  colnames(count)[2] <- "Species_Count"
  
  return(count)
}



