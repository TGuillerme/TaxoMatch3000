species_beaks <- algo_design_data$Species_beaks

## load jetz tree tip labels
jetz_tree <- algo_design_data$Jetz_tree
jetz_tips <- jetz_tree$tip.label

## create jetz dataframe splitting species and genus
jetz_df <- data.frame(do.call(rbind, strsplit(jetz_tips, "_")))
colnames(jetz_df) <- c("Genus", "Species")



## create df for species beaks
beaks_df <-  data.frame(do.call(rbind, strsplit(species_beaks, "_")))
colnames(beaks_df) <- c("Genus", "Species")


## count the number of species for each genus
count_jetz <- aggregate(species_jetz ~ genus_jetz, data = jetz_df, FUN = function(x) length(unique(x)))



## functionalise 
count_species_per_genus <- function(data) {
  # Check if the required columns exist
  if(!all(c("Genus", "Species") %in% colnames(data))) {
    stop("Data must contain 'Genus' and 'Species' columns")
  }
  
  # Aggregate to count unique species per genus
  count <- aggregate(Species ~ Genus, data = data, FUN = function(x) length(unique(x)))
  
  # Rename the count column for clarity
  colnames(count)[2] <- "Species_Count"
  
  return(count)
}

## test usage 
count_jetz <- count_species_per_genus(jetz_df)
count_beaks <- count_species_per_genus(beaks_df)


monotypic_genera_jetz <- count_jetz[count_jetz$Species_Count == 1, "Genus"]
monotypic_genera_beaks <- count_beaks[count_beaks$Species_Count == 1, "Genus"]

mismatches_jetz <- setdiff(monotypic_genera_jetz, monotypic_genera_beaks)
mismatches_beaks <- setdiff(monotypic_genera_beaks, monotypic_genera_jetz)
mismatches_list <- list(
  Jetz_Mismatches = mismatches_jetz,
  Beaks_Mismatches = mismatches_beaks
)






