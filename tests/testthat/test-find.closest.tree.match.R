# test_that("find.closest.tree.match works", {
#     load("../../data/algo_design_data.rda")

#     names <- algo_design_data$Species_beaks
#     tree  <- algo_design_data$Clements_tree

#     matches <- match(names, tree$tip.label)
#     unmatches <- names[which(is.na(matches))]
#     matches <- names[which(!is.na(matches))]

#     ## Selecting one biomial name that doesn't match
#     binomial <- unmatches[2]
#     ## Test for "Elliotomyia_chionogaster" (should be Accipiter_chiono?)

#     expect_null(find.closest.tree.match())
# })