test_that("split.match.binomial works", {
    load("../../data/algo_design_data.rda")

    names <- algo_design_data$Species_beaks
    tree  <- algo_design_data$Clements_tree

    matches <- match(names, tree$tip.label)
    unmatches <- names[which(is.na(matches))]
    matches <- names[which(!is.na(matches))]

    ## Selecting one biomial name that doesn't match
    one_nomatch <- unmatches[2]
    ## Selecting one that works
    one_match <- matches[1]

    ## Matching the genus and species name for the match
    genus_match <- split.match.binomial(one_match, tree, focus = "genus")
    species_match <- split.match.binomial(one_match, tree, focus = "species")

    ## Six genus names
    expect_equal(genus_match, c(8408))
    expect_equal(species_match, c(369, 511, 768, 1047, 8408))

    expect_equal(split.match.binomial(one_match, tree, focus = "genus", return.index = FALSE), "Abeillia_abeillei")
    expect_equal(split.match.binomial(one_match, tree, focus = "species", return.index = FALSE), c("Orchesticus_abeillei", "Icterus_abeillei","Arremon_abeillei","Coccothraustes_abeillei","Abeillia_abeillei"))

    ## No matching
    expect_equal(split.match.binomial("Elliotomyia_sapiens", tree, focus = "genus", return.index = TRUE), c(8328, 8329))
    expect_null(split.match.binomial("Elliotomyia_sapiens", tree, focus = "species", return.index = TRUE))
})