#' @title split.match.binomial
#'
#' @description Matches binomial names focusing on species or genus
#'
#' @param binomial a binomial species name (\code{"character"})
#' @param tree a \code{"phylo"} object to match the binomial name.
#' @param focus which part of the name to focus on: \code{"species"} or \code{"genus"}?
#' @param return.index logical, whether to return the matched indices (\code{TRUE}) or the match names (\code{FALSE})
#' 
#' @details
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export


## Match the focus in the tree
split.match.binomial <- function(binomial, tree, focus, return.index = TRUE) {
    ## Get the element in the split.fun to focus on
    element <- switch(focus,
              "species" = 2,
              "genus"   = 1)

    ## Split the tree names for matching
    focal_tree_bits <- strsplit(tree$tip.label, split = "_")
    focal_tree_bits <- unlist(lapply(focal_tree_bits, `[[`, element))

    ## Split the binomial name (if needed)
    if(length(binomial) == 1) {
        binomial <- strsplit(binomial, split = "_")[[1]]
    }
    x <- binomial[element]

    ## Get the matches
    matches <- which(!is.na(match(focal_tree_bits, x)))

    ## Return null if empty
    if(length(matches) == 0) {
        return(NULL)
    }

    ## Return the matching
    if(return.index) {
        return(matches)
    } else {
        return(tree$tip.label[matches])
    }
}