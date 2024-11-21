#' @title find.closest.tree.match
#'
#' @description Find the closest partial match in a tree
#'
#' @param binomial a \code{"character"} string to match
#' @param focus which part of the name to focus on: \code{"species"} or \code{"genus"}?
#' @param tree a \code{"phylo"} object to search in.
#' @param split.fun the function for splitting names.
#' @param distance optional, an algorithm for measuring the distance.
#' 
#' @details
#' 
#' @examples
#'
#' @seealso
#' 
#' @author Thomas Guillerme
#' @export

# find.closest.tree.match <- function(binomial, tree, focus, distance) {
    
#     ## Matching the focal element on the tree
#     matches <- split.match.binomial(binomial, tree, focus, return.index = TRUE)

#     ## Check which match is the closest?
#     if(length(matches) > 1) {
#         ## Distance tracker
#         distance <- 0

#         ## Find the matching genus (distance 0)
#         matching_node <- getMRCA(tree, tip = tree$tip.label[matches])

#         ## Checking which node contains the suspected species
#         while(length(new_match <- check.match(...)) == 0) {
#             ## If not go down one node
#             matching_node <- tree$edge[which(tree$edge[,2] == matching_node), 1]
#             ## Increment the distance
#             distance <- distance + 1
#         }
#     }


#     return(NULL)
# }