# Additional functions


# without function -------------------
#' Without
#'
#' Handy function to remove existing items from a vector - borrowed from the
#' documentation for  \code{\link{match}}
#' @param x vector of values
#' @param y vector of values to be removed from \code{x}
#' @return vector of remaining values
#'
#'
#' @examples
#' 1:8 %w/o% c(1,4)
#'
#' # character vectors
#' letters[1:8] %w/o% letters[c(1,4)]
#' @export
# handy without function
"%w/o%" <- function(x, y) x[!x %in% y]





# ChEBI link function -----------------
#' ChEBI link
#'
#' Create link to CAS name search in ChEBI
#'
#' @param cas CAS reference ID for drug
#' @param name Molecule name
#'
#' @return Link string which can be put into a datatable
#' @export
#'
#' @examples
#' createChebiLink("drugID", "drugname")
#'
createChebiLink <- function(cas, name) {
  sprintf('<a href="http://www.ebi.ac.uk/chebi/advancedSearchFT.do?searchString=%s" target="_blank" class="btn btn-primary">%s</a>',cas, name)
}






