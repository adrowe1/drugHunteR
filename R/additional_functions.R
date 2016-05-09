# Additional functions


# tmp -------------------
#' FIXME
#' @param fixme a
#' @return fixme b
#'
#'
#' @examples
#' fixme

#' @export
#' @import dplyr magrittr
# handy without function
"%w/o%" <- function(x, y) x[!x %in% y]

# # SQLite query
# SQLiteQuery <- function(databasePath, databaseName, sqlQuery){
#   db <- dbConnect(SQLite(), dbname=file.path(databasePath, databaseName))
#   reply <- dbGetQuery(db, sqlQuery )
#   dbDisconnect(db)
#   reply
# }

# Create link to ChEBI
createLink <- function(cas, name) {
  sprintf('<a href="http://www.ebi.ac.uk/chebi/advancedSearchFT.do?searchString=%s" target="_blank" class="btn btn-primary">%s</a>',cas, name)
}

##### END FUNCTIONS ###########




