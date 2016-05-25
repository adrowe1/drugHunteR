# database functions - tidy connect, query, disconnect every time

# Write to database ---------------

#' Write table to database
#'
#' write a data frame to the database, appending it to an existing sqlite table
#' @param dbPath Path to the database
#' @param tableName Name of database table to append to
#' @param inboundData Data frame to be appended to table
#'
#' @return single Boolean value for matching of column names and types
#' between the input data frame and the target database table
#' @export
#' @import RSQLite magrittr dplyr
#'
#' @examples
#' writeTableToDB(dbType, dbPath, tableName, inboundData)
writeTableToDB <- function(dbPath, tableName, inboundData) {
  # open connection
  con <- dbConnect(RSQLite::SQLite(), dbname=dbPath)
  # get the column names and types of the database table
  tmp <- dbReadTable(conn=con, name=tableName)
  colnamesExisting <- tmp %>% colnames()
  coltypesExisting <- tmp %>% sapply(class) %>% unname
  # get the column names and types of the input table
  colnamesInput <- inboundData %>% colnames()
  coltypesInput <- inboundData %>% sapply(class) %>% unname
  # test all OK
  tablesMatch <- c(colnamesExisting == colnamesInput, coltypesExisting == coltypesInput) %>% all
  # write to table if the table names and types match, otherwise do nothing
  if (tablesMatch)
    dbWriteTable(conn=con, name=tableName, inboundData, append=TRUE, row.names=FALSE)
  # disconnect
  dbDisconnect(con)

  # return TRUE/FALSE for table column names and classes matching - can be used in shiny validate/need later
  tablesMatch
}







# Read from database --------------

#' Read table from database
#'
#' Read full contents of a single table from the database
#'
#' @param dbPath Path to the database
#' @param tableName Name of database table to read from
#'
#' @return A data frame (tbl_df) containing the table
#' @export
#' @import RSQLite magrittr dplyr
#'
#' @examples
#' results <- readTableFromDB(dbType, dbPath, tableName)
readTableFromDB <- function(dbPath, tableName) {
  # open connection
  con <- dbConnect(RSQLite::SQLite(), dbname=dbPath)
  # write to table
  output <- dbReadTable(conn=con, name=tableName)
  # disconnect
  dbDisconnect(con)
  # return
  output %>% tbl_df
}



# Query database --------------

#' Query database
#'
#' Run an SQL query string on the database
#'
#' @param dbPath Path to the database
#' @param SQLstring SQL string defining query to be performed
#'
#' @return A data frame (tbl_df) containing the query results
#'
#' @export
#' @import RSQLite magrittr dplyr
#'
#' @examples
#' results <- queryDB(dbType, dbPath, SQLstring)
queryDB <- function(dbPath, SQLstring) {
  # open connection
  con <- dbConnect(RSQLite::SQLite(), dbname=dbPath)
  # write to table
  output <- dbGetQuery(conn=con, SQLstring)
  # disconnect
  dbDisconnect(con)
  # return
  output %>% tbl_df
}



