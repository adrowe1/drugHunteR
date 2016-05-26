# file handling functions

# determine contents of zip file ------------------
#' Get Zip File Contents
#'
#' Get a list of the files included in the zip
#'
#' @param pathToZipFile
#'
#' @return dataframe with all csv files and their checksums
#'
#' @export
#' @import magrittr stringr
#'
#' @examples
#' zipFileContents(pathToZipFile)
#'
zipFileContents <- function(pathToZipFile){
  # path to zip file - probably inputFiles()$datapath in shiny reactive context

  if (is.null(pathToZipFile) ){
    allFiles <- NA

    allChecksums <- NA
  } else {
    # list all files in zip
    allFiles <- pathToZipFile %>%
      unzip(list = TRUE) %>%
      use_series(`Name`) %>%
      grep("^_", ., invert=TRUE, value=TRUE) %>%
      grep(".csv$", ., value=TRUE)

    # get checksums of all files
    allChecksums <- pathToZipFile %>%
      unzip(files=allFiles) %>%
      tools::md5sum() %>%
      unname()
  }
  # Put the outputs into a list
  output <- data_frame(files=allFiles, checksum=allChecksums)
  # return
  output
}


