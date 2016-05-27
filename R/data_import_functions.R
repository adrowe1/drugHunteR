# import dispensing file for Echo 550


#' Import Echo 550 dispensing file
#'
#' Import the
#'
#' @param inputPath Path to a dispensing file
#'
#' @return List containing dispensing data in position 1,
#' extra information in position 2, with an md5 checksum appenedd as the final column
#' @export
#' @import readr magrittr dplyr tools
#'
#' @examples
#' tmp <- importDispensingFileEcho550(inputPath)
importDispensingFileEcho550 <- function(inputPath) {
  # extract main data
  dat <- read_csv(inputPath, skip=8)
  # extract header
  header <- read_csv(inputPath, skip=0, col_names = FALSE,  n_max = 6)
  # tidy header
  header %<>% is.na %>% apply(2, all) %>% not() %>% header[.] %>% set_colnames(c("info", "value"))
  # chop tail
  tails <- dat %>% filter(is.na(`Transfer Volume`)) %>% filter(!is.na(`Source Plate Name`))
  # tidy tail
  tails %<>% is.na %>% apply(2, all) %>% not() %>% tails[.] %>% set_colnames(c("info", "value"))
  # combine to meta info
  meta <- bind_rows(header, tails)

  # retain only data - removing tails
  dat %<>% filter(!is.na(`Transfer Volume`))

  # fix column names so that the database names match the imported (s/ /./g)
  colnames(dat) <- dat %>% colnames() %>% str_replace_all(" ", ".")

  # checksum
  checksum <- tools::md5sum(inputPath) %>% unname()

  # append checksum to datasets
  dat$checksum <- checksum
  meta$checksum <- checksum
  # append filename to meta
  meta$filename <- basename(inputPath)

  # return
  list(dat, meta)
}





# import plate reader output -------------------
#' Import plate reader data
#'
#' Import data for plate 1 from an Envision plate reader
#'
#' @param inputPath Path to file
#' @param plateRowCount number of rows on plate, defined alphabetically (caps) from A: default=16
#' @param plateColCount number of columns on plate, defined numerically from 01: default=24
#'
#' @return list with data and meta data
#' @export
#' @import readr magrittr dplyr tidyr
#'
#' @examples
#' tmp <- importPlateReaderDataEnvision(inputPath)
importPlateReaderDataEnvision <- function(inputPath, plateRowCount=16, plateColCount=24) {
  # extract main data
  dat <- read_csv2(inputPath, skip=9)
  # extract header
  header <- read_csv2(inputPath, skip=0, col_names = FALSE,  n_max = 9)
  # tidy header
  header %<>% is.na %>%
    apply(2, all) %>%
    not() %>%
    header[.] %>%
    set_colnames(c("info")) %>%
    mutate(value=NA)
  # chop tail
  tails <- dat %>% .[-(1:plateRowCount),]
  # set colname
  colnames(tails)[1] <- "info"

  # remove empty plate2 data
  tails %<>%
    filter(!info %in% LETTERS[1:plateRowCount])
  # tidy tail
  tails %<>%
    is.na %>%
    not %>%
    apply(2, sum) %>%
    is_less_than(2) %>%
    not %>%
    tails[,.] %>%
    set_colnames(c("info", "value"))
  # combine to meta info
  meta <- bind_rows(header, tails)
  meta %<>% is.na %>%
    apply(1, all) %>%
    not() %>%
    meta[.,]

  # retain only data - removing tails
  dat %<>% .[(1:plateRowCount),(1:plateColCount)]
  # correct the column name
  colnames(dat)[1] <- "row"

  dat %<>% gather(col, value, -row) %>% unite(well, row, col, sep="")

  # fix column type for value
  dat$value %<>% as.numeric()

  # checksum
  checksum <- tools::md5sum(inputPath) %>% unname()

  # append checksum to datasets
  dat$checksum <- checksum
  dat$`Keep.Flag` <- as.integer(1)
  meta$checksum <- checksum
  # append filename to meta
  meta$filename <- basename(inputPath)

  # return
  list(dat, meta)
}




# match filename to database tables ------------
#' Match filename to database table
#'
#' @param fileList vector of input files
#' @param fileClassifiers data frame of file classifiers and related database tables
#'
#' @return
#' @export
#' @import stringr dplyr magrittr
#'
#' @examples
#' matchFilenameToTable(fileList, fileClassifiers)
matchFilenameToTable <- function(fileList, fileClassifiers) {
  # lazy solution as I can't get it to work with lapply
  patternMatch <- NULL
  for (pattern in fileClassifiers$pattern){
    if (any(str_detect(fileList, pattern))) {
      tmp <- data_frame(file=fileList[str_detect(fileList, pattern)], pattern=pattern)
      patternMatch <- bind_rows(patternMatch, tmp)
    }
  }
  # join tables
  output <- left_join(patternMatch, fileClassifiers)
  # return
  output
}






# classify import files correctly and write to database ------------------
#' Put data from chosen file into correct database table
#'
#' @param fileList
#' @param fileClassifiers
#' @param dbPath
#' @param pathToZipFile
#' @param tmpdir
#' @param chosenIndividual input$dropdownSampleGroups
#'
#' @return
#' @export
#' @import utils magrittr dplyr
#'
#' @examples
#' putChosenFilesIntoDatabase(fileList, chosenIndividual, fileClassifiers, dbPath, pathToZipFile, tmpdir=tempDirectory)
#'
putChosenFilesIntoDatabase <- function(fileList, chosenIndividual, fileClassifiers, dbPath, pathToZipFile, tmpdir=tempDirectory){

  # match filename to classifiers and extract name from matching value, using this as the basis for defining the recipient table
  filematching <- matchFilenameToTable(fileList, fileClassifiers)

  # for each row in fileList import
  for (row in 1:nrow(filematching)) {
    content <- filematching[row,]
    # get file list and identify complete path to file
    filePath <- pathToZipFile %>%
      unzip(list = TRUE, exdir = tmpdir) %>%
      use_series(`Name`) %>%
      grep("^_", ., invert=TRUE, value=TRUE) %>%
      grep(".csv$", ., value=TRUE) %>%
      grep(content$file, ., value=TRUE) %>%
      grep(chosenIndividual, ., value=TRUE)
    # unzip file and import data by type
    if (content$type=="dispensing") {
      imported <- pathToZipFile %>%
        unzip(files=filePath, exdir = tmpdir) %>%
        importDispensingFileEcho550()
    } else if (content$type=="plate") {
      imported <- pathToZipFile %>%
        unzip(files=filePath, exdir = tmpdir) %>%
        importPlateReaderDataEnvision()
    }
    # write to data table
    writeTableToDB(dbPath, content$dbDataTable, imported[[1]])
    # write to metadata table
    writeTableToDB(dbPath, content$dbMetaTable, imported[[2]])
  }


}
















