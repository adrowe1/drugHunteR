# import dispensing file for Echo 550


#' Import Echo 550 dispensing file
#'
#' Import the
#'
#' @param inputPath Path to a dispensing file
#'
#' @return List containing dispensing data in position 1,
#' extra information in position 2, and an md5 checksum in position 3
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

  # checksum
  checksum <- tools::md5sum(inputPath) %>% unname()

  # return
  list(dat, meta, checksum)
}





