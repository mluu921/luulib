#' Append today's date to the file that is being saved
#'
#' @param file_name a character file name the date will be appended onto
#'
#' @return
#' @export
#'
#' @examples
#'
#' save_date('new_plot.jpg')

save_date <- function(file_name) {
  stringr::str_replace(file_name, '\\.', glue::glue('_{Sys.Date()}.'))
}

