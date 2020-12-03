#' Setup data analysis folder structure
#'
#' Creates standard src, reports, figures, data, and docs folder
#'
#' @return
#' @export
#'
#' @examples
#'
setup_analysis_folders <- function() {

  folders <- c('src', 'reports', 'figures', 'data', 'docs')

  purrr::walk(folders, ~ fs::dir_create(.x))


}
