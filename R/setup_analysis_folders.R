#' Setup data analysis folder structure
#'
#' Creates standard src, reports, figures, data, munge, and docs folder
#'
#' @return
#' @export
#'
#' @examples
#'
setup_analysis_folders <- function(folders = c('src', 'reports', 'figures', 'data', 'docs', 'munge', 'tables', 'output')) {

  purrr::walk(folders, ~ fs::dir_create(.x))


}
