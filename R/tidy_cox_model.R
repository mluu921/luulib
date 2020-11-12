#' Tidy cox model results
#'
#' @param fit cox fit object
#' @param exponentiate exponentiate the results
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
tidy_cox_model <- function(fit, exponentiate = T, conf.int = T) {
  broom::tidy(fit, exponentiate = exponentiate, conf.int = conf.int) %>%
    mutate(
      across(c(estimate, conf.low, conf.high), ~ format(round(.x, 3), 3)),
      hr = glue::glue('{estimate} ({conf.low} to {conf.high})'),
      p.value = scales::pvalue(p.value)
    ) %>%
    select(
      term, hr, p.value
    )
}
