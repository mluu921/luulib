#' Tidy survival estimates into GT tables
#'
#' @param fit survfit object
#' @param times sequences of times to generate the survival estimates for
#'
#' @return
#' @export
#' @import gt
#' @import dplyr
#' @importFrom stringr str_remove
#'
#' @examples
tidy_surv_table <- function(fit, times) {
  fit <- summary(fit, times)

  table <- tibble(
    'strata' = fit$strata %>% str_remove(., 'trt='),
    'time' = fit$time,
    'risk' = fit$n.risk,
    'event' = fit$n.event,
    'surv' = fit$surv,
    'std.error' = fit$std.err,
    'lower' = fit$lower,
    'upper' = fit$upper
  )

  out <- table %>%
    select(-std.error) %>%
    group_by(strata) %>%
    gt() %>%
    cols_label(
      time = 'Time',
      risk = '# Risk',
      event = '# Event',
      surv = 'Survival',
      lower = 'Lower 95% CI',
      upper = 'Upper 95% CI'
    ) %>%
    fmt_percent(columns = vars(surv, lower, upper),
                decimals = 1) %>%
    tab_style(
      style = cell_text(weight = 'bold'),
      locations = cells_row_groups()
    ) %>%
    tab_style(
      style = cell_text(weight = 'bold'),
      locations = cells_column_labels(everything())
    )

  return(out)
}
