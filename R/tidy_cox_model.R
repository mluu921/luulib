tidy_cox_model <- function(fit, exponentiate = T) {
  broom::tidy(fit, exponentiate = exponentiate) %>%
    mutate(
      across(c(estimate, conf.low, conf.high), ~ format(round(.x, 3), 3)),
      hr = glue::glue('{estimate} ({conf.low} to {conf.high})'),
      p.value = ifelse(p.value < 0.001, '<0.001', format(round(p.value, 3), 3))
    ) %>%
    select(
      term, hr, p.value
    )
}
