






test_that('tidy_survfit_summary', {

  data <- survival::lung %>% tibble::as_tibble()

  fit <- survival::survfit(survival::Surv(time, status) ~ sex, data = data)

  out <- tidy_summary_survfit2(fit, times = seq(0, 1200, 12))

  expect_s3_class(out, class = c("tbl_df", "tbl", "data.frame"))

})
