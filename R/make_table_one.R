#' Tableone wrapper based off of gtsummary
#'
#' @param data
#' @param table_vars
#' @param by
#'
#' @return
#' @export
#'
#' @examples
#' table_one(data = df, table_vars = table_vars, by = ln_positive)
#'
make_table_one <- function(data, table_vars, by) {

  suppressWarnings({
    data %>%
      select({{by}}, one_of(table_vars)) %>%
      tbl_summary(
        by = {{by}},
        type = list(
          all_continuous() ~ "continuous2",
          all_categorical() ~ "categorical"
        ),
        statistic = all_continuous() ~ c("{mean} ({sd})",
                                         "{median} [{p25}, {p75}]")
      ) %>%
      add_p(
        pvalue_fun  = ~ scales::pvalue(.x),
        test = list(all_categorical() ~ 'chisq.test')
      ) %>%
      bold_labels() %>%
      add_overall()
  })

}



