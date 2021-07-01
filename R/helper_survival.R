
#' Tidy summary survfit
#'
#' @param fit
#' @param times
#'
#' @return
#' @export
#'
#' @examples

tidy_summary_survfit2 <- function(fit, times) {

  est <- summary(fit, times = times, extend = T)

  tibble(
    time = est[['time']],
    n.risk = est[['n.risk']],
    n.event = est[['n.event']],
    n.censor = est[['n.censor']],
    estimate = est[['surv']],
    std.error = est[['std.err']],
    conf.high = est[['upper']],
    conf.low = est[['lower']],
    strata = est[['strata']]
  )

}
