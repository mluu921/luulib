#' Helper function to extract survfit summary objects
#'
#' @param fit summary survfit object
#'
#' @return
#' @export
#'
#' @examples
#'
#' tidy_summary_survfit
#'
#'
tidy_summary_survfit <- function(fit) {

  tibble(
    time = fit$time,
    n.risk = fit$n.risk,
    n.event = fit$n.event,
    n.censor = fit$n.censor,
    estimate = fit$surv,
    std.error = fit$std.err,
    conf.high = fit$upper,
    conf.low = fit$lower,
    strata = fit$strata
  )
}

#' Title
#'
#' @param data
#' @param fit
#' @param break_x_by
#' @param xlim
#' @param curve_labels
#' @param curve_text_size
#' @param logrank_test
#' @param table_title
#' @param table_title_size
#' @param table_text_size
#' @param table_label_spacing
#' @param plot_table_ratio
#' @param pvalue_text_size
#' @param pvalue_text_location
#' @param curve_color_palette
#' @param xlab
#' @param ylab
#' @param theme_plot_text_size
#'
#' @return
#' @export
#'
#' @examples
surv_plot <- function(data,
                      fit,
                      break_x_by = 12,
                      xlim = NULL,
                      curve_labels = NULL,
                      curve_text_size = 5,
                      logrank_test = T,
                      table_title = 'No. at risk',
                      table_title_size = 15,
                      table_text_size = 5,
                      table_label_spacing = 20,
                      plot_table_ratio = c(.9, .1),
                      pvalue_text_size = 5,
                      pvalue_text_location = c(.02, .02),
                      curve_color_palette = ggsci::pal_d3(),
                      expand_plot_area = 12,
                      theme_plot_text_size = 15,
                      plot_margin =  margin(10, 50, 0, 50),
                      table_margin = margin(0, 0, 15, 0),
                      xlab = 'Time, mo',
                      ylab = 'Overall Survival, %') {

  # extract the plot data ---------------------------------------------------

  plot_data <- summary(fit, times = c(0, unique(fit$time))) %>%
    tidy_summary_survfit() %>%
    filter(time <= xlim[[2]]+1) %>%
    mutate(strata = as_factor(strata))

  if (!is.null(curve_labels)) {
    plot_data <- plot_data %>%
      mutate(strata = factor(strata, labels = curve_labels))
  }

  # extract the last point for the plot labels ------------------------------

  labels <- plot_data %>%
    group_by(strata) %>%
    filter(time == max(time)) %>%
    ungroup()

  # log rank test -----------------------------------------------------------

  lr <- survdiff(eval(fit$call$formula, envir = parent.frame()), data = data)

  p <-
    pchisq(lr$chisq, length(lr$n) - 1, lower.tail = F) %>% scales::pvalue(., add_p = T)

  if (logrank_test == T) {
    annotate_p <- annotate(
      'text',
      x = max(plot_data$time) * pvalue_text_location[[1]],
      y = pvalue_text_location[[2]],
      label = paste0('Logrank Test: ', p),
      hjust = 0,
      vjust = 0,
      size = pvalue_text_size
    )

  } else {

    annotate_p <- NULL

  }

  # generate the color pallete ----------------------------------------------

  if (class(curve_color_palette) == 'function') {
    colors <- curve_color_palette(length(labels$strata))

    curve_colors <- scale_color_manual(values = colors)

  }

  if (class(curve_color_palette) == 'character') {
    curve_colors <- scale_color_manual(values = curve_color_palette)

  }


  # create the plot ---------------------------------------------------------

  plot <- ggplot(plot_data, aes(x = time, y = estimate)) +
    geom_step(aes(group = strata, color = strata), size = 1) +
    scale_x_continuous(breaks = seq(0, max(plot_data$time), break_x_by), expand = expansion(mult = .1)) +
    coord_cartesian(
      xlim = c(xlim[[1]], xlim[[2]]),
      ylim = c(0, 1),
      expand = F,
      clip = 'off'
    ) +
    theme_classic(base_size = theme_plot_text_size) +
    labs(x = xlab, y = ylab) +
    theme(
      axis.line.y.right = element_blank(),
      axis.ticks.y.right = element_blank(),
      legend.position = 'none',
      axis.title = element_text(face = 'bold'),
      plot.margin = plot_margin
    ) +
    scale_y_continuous(
      labels = scales::percent_format()
    ) +
    ggrepel::geom_text_repel(
      data = labels,
      aes(x = time, y = estimate, label = strata, color = strata),
      segment.alpha = .25,
      size = curve_text_size,
      fontface = 'bold',
      direction = 'y',
      point.padding = 0,
      nudge_x = 1,
      hjust = 0,
      vjust = .5,
      xlim = c(0, Inf)) +
    annotate_p +
    curve_colors

  # extract the table data --------------------------------------------------

  table_data <-
    summary(fit, times = seq(0, xlim[[2]], break_x_by)) %>%
    tidy_summary_survfit() %>%
    mutate(strata = as_factor(strata))

  if (!is.null(curve_labels)) {
    table_data <- table_data %>%
      mutate(strata = factor(strata, labels = curve_labels))
  }

  # create the table --------------------------------------------------------

  table <-
    ggplot(table_data, aes(x = time, y = fct_rev(strata)))   +
    geom_text(aes(label = n.risk), size = table_text_size) +
    theme_bw(base_size = 15) +
    theme(
      axis.text.y = element_text(margin = margin(0, table_label_spacing, 0, 0)),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text.x = element_blank(),
      plot.title = element_text(margin = margin(0, 0, 15, 0), size = table_title_size),
      plot.title.position = 'panel',
      plot.margin = table_margin
    ) +
    coord_cartesian(xlim = c(xlim[[1]], xlim[[2]]),
                    expand = F,
                    clip = 'off') +
    labs(title = table_title)

  plot / table +
    plot_layout(heights = plot_table_ratio)

}


# fit <- survfit(Surv(time, status) ~ sex, data = data)
#
# surv_plot(
#   data = data,
#   fit = fit,
#   xlim = c(0, 60)
# )



