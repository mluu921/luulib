#' Create a Kaplan-Meier Plot with Corresponding 'at risk' table
#'
#' @param data a tibble or dataframe
#' @param fit a survfit model
#' @param curve_labels labels for the legend of the curve
#' @param break_x_by sequence for the x axis
#' @param xlim limits for the x axis
#' @param plot_table_size size of the table
#' @param expand_plot expand the plot to provide more room
#' @param plot_title plot title
#' @param line_annotation_size size of the text for the annotation
#' @param plot_text_size controls the plot theme text size
#' @param table_text_size controls the table theme text size
#' @param pvalue_text_size controls the pvalue or label text size
#' @param log_rank controls whether to show p value for log rank test or provide string for custom label
#' @param curve_line_types logical - controls whether to have different line types for each curve
#'
#' @return
#' @export
#' @import ggplot2
#' @import ggrepel
#' @import survival
#' @import ggsci
#' @import dplyr
#' @import broom
#' @import forcats
#' @importFrom tidyr complete
#' @importFrom stats formula pchisq
#' @import patchwork
#'
#' @examples
#'
surv_plot <-
  function(data,
           fit,
           break_x_by = 12,
           xlim = NULL,
           plot_table_size = 5,
           expand_plot = 0,
           plot_title = NULL,
           curve_labels_nudge_x = 1,
           curve_labels = NULL,
           curve_text_size = 5,
           curve_line_size = 1,
           plot_text_size = 15,
           table_text_size = 15,
           pvalue_text_size = 5,
           curve_color_values = NULL,
           curve_line_types = T,
           log_rank = T,
           curve_color_palette = ggsci::pal_d3(),
           xlab = 'Time, mo',
           ylab = 'Overall Survival, %') {

    if(log_rank == T) {
      ## get lr test results
      f <- fit$call$formula %>% formula()

      lr <- survdiff(f, data = data)

      p <- pchisq(lr$chisq, length(lr$n) - 1, lower.tail = F)

      p <- ifelse(p < 0.001, 'p < 0.001', paste0('p = ', format(round(p, 3), 3)))

      p_label <- paste0('Logrank test: ', p)

    } else {
      p_label <- log_rank

    }

    ## create plot data

    s <- summary(fit, times = c(0, unique(fit$time)))

    plot_dat <- tibble(
      time = c(s$time),
      n.risk = c(s$n.risk),
      n.event = c(s$n.event),
      n.censor = c(s$n.censor),
      estimate = c(s$surv),
      std.error = c(s$std.err),
      conf.high = c(s$upper),
      conf.low = c(s$lower),
      strata = c(s$strata)
    ) %>% mutate(strata = as_factor(strata))

    if(is.null(xlim)) {

      plot_dat

      plot_xlim <- c(0, max(plot_dat$time))


    } else {

      plot_xlim <- xlim

      plot_dat <- plot_dat %>%
        filter(
          time <= plot_xlim[[2]]
        )

    }

    plot_labels <- plot_dat %>%
      group_by(strata) %>%
      slice_tail(n = 1) %>%
      ungroup()

    if (is.null(curve_labels)) {
      plot_labels <- plot_labels %>%
        mutate(strata = as_factor(strata))

      plot_dat <- plot_dat %>%
        mutate(strata = as_factor(strata))
    } else {
      plot_labels <- plot_labels %>%
        mutate(strata = as_factor(strata)) %>%
        mutate(strata = factor(strata, labels = curve_labels))

      plot_dat <- plot_dat %>%
        mutate(strata = as_factor(strata)) %>%
        mutate(strata = factor(strata, labels = curve_labels))
    }

    if (curve_line_types == T) {
      linetype <- rlang::quo(strata)

    }

    if (curve_line_types == F) {
      linetype <- NULL
    }

    plot <-
      ggplot(plot_dat,
             aes(x = time,
                 y = estimate)) +
      geom_step(size = curve_line_size, aes(color = strata, linetype = !!linetype))  +
      labs(x = xlab, y = ylab, title = plot_title) +
      theme_classic(base_size = plot_text_size) +
      theme(
        legend.position = 'none',
        legend.title = element_blank(),
        axis.title = element_text(face = 'bold'),
        legend.background = element_blank(),
        legend.key.width = unit(3, 'lines'),
        legend.text = element_text(),
        plot.margin = unit(c(0, 0, 0, 0), 'lines')
      ) +
      scale_y_continuous(labels = scales::percent_format(),
                         limits = c(0, 1)) +
      scale_x_continuous(
        breaks = seq(0, plot_xlim[[2]], break_x_by),
        limits = c(0, plot_xlim[[2]] + expand_plot)
      ) +
      annotate(
        geom = 'text',
        label = p_label,
        hjust = 0,
        x = 0,
        y = 0,
        size = pvalue_text_size
      ) +
      geom_text_repel(
        data = plot_labels,
        aes(label = strata, color = strata),
        segment.alpha = .5,
        point.padding = 0,
        nudge_x = curve_labels_nudge_x,
        direction = 'y',
        hjust = 0,
        size = curve_text_size,
        segment.color = 'black',
        min.segment.length = Inf
      ) +
      scale_color_manual(palette = curve_color_palette, values = curve_color_values) +
      coord_cartesian(clip = 'off')

    temp <- summary(fit, times = seq(0, 500, break_x_by))

    table_dat <- tibble(
      time = temp$time,
      strata = temp$strata,
      n.risk = temp$n.risk
    ) %>% complete(., strata, time, fill = list(n.risk = 0)) %>%
      filter(
        time <= plot_xlim[[2]]
      )

    if (is.null(curve_labels)) {
      table_dat <- table_dat %>%
        mutate(strata = as_factor(strata))
    } else {
      table_dat <- table_dat %>%
        mutate(strata = as_factor(strata)) %>%
        mutate(strata = factor(strata, labels = curve_labels))
    }

    plot_table <-
      ggplot(table_dat, aes(x = time, y = fct_rev(strata))) +
      geom_text(aes(label = n.risk), size = plot_table_size) +
      scale_x_continuous(breaks = seq(0, max(table_dat$time), break_x_by), limits = c(0, plot_xlim[[2]]+expand_plot)) +
      labs(title = 'No. at Risk') +
      theme_minimal(base_size = table_text_size) +
      theme(
        plot.title = element_text(size = 12),
        panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 12),
        plot.margin = unit(c(0,0,0,0), 'lines')
      )

    out <-
      plot / plot_table + plot_layout(nrow = 2, height = c(5, 1.75))

    return(out)


  }
