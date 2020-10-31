#' Create a Kaplan-Meier Plot with Corresponding 'at risk' table
#'
#' @param data a tibble or dataframe
#' @param fit a survfit model
#' @param curve_labels labels for the legend of the curve
#' @param break_x_by sequence for the x axis
#' @param xlim limits for the x axis
#' @param plot_table_size size of the table
#' @param expand_plot expand the plot to provide more room
#' @param line_annotation_size size of the text for the annotation
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
           curve_labels = NULL,
           curve_text_size = 5) {

    ## get lr test results
    f <- fit$call$formula %>% formula()

    lr <- survdiff(f, data = data)

    p <- pchisq(lr$chisq, length(lr$n) - 1, lower.tail = F)

    p <- ifelse(p < 0.001, 'p < 0.001', paste0('p = ', format(round(p, 3), 3)))

    ## create plot data
    plot_dat <- tidy(fit) %>% mutate(strata = as_factor(strata))

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

    plot <-
      ggplot(plot_dat,
             aes(x = time,
                 y = estimate)) +
      geom_step(size = 1, aes(color = strata, linetype = strata))  +
      labs(x = 'Time, mo', y = 'Overall Survival, %') +
      theme_classic(base_size = 15) +
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
        label = paste0('Logrank test: ', p),
        hjust = 0,
        x = 0,
        y = 0
      ) +
      geom_text_repel(
        data = plot_labels,
        aes(label = strata, color = strata),
        segment.alpha = .5,
        point.padding = 0,
        nudge_x = 1,
        direction = 'y',
        hjust = 0,
        size = curve_text_size,
        segment.color = 'black'
      ) +
      scale_color_d3()

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
      theme_minimal(base_size = 15) +
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