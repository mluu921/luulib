surv_plot <-
  function(data,
           fit,
           legend_labels = waiver(),
           break_x_by = 12,
           xlim = NULL,
           legend_location = c(.01, .01),
           legend_justification = c('left', 'bottom'),
           plot_table_size = 5,
           expand_plot = 0,
           line_annotation_size = 5) {

    ## define plot data
    plot_xlim <- if (is.null(xlim)) {
      NULL
    } else {
      xlim
    }

    plot_legend_location <- if (is.null(legend_location)) {
      'top'
    } else {
      legend_location
    }

    ## get lr test results
    f <- fit$call$formula %>% formula()

    lr <- survdiff(f, data = data)

    p <- pchisq(lr$chisq, length(lr$n) - 1, lower.tail = F)

    p <- ifelse(p < 0.001, 'p < 0.001', paste0('p = ', format(round(p, 3), 3)))

    ## create plot data
    plot_dat <- broom::tidy(fit) %>% mutate(strata = as_factor(strata))

    plot_dat <- plot_dat %>%
      filter(
        time <= plot_xlim[[2]]
      )

    plot_labels <- plot_dat %>%
      group_by(strata) %>%
      slice_tail(n = 1)

    plot <-
      ggplot(plot_dat,
             aes(
               x = time,
               y = estimate,
               color = strata,
               linetype = strata
             )) +
      geom_step(size = 1)  +
      labs(x = 'Time, mo', y = 'Overall Survival, %') +
      theme_classic(base_size = 15) +
      theme(
        legend.position = 'none',
        legend.title = element_blank(),
        axis.title = element_text(face = 'bold'),
        legend.justification = legend_justification,
        legend.background = element_blank(),
        legend.key.width = unit(3, 'lines'),
        legend.text = element_text(),
        plot.margin = unit(c(0,0,0,0), 'lines')
      ) +
      scale_y_continuous(labels = scales::percent_format(),
                         limits = c(0, 1)) +
      scale_x_continuous(breaks = seq(0, plot_xlim[[2]], break_x_by),
                         limits = c(0, plot_xlim[[2]]+expand_plot)) +
      ggsci::scale_color_d3(labels = legend_labels) +
      scale_linetype_discrete(labels = legend_labels) +
      annotate(geom = 'text', label = paste0('Logrank test: ', p), hjust = 0, x = 0, y = 0) +
      ggrepel::geom_text_repel(data = plot_labels, aes(label = legend_labels),
                               xlim = c(plot_xlim[[2]]+1, NA),
                               segment.alpha = .5,
                               point.padding = 0,
                               direction = 'both',
                               hjust = 0,
                               size = line_annotation_size,
                               segment.color = 'black') +
      guides(linetype = guide_legend(override.aes = list(size = 1)))

    temp <- summary(fit, times = seq(0, 500, break_x_by))

    table_dat <- tibble(
      time = temp$time,
      strata = temp$strata,
      n.risk = temp$n.risk
    ) %>% complete(., strata, time, fill = list(n.risk = 0)) %>%
      filter(
        time <= plot_xlim[[2]]
      )

    plot_table <-
      ggplot(table_dat, aes(x = time, y = fct_rev(strata))) +
      geom_text(aes(label = n.risk), size = plot_table_size) +
      scale_x_continuous(breaks = seq(0, max(table_dat$time), break_x_by), limits = c(0, plot_xlim[[2]]+expand_plot)) +
      scale_y_discrete(labels = rev(legend_labels)) +
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
