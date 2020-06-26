surv_plot <-
  function(data,
           fit,
           legend_labs = NULL,
           xlim = NULL,
           break_x_by = 12,
           xlab = 'Time (Months)',
           ylab = 'Overall Survival',
           fun = NULL,
           legend_justification = c('left', 'bottom'),
           legend_location = c(.01, .01)) {

    suppressMessages({
      suppressWarnings({
        km <- survminer::ggsurvplot(
          data = data,
          fit = fit,
          palette = paletteer::paletteer_d("ggsci::category20_d3"),
          risk.table = T,
          censor = F,
          break.x.by = break_x_by,
          surv.scale = 'percent',
          axes.offset = T,
          legend.labs = legend_labs,
          legend = legend_location,
          linetype = 'strata',
          xlim = xlim,
          fontsize = 4,
          xlab = xlab,
          ylab = ylab,
          pval = F,
          fun = fun,
          ggtheme = survminer::theme_survminer() +
            theme(
              axis.title.x = element_text(face = 'bold', size = 15),
              axis.title.y = element_text(face = 'bold', size = 15),
              plot.caption = element_text(size = 5, hjust = 1),
              legend.justification = legend_justification,
              legend.text = element_text(size = 15, face = 'bold'),
              legend.key.size = unit(1, 'cm'),
              legend.background = element_blank(),
              legend.title = element_blank()
            ),
          tables.theme = theme(
            axis.title = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            axis.line = element_blank()
          )
        )

        log_rank_p <- function(fit) {
          p <- pchisq(fit$chisq, length(fit$n) - 1, lower.tail = F)

          p <-
            ifelse(p < 0.001, 'p < 0.001', paste0('p = ', format(round(p, 3), 3)))

          return(p)
        }

        f <- fit$call$formula %>% as.formula()

        lr <- survival::survdiff(f, data = data)

        p <- log_rank_p(lr)

        km$table <-
          km$table + labs(title = paste0('Logrank test: ', p)) + labs(x = '', y = '')

        km$plot <-
          km$plot + scale_y_continuous(limits = c(0, 1),
                                       labels = scales::percent_format())
      })
    })

    return(km)
  }

