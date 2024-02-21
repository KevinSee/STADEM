#' @title Time Series Plot
#'
#' @description Plot window counts, trap estimates and STADEM estimates of total escapement through time.
#'
#' @author Kevin See
#'
#' @param stadem_mod `mcmc.list` or `jagsUI` object returned from running \code{runSTADEMmodel}.
#' @param weeklyData weekly data compiled by \code{compileGRAdata}, with the name \code{weeklyData} in the resulting list.
#'
#' @import rjags dplyr ggplot2
#' @return NULL
#' @export

plotTimeSeries = function(stadem_mod = NULL,
                          weeklyData = NULL) {

  stopifnot(!is.null(stadem_mod))
  stopifnot(!is.null(weeklyData))

  # make sure stadem_mod is mcmc.list
  if(inherits(stadem_mod, "jagsUI")) {
    stadem_mod = stadem_mod$samples
  }

  stopifnot(!is.null(stadem_mod),
            inherits(stadem_mod, c('mcmc', 'mcmc.list')))


  plot_df = summary(stadem_mod)$quantiles %>%
    as_tibble(rownames = 'var') %>%
    filter(grepl('^X.all', var)) %>%
    mutate(week = as.integer(str_extract(var, "[0-9]+")),
           param = str_extract_all(var, "[:alpha:]+", simplify = T)[,3],
           param = ifelse(param == '', 'all', param)) %>%
    select(var, param, week, everything()) %>%
    left_join(weeklyData %>%
                rename(week = week_num)) %>%
    # adjust window count for night-time passage
    mutate(win_cnt_adj = win_cnt / (day_tags / tot_tags)) %>%
    mutate_at(vars(win_cnt_adj,
                   trap_est),
              list(~ if_else(. == Inf,
                             as.numeric(NA),
                             .))) %>%
    # mark trap estimate as NA if not considered valid
    mutate(trap_est_valid = if_else(trap_valid,
                                    trap_est,
                                    as.numeric(NA)))


  plot_df %>%
    filter(param == 'all') %>%
    ggplot(aes(x = Start_Date)) +
    geom_ribbon(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                alpha = 0.2) +
    geom_line(aes(y = win_cnt_adj,
                  color = 'Window (adj)')) +
    geom_point(aes(y = win_cnt_adj,
                   color = 'Window (adj)')) +
    geom_line(aes(y = win_cnt,
                  color = 'Window (raw)')) +
    geom_point(aes(y = win_cnt,
                   color = 'Window (raw)')) +
    geom_line(aes(y = trap_est_valid,
                  color = 'Trap')) +
    geom_point(aes(y = trap_est,
                   shape = trap_valid,
                   color = 'Trap')) +
    scale_shape_manual(values = c('TRUE' = 19,
                                  'FALSE' = 1),
                       name = 'Valid Est.') +
    geom_line(aes(y = `50%`,
                  color = 'Model')) +
    geom_point(aes(y = `50%`,
                   color = 'Model')) +
    scale_color_manual(values = c('Model' = 'black',
                                  'Window (raw)' = 'lightblue',
                                  'Window (adj)' = 'blue',
                                  'Trap' = 'red')) +
    theme(legend.position = 'bottom') +
    labs(x = 'Date',
         y = 'Estimate',
         color = 'Source',
         title = 'Total Escapement')

}
