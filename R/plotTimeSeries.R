#' @title Time Series Plot
#'
#' @description Plot window counts, trap estimates and STADEM estimates of total escapement through time.
#'
#' @author Kevin See
#'
#' @param stadem_mod jagsUI object returned from running \code{runSTADEMmodel}.
#' @param weeklyData weekly data compiled by \code{compileGRAdata}, with the name \code{weeklyData} in the resulting list.
#'
#' @import dplyr ggplot2
#' @return NULL
#' @export

plotTimeSeries = function(stadem_mod = NULL,
                          weeklyData = NULL) {

  stopifnot(!is.null(stadem_mod))
  stopifnot(!is.null(weeklyData))


  plot_df = stadem_mod$summary[grep('^X.all', rownames(stadem_mod$summary)),] %>%
    as_tibble(rownames = 'var') %>%
    mutate(week = as.integer(str_extract(var, "[0-9]+")),
           param = str_extract_all(var, "[:alpha:]+", simplify = T)[,3],
           param = ifelse(param == '', 'all', param)) %>%
    select(var, param, week, everything()) %>%
    left_join(weeklyData %>%
                rename(week = week_num))


  plot_df %>%
    filter(param == 'all') %>%
    ggplot(aes(x = Start_Date,
               y = `50%`)) +
    geom_ribbon(aes(ymin = `2.5%`,
                    ymax = `97.5%`),
                alpha = 0.2) +
    geom_line(aes(y = win_cnt / (day_tags / tot_tags),
                  color = 'Window (adj)')) +
    geom_point(aes(y = win_cnt / (day_tags / tot_tags),
                   color = 'Window (adj)')) +
    geom_line(aes(y = win_cnt,
                  color = 'Window (raw)')) +
    geom_point(aes(y = win_cnt,
                   color = 'Window (raw)')) +
    geom_line(aes(y = trap_est,
                  color = 'Trap')) +
    geom_point(aes(y = trap_est,
                   color = 'Trap')) +
    geom_line(aes(color = 'Model')) +
    geom_point(aes(color = 'Model')) +
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
