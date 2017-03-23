#' @title Combined trap rate
#'
#' @description Utilize mark-recapture estimates of trap rate, as well as observed rates queried through DART.
#'
#' @author Kevin See
#'
#' @param filepath file path to the excel file containing data
#' @param week_strata vector of intervals delimiting the weekly strata to summarize mark-recapture data over. Returned as one part of the \code{summariseLGRweekly} function.
#' @param return_weekly Should the results be summarised on weekly basis? Default is \code{TRUE}.
#' @param poor_cv_threshold The threshold for the CV of the mark-recapture estimate of trap rate. If an estimate exceeds this threshold, the calculated rate from DART will be used.
#' @param m model type to use to estimate parameters. Standard \code{Mt} and \code{Mt.bc} use \code{closedp} and \code{closedp.bc} functions from \code{Rcapture} package. \code{Chap} used the Chapman, or modified Lincoln-Peterson estimator.

#'
#' @source \url{http://www.cbr.washington.edu/dart}
#' @import lubridate httr readxl dplyr boot msm
#' @export
#' @return NULL

trapRateInputs = function(filepath = NULL,
                          week_strata = NULL,
                          return_weekly = T,
                          poor_cv_threshold = 0.7,
                          m = c('Mt.bc', 'Mt', 'Chap')) {

  trap_rate_dart = queryTrapRate(week_strata,
                                 return_weekly)

  m = match.arg(m)
  trap_rate_mr = mrTrapRate(filepath,
                            week_strata,
                            m = m)

  trap_rate = trap_rate_dart %>%
    full_join(trap_rate_mr) %>%
    dplyr::mutate(trap_open = ifelse(trap_fish > 0, T, trap_open)) %>%
    dplyr::select(Year, week_num_org, trap_open, mean_goal:calc_rate, p, p_se) %>%
    dplyr::mutate(trap_rate = p,
           trap_rate_se = p_se) %>%
    dplyr::mutate(trap_rate = ifelse(p_se / p > poor_cv_threshold | is.na(p), calc_rate, trap_rate),
           trap_rate_se = ifelse(p_se / p > poor_cv_threshold | is.na(p), 0.001, trap_rate_se),
           trap_rate_se = ifelse(trap_rate_se == 0, 0.001, trap_rate_se)) %>%
    # set up parameters describing trap rate as a beta distribution
    dplyr::mutate(trap_alpha = ((1 - trap_rate) / trap_rate_se^2 - 1 / trap_rate) * trap_rate^2,
           trap_beta = trap_alpha * (1 / trap_rate - 1),
           trap_alpha = ifelse(trap_open, trap_alpha, 1e-12),
           trap_beta = ifelse(trap_open, trap_beta, 1)) %>%
    dplyr::select(Year, week_num_org, matches('trap'))

  return(trap_rate)
}
