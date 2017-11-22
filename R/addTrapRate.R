#' @title Combine fish and trap rate data
#'
#' @description Combine weekly data with trap rate data, and determine which trap rate estimates are invalid
#'
#' @author Kevin See
#'
#' @param lgr_weekly dataframe containing weekly summaries of window counts and trap data. Part of what is returned by \code{summariseLGRweekly}.
#' @param trap_rate dataframe containing estimates of trap rate
#' @param trap_rate_dist distributional form for trap rate prior. \code{beta} returns alpha and beta parameters for beta distribution. \code{logit} returns mean and standard deviation in logit space. Default is \code{beta}. Support for logit distribution is coming in future versions of STADEM.
#'
#' @import dplyr
#' @return NULL
#'
addTrapRate = function(lgr_weekly = NULL,
                       trap_rate = NULL,
                       trap_rate_dist = c('beta', 'logit')) {

  stopifnot(!is.null(lgr_weekly))
  stopifnot(!is.null(trap_rate))

  lgr_week_trapRate = lgr_weekly %>%
    dplyr::left_join(trap_rate) %>%
    dplyr::mutate(trap_open = ifelse(is.na(trap_open), F, trap_open)) %>%
    dplyr::mutate(trap_est = ifelse(trap_open, trap_fish / trap_rate, NA)) %>%
    dplyr::mutate(trap_est_se = sqrt(trap_rate_se^2 * (-trap_fish * trap_rate^(-2))^2)) %>%
    # check to see if some trap estimates seem valid
    dplyr::mutate(Prob_Less = pbinom(trap_fish, win_cnt, trap_rate, lower.tail=T),
           Prob_More = pbinom(trap_fish, win_cnt, trap_rate, lower.tail=F),
           lower_trap_lim = qbinom(0.05, win_cnt, trap_rate, lower.tail=T),
           upper_trap_lim = qbinom(0.95, win_cnt, trap_rate, lower.tail=T),
           lower_trap_est = lower_trap_lim / trap_rate,
           upper_trap_est = upper_trap_lim / trap_rate,
           trap_valid = ifelse(trap_open & lower_trap_est <= win_cnt & upper_trap_est >= win_cnt & trap_fish <= win_cnt, T, F),
           trap_valid = ifelse(trap_open & trap_fish == 0 & win_cnt > 0, F, trap_valid),
           trap_valid = ifelse(trap_rate < 0.01, F, trap_valid),
           trap_valid = ifelse(!trap_open, F, trap_valid))

  if(trap_rate_dist == 'beta') {
    lgr_week_trapRate = lgr_week_trapRate %>%
      dplyr::mutate(trap_alpha = ifelse(trap_open & trap_valid, trap_alpha, 1e-12),
             trap_beta = ifelse(trap_open & trap_valid, trap_beta, 1)) %>%
      dplyr::select(-(Prob_Less:upper_trap_est))
  }

  if(trap_rate_dist == 'logit') {
    lgr_week_trapRate = lgr_week_trapRate %>%
      dplyr::mutate(trap_mu = ifelse(trap_open & trap_valid, boot::logit(trap_rate), 1e-12),
                    trap_sd = ifelse(trap_open & trap_valid, boot::logit(trap_rate_se), 0)) %>%
      dplyr::select(-(Prob_Less:upper_trap_est))
  }

  return(lgr_week_trapRate)
}
