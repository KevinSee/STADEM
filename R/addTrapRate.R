#' @title Combine fish and trap rate data
#'
#' @description Combine weekly data with trap rate data, and determine which trap rate estimates are invalid
#'
#' @author Kevin See
#'
#' @param dam_weekly dataframe containing weekly summaries of window counts and trap data. Part of what is returned by \code{summariseLGRweekly}.
#' @param trap_rate dataframe containing estimates of trap rate
#' @param trap_rate_dist distributional form for trap rate prior. \code{beta} returns alpha and beta parameters for beta distribution. \code{logit} returns mean and standard deviation in logit space. Default is \code{beta}. Support for logit distribution is coming in future versions of STADEM.
#'
#' @import dplyr
#' @return NULL
#'
addTrapRate = function(dam_weekly = NULL,
                       trap_rate = NULL,
                       trap_rate_dist = c('beta', 'logit')) {

  stopifnot(!is.null(dam_weekly))
  stopifnot(!is.null(trap_rate))

  dam_week_trapRate = dam_weekly %>%
    left_join(trap_rate,
              by = join_by(Start_Date,
                           week_num)) |>
              # by = c('Start_Date', 'week_num')) %>%
    mutate(trap_open = ifelse(is.na(trap_open), F, trap_open)) %>%
    mutate(trap_est = ifelse(trap_open, trap_fish / trap_rate, NA)) %>%
    mutate(trap_est_se = sqrt(trap_rate_se^2 * (-trap_fish * trap_rate^(-2))^2)) %>%
    # check to see if some trap estimates seem valid
    rowwise() %>%
    mutate(lower_trap_lim = quantile(rbinom(2000, win_cnt, max(0, trap_rate + qnorm(0.025) * trap_rate_se)),
                                     c(0.025),
                                     na.rm = T),
           upper_trap_lim = quantile(rbinom(2000, win_cnt, min(1, trap_rate + qnorm(0.975) * trap_rate_se)),
                                     c(0.975),
                                     na.rm = T),
           trap_valid = if_else(lower_trap_lim <= trap_fish & upper_trap_lim >= trap_fish & trap_open,
                                T, F),
           trap_valid = ifelse(trap_open & trap_fish == 0 & win_cnt > 0, F, trap_valid),
           trap_valid = ifelse(trap_rate < 0.01, F, trap_valid),
           trap_valid = ifelse(!trap_open, F, trap_valid)) %>%
    ungroup()
    # mutate(Prob_Less = pbinom(trap_fish, win_cnt, trap_rate, lower.tail=T),
    #        Prob_More = pbinom(trap_fish, win_cnt, trap_rate, lower.tail=F),
    #        lower_trap_lim = qbinom(0.05, win_cnt, trap_rate, lower.tail=T),
    #        upper_trap_lim = qbinom(0.95, win_cnt, trap_rate, lower.tail=T),
    #        lower_trap_est = lower_trap_lim / trap_rate,
    #        upper_trap_est = upper_trap_lim / trap_rate,
    #        trap_valid = ifelse(trap_open & lower_trap_est <= win_cnt & upper_trap_est >= win_cnt & trap_fish <= win_cnt, T, F),
    #        trap_valid = ifelse(trap_open & trap_fish == 0 & win_cnt > 0, F, trap_valid),
    #        trap_valid = ifelse(trap_rate < 0.01, F, trap_valid),
    #        trap_valid = ifelse(!trap_open, F, trap_valid))

  if(trap_rate_dist == 'beta') {
    dam_week_trapRate = dam_week_trapRate %>%
      # set up parameters describing trap rate as a beta distribution
      mutate(trap_alpha = ((1 - trap_rate) / trap_rate_se^2 - 1 / trap_rate) * trap_rate^2,
             trap_alpha = ifelse(trap_alpha < 0, 0.01, trap_alpha),
             trap_beta = trap_alpha * (1 / trap_rate - 1),
             trap_alpha = ifelse(trap_open, trap_alpha, 1e-12),
             trap_beta = ifelse(trap_open, trap_beta, 1)) %>%
      # fix some parameter values if trap not open or invalid
      mutate(trap_alpha = ifelse(trap_open & trap_valid, trap_alpha, 1e-12),
             trap_beta = ifelse(trap_open & trap_valid, trap_beta, 1))
  }

  if(trap_rate_dist == 'logit') {
    dam_week_trapRate = dam_week_trapRate %>%
      # set up parameters describing trap rate as a logit distribution
      mutate(trap_mu = ifelse(trap_open, boot::logit(trap_rate), 1e-12),
             trap_sd = ifelse(trap_open, (1 / n_trap_tags) + (1 / (n_poss_tags - n_trap_tags)), 0)) %>%
      # fix some parameter values if trap not open or invalid
      mutate(trap_mu = ifelse(trap_open & trap_valid, trap_mu, -25),
             trap_sd = ifelse(trap_open & trap_valid, trap_sd, 0))
  }

  return(dam_week_trapRate)
}
