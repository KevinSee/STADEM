#' @title STADEM Data Summary
#'
#' @description Query and summarise data for STADEM
#'
#' @author Kevin See
#'
#' @param yr spawn year.
#' @param spp species to focus on. Currently the possible choices are: \code{Chinook} or \code{Steelhead}
#' @param dam the dam code. Currently only set up for Lower Granite (\code{LWG}). Other possible codes to be implemented in the future are: WFF (Willamette Falls), BON (Bonneville), TDA (The Dalles), JDA (John Day), MCN (McNary), IHR (Ice Harbor), LMN (Lower Monumental), LGS (Little Goose), LWG (Lower Granite), PRO (Prosser), ROZ (Roza), PRD (Priest Rapids), WAN (Wanapum), RIS (Rock Island), TUM (Tumwater), RRH (Rocky Reach), WEL (Wells), ZOS (Zosel)
#' @param damPIT the dam code for the dam you wish to query for PIT tag data. Currently only available for Lower Granite Dam (\code{GRA}).
#' @param strata_beg 3 letter code for the day of the week each weekly strata should begin on.
#' @param start_day date (\code{month / day}) when query should start
#' @param end_day date (\code{month / day}) when query should end
#' @param incl_jacks should jacks be included in the window count totals? \code{T / F}
#' @param sthd_type should window counts of steelhead be for all steelhead, or only unclipped (i.e. wild) fish? Default is \code{all}.
#' @param sthd_B_run should numbers of B run steelhead be reported? These are defined as wild steelhead greater than 780mm in length. Default is \code{FALSE}.
#' @param trap_path file path where a csv file containing the data from the fish trap is located
#' @param useDARTrate should the DART query for the trap rate be used? Default is \code{FALSE}, which implies the trap rate is estimated by PIT tags.
#' @param trap_rate_cv constant coefficient of variation (CV) that should be applied to estimates of trap rate queried by DART. Default value is \code{0}.
#' @param trap_rate_dist distributional form for trap rate prior. \code{beta} returns alpha and beta parameters for beta distribution. \code{logit} returns mean and standard deviation in logit space.
#'
#' @import lubridate dplyr boot
#' @export
#' @return NULL
#' @examples compileGRAdata(2012)

compileGRAdata = function(yr,
                          spp = c('Chinook', 'Steelhead'),
                          # dam = c('LWG', 'WFF', 'BON', 'TDA', 'JDA', 'MCN', 'IHR', 'LMN', 'LGS', 'PRO', 'ROZ', 'PRD', 'WAN', 'RIS', 'TUM', 'RRH', 'WEL', 'ZOS'),
                          dam = c('LWG'),
                          damPIT = 'GRA',
                          strata_beg = NULL,
                          start_day = NULL,
                          end_day = NULL,
                          incl_jacks = F,
                          sthd_type = c('all', 'unclipped'),
                          sthd_B_run = FALSE,
                          trap_path = NULL,
                          useDARTrate = F,
                          trap_rate_cv = 0,
                          trap_rate_dist = c('beta', 'logit')) {

  # need a year
  stopifnot(!is.null(yr))

  # if not provided, set a few defaults
  spp = match.arg(spp)
  dam = match.arg(dam)
  if(dam != 'LWG') {
    stop('Currently only works for Lower Granite (code LWG)')
  }

  sthd_type = match.arg(sthd_type)
  trap_rate_dist = match.arg(trap_rate_dist)

  # currently pit tag query only works for Lower Granite
  try( if(dam != 'LWG') stop('Dam code must be LWG') )


  #---------------------------------------------
  # query window counts
  cat('Querying window counts\n')
  win_cnts = getWindowCounts(dam = dam,
                             spawn_yr = yr,
                             spp = spp,
                             start_day = start_day,
                             end_day = end_day,
                             incl_jacks = incl_jacks,
                             sthd_type = sthd_type)

  #--------------------------------------------------------
  # query PIT tag data from previously tagged fish
  cat('Querying night passage & reascension PIT tags\n')
  pit_df = queryPITtagData(spawn_yr = yr,
                           spp = spp,
                           start_day = start_day,
                           end_day = end_day)

  #--------------------------------------------------------
  # determine weekly strata
  cat('Dividing into strata\n')
  week_strata = weeklyStrata(spawn_yr = yr,
                             spp = spp,
                             strata_beg = strata_beg)

  # read in data for Chinook and steelhead
  cat('Getting LGR trap data\n')
  if(!is.null(trap_path)) {
    trap_yr = readLGRtrapDB(trap_path = trap_path,
                            date_range = c(lubridate::ymd(lubridate::int_start(week_strata[1])),
                                           lubridate::ymd(lubridate::int_end(week_strata[length(week_strata)]) + lubridate::dseconds(1))))
  }

  if(is.null(trap_path)) {
    trap_yr = lgr_trap %>%
      # filter for date range
      dplyr::filter(Date >= lubridate::ymd(lubridate::int_start(week_strata[1])),
                    Date < lubridate::ymd(lubridate::int_end(week_strata[length(week_strata)]) + lubridate::dseconds(1))) %>%
      dplyr::arrange(Date)
  }

  # summarise by date for particular species
  trap_df = summariseLGRtrapDaily(trap_df = trap_yr,
                                  spp = spp,
                                  sthd_B_run = sthd_B_run)

  #--------------------------------------------------------
  # Query trap rate from DART
  cat('Estimating trap rate\n')

  # estimate trap rate from PIT tags
  trap_rate = tagTrapRate(trap_dataframe = trap_yr,
                          week_strata = week_strata) %>%
    dplyr::mutate(trap_open = ifelse(n_trap > 0, T, F)) %>%
    dplyr::left_join(tibble(Start_Date = int_start(week_strata),
                            week_num = 1:length(week_strata))) %>%
    dplyr::rename(n_trap_tags = n_trap,
                  n_poss_tags = n_tot, # include the tag counts going into trap rate calc.
                  trap_rate = rate,
                  trap_rate_se = rate_se) %>%
    dplyr::mutate(Start_Date = ymd(Start_Date)) %>%
    dplyr::select(Start_Date,
                  week_num,
                  trap_open,
                  everything())

  # to use DART trap rate instead
  # impose constant CV on trap rate estimates
  if(useDARTrate) {
    trap_rate = trap_rate %>%
      dplyr::left_join(queryTrapRate(week_strata,
                                     spp = spp,
                                     return_weekly = T)) %>%
      dplyr::mutate(trap_rate = ActualRateInclusiveTime,
                    # add some error
                    trap_rate_se = trap_rate * trap_rate_cv)
  }


  if(trap_rate_dist == 'beta') {
    trap_rate = trap_rate %>%
      # set up parameters describing trap rate as a beta distribution
      dplyr::mutate(trap_alpha = ((1 - trap_rate) / trap_rate_se^2 - 1 / trap_rate) * trap_rate^2,
                    trap_alpha = ifelse(trap_alpha < 0, 0.01, trap_alpha),
                    trap_beta = trap_alpha * (1 / trap_rate - 1),
                    trap_alpha = ifelse(trap_open, trap_alpha, 1e-12),
                    trap_beta = ifelse(trap_open, trap_beta, 1)) %>%
      dplyr::select(Start_Date, week_num, n_trap_tags, n_poss_tags, matches('^trap')) %>%  # include the tag observations
      dplyr::distinct()
  }

  if(trap_rate_dist == 'logit') {
    trap_rate = trap_rate %>%
      # set up parameters describing trap rate as a logit distribution
      dplyr::mutate(trap_mu = ifelse(trap_open, boot::logit(trap_rate), 1e-12),
                    trap_sd = ifelse(trap_open, (1 / n_trap) + (1 / (n_tot - n_trap)), 0)) %>%
      # trap_sd = ifelse(trap_open, boot::logit(trap_rate_se), 0)) %>%
      dplyr::select(Start_Date, week_num, matches('^trap')) %>%
      dplyr::distinct()
  }

  #--------------------------------------------------------
  # comnbine window counts, PIT tag data and trap summary on daily time-step
  cat('Combining daily data\n')
  dam_daily = win_cnts %>%
    dplyr::select(-Year) %>%
    dplyr::full_join(summarisePITdataDaily(pit_df) %>%
                       select(-SpawnYear)) %>%
    dplyr::mutate_at(vars(tot_tags:reascent_tags_H),
                     funs(ifelse(is.na(.), 0, .))) %>%
    dplyr::left_join(trap_df)

  #------------------------------------------
  # get week strata for each date
  cat('Summarising by week\n')
  dam_daily$week_num = NA
  for(i in 1:length(week_strata)) {
    dam_daily$week_num[with(dam_daily, which(Date %within% week_strata[i]))] = i
  }

  #------------------------------------------
  # summarise by week and add trap rate
  dam_weekly = dam_daily %>%
    dplyr::group_by(week_num) %>%
    dplyr::summarise(Species = unique(Species),
                     Start_Date = min(Date)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(dam_daily %>%
                       dplyr::group_by(week_num) %>%
                       dplyr::summarise_at(vars(win_cnt:n_invalid),
                                           funs(sum), na.rm = T) %>%
                       dplyr::ungroup() %>%
                       dplyr::mutate_at(vars(win_cnt:n_invalid),
                                        funs(ifelse(is.na(.), 0, .)))) %>%
    dplyr::mutate(window_open = ifelse(win_cnt > 0, T, F)) %>%
    dplyr::select(Species, Start_Date, week_num, everything()) %>%
    addTrapRate(trap_rate,
                trap_rate_dist)

  return(list('weekStrata' = week_strata,
              'trapData' = trap_yr,
              'dailyData' = dam_daily,
              'weeklyData' = dam_weekly))

}

