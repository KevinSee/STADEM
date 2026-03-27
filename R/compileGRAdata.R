#' @title STADEM Data Summary
#'
#' @description Query and summarise data for STADEM
#'
#' @author Kevin See
#'
#' @inheritParams getWindowCounts
#' @param yr spawn year.
#' @param damPIT the dam code for the dam you wish to query for PIT tag data.
#'   Currently only available for Lower Granite Dam (\code{LWG}) and Priest Rapids Dam (\code{PRA}).
#' @param strata_beg 3 letter code for the day of the week each weekly strata
#'   should begin on. Default value is \code{'Mon'}.
#' @param last_strata_min minimum length (in days) for the final strata. Default
#'   value is 3.
#' @param sthd_B_run should numbers of B run steelhead be reported? These are
#'   defined as wild steelhead greater than 780mm in length. Default is
#'   \code{FALSE}.
#' @param trap_dbase data frame object containing the LWG trapping data with
#'   identical data types as in tblLGDMasterCombineExport; required fields
#'   include MasterID, LGDNumPIT, CollectionDate, SRR, LGDSpecies, LGDRear,
#'   LGDLifeStage, LGDMarkAD, LGDValid, LGDFLmm, PTAgisSxCGRAObs.
#' @param useDARTrate should the DART query for the trap rate be used? Default
#'   is \code{FALSE}, which implies the trap rate is estimated by PIT tags.
#' @param trap_rate_cv constant coefficient of variation (CV) that should be
#'   applied to estimates of trap rate queried by DART. Default value is
#'   \code{0}.
#' @param trap_rate_dist distributional form for trap rate prior. \code{beta}
#'   returns alpha and beta parameters for beta distribution. \code{logit}
#'   returns mean and standard deviation in logit space.
#'
#' @import lubridate dplyr boot
#' @export
#' @return NULL
#' @examples compileGRAdata(2012)

compileGRAdata = function(yr,
                          spp = c('Chinook', 'Steelhead', 'Coho'),
                          dam = c('LWG', 'WFF', 'BON', 'TDA', 'JDA', 'MCN', 'IHR', 'LMN', 'LGS', 'PRO', 'ROZ', 'PRD', 'WAN', 'RIS', 'TUM', 'RRH', 'WEL', 'ZOS'),
                          # dam = c('LWG'),
                          start_date = NULL,
                          end_date = NULL,
                          incl_jacks = NULL,
                          sthd_type = c('all', 'unclipped'),
                          damPIT = c('GRA', 'PRA'),
                          strata_beg = 'Mon',
                          last_strata_min = 3,
                          sthd_B_run = FALSE,
                          trap_dbase = NULL,
                          incl_trapRate = T,
                          useDARTrate = F,
                          trap_rate_cv = 0,
                          trap_rate_dist = c('beta', 'logit')) {

  # need a start date
  stopifnot(!is.null(start_date))

  # if not provided, set a few defaults
  spp = match.arg(spp)
  dam = match.arg(dam)
  damPIT = match.arg(damPIT)
  # if(dam != 'LWG') {
  #   stop('Currently only works for Lower Granite (code LWG)')
  # }

  sthd_type = match.arg(sthd_type)
  trap_rate_dist = match.arg(trap_rate_dist)

  if(is.null(trap_dbase)) {
    stop('Trapping data is not supplied.')
  }

  if(is.null(incl_jacks)) {
    incl_jacks = ifelse(spp %in% c('Chinook', 'Coho'), T, F)
  }

  # currently pit tag query only works for Lower Granite and Priest Rapids
  try( if(!damPIT %in% c('GRA', 'PRA')) stop('PIT tag queries currently only work for Lower Granite (code GRA) and Priest Rapids (PRA)') )

  cat(paste0('Compiling data for ', spp, ' spawn year ', lubridate::year(lubridate::ymd(end_date)),'\n'))

  #---------------------------------------------
  # query window counts
  cat('Querying window counts\n')
  win_cnts = getWindowCounts(dam = dam,
                             spp = spp,
                             start_date = start_date,
                             end_date = end_date,
                             incl_jacks = incl_jacks,
                             sthd_type = sthd_type)

  #--------------------------------------------------------
  # query PIT tag data from previously tagged fish
  cat('Querying night passage & reascension PIT tags\n')
  pit_df = queryPITtagData(damPIT = damPIT,
                           spp = spp,
                           start_date = start_date,
                           end_date = end_date)

  #--------------------------------------------------------
  # determine weekly strata
  cat('Dividing into strata\n')
  week_strata = weeklyStrata(start_date = start_date,
                             end_date = end_date,
                             strata_beg = strata_beg,
                             last_strata_min = last_strata_min)

  # read in data for Chinook, steelhead, and coho
  cat('Getting LGR trap data\n')
  trap_yr = trap_dbase %>%
    rename(Tag.ID = LGDNumPIT) %>%
    mutate(Date = lubridate::floor_date(CollectionDate, unit = 'day'),
           SppCode = as.numeric(substr(SRR, 1, 1)),
           Tag.ID = as.character(Tag.ID),
           Tag.ID = ifelse(nchar(Tag.ID) < 3, NA, Tag.ID),
           Species = ifelse(SppCode == 1, 'Chinook', ifelse(SppCode == 3, 'Steelhead', ifelse(SppCode == 2, 'Coho', NA)))) %>%
    filter(Species %in% c('Chinook', 'Steelhead', 'Coho'),   # drop data from other species, and other runs of Chinook
           Date >= lubridate::ymd(lubridate::int_start(week_strata[1])), # filter out records outside dates of interest
           Date < lubridate::ymd(lubridate::int_end(week_strata[length(week_strata)]) + lubridate::dseconds(1)),
           LGDLifeStage == 'RF', # filter out juveniles, keep only adults
           (PTAgisSxCGRAObs != 'Yes' | is.na(PTAgisSxCGRAObs) ) ) # drop sort-by-code fish

  # summarise by date for particular species
  trap_df = summariseLGRtrapDaily(trap_df = trap_yr,
                                  spp = spp,
                                  sthd_B_run = sthd_B_run)

  # add week number to trap data
  trap_yr$week_num = NA
  for(i in 1:length(week_strata)) {
    trap_yr$week_num[with(trap_yr, which(Date %within% week_strata[i]))] = i
  }

  #--------------------------------------------------------
  if(incl_trapRate) {
    # Query trap rate from DART
    cat('Estimating trap rate\n')

    # estimate trap rate from PIT tags
    if(!useDARTrate) {
      trap_rate = tagTrapRate(trap_dataframe = trap_yr,
                              week_strata = week_strata) %>%
        # mutate(trap_open = ifelse(n_trap > 0, T, F)) %>%
        left_join(tibble(Start_Date = lubridate::int_start(week_strata),
                         week_num = 1:length(week_strata)), by = 'week_num') %>%
        mutate(Start_Date = lubridate::ymd(Start_Date)) %>%
        rename(n_trap_tags = n_trap,
               n_poss_tags = n_tot, # include the tag counts going into trap rate calc.
               trap_rate = rate,
               trap_rate_se = rate_se) %>%
        mutate(trap_open = if_else(n_trap_tags > 0, T, F)) %>%
        select(Start_Date,
               week_num,
               trap_open,
               everything())
    }

    # to use DART trap rate instead
    # impose constant CV on trap rate estimates
    if(useDARTrate) {
      trap_rate = queryTrapRate(week_strata,
                                # spp = spp,
                                return_weekly = T) %>%
        mutate(trap_rate = ActualRateInclusiveTime,
               # add some error
               trap_rate_se = trap_rate * trap_rate_cv) %>%
        mutate(trap_open = if_else(trap_rate > 0, T, F)) %>%
        select(Start_Date,
               week_num,
               trap_open,
               everything())
    }

    trap_rate = trap_rate %>%
      left_join(trap_yr %>%
                  group_by(week_num) %>%
                  summarise(trap_fish = n()),
                by = 'week_num') %>%
      mutate(trap_open = if_else(!trap_open & trap_fish > 0,
                                 T, trap_open),
             trap_open = if_else(is.na(trap_open), F, trap_open)) %>%
      select(-trap_fish)

  }

  # if(trap_rate_dist == 'beta') {
  #   trap_rate = trap_rate %>%
  #     # set up parameters describing trap rate as a beta distribution
  #     mutate(trap_alpha = ((1 - trap_rate) / trap_rate_se^2 - 1 / trap_rate) * trap_rate^2,
  #            trap_alpha = ifelse(trap_alpha < 0, 0.01, trap_alpha),
  #            trap_beta = trap_alpha * (1 / trap_rate - 1),
  #            trap_alpha = ifelse(trap_open, trap_alpha, 1e-12),
  #            trap_beta = ifelse(trap_open, trap_beta, 1)) %>%
  #     select(Start_Date, week_num, n_trap_tags, n_poss_tags, matches('^trap')) %>%  # include the tag observations
  #     distinct()
  # }
  #
  # if(trap_rate_dist == 'logit') {
  #   trap_rate = trap_rate %>%
  #     # set up parameters describing trap rate as a logit distribution
  #     mutate(trap_mu = ifelse(trap_open, boot::logit(trap_rate), 1e-12),
  #            trap_sd = ifelse(trap_open, (1 / n_trap_tags) + (1 / (n_poss_tags - n_trap_tags)), 0)) %>%
  #     # trap_sd = ifelse(trap_open, boot::logit(trap_rate_se), 0)) %>%
  #     select(Start_Date, week_num, matches('^trap')) %>%
  #     distinct()
  # }

  #--------------------------------------------------------
  # comnbine window counts, PIT tag data and trap summary on daily time-step
  cat('Combining daily data\n')
  dam_daily = win_cnts %>%
    select(-Year) %>%
    full_join(summarisePITdataDaily(pit_df) %>%
                select(-SpawnYear),
              by = c('Species', 'Date')) %>%
    mutate_at(vars(tot_tags:reascent_tags_H),
              list(~ifelse(is.na(.), 0, .))) %>%
    left_join(trap_df,
              by = 'Date')

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
    group_by(week_num) %>%
    summarise(Species = unique(Species),
              Start_Date = min(Date)) %>%
    ungroup() %>%
    left_join(dam_daily %>%
                group_by(week_num) %>%
                summarise_at(vars(win_cnt:n_invalid),
                             list(sum),
                             na.rm = T) %>%
                ungroup() %>%
                mutate_at(vars(win_cnt:n_invalid),
                          list(~ifelse(is.na(.), 0, .))), by = 'week_num') %>%
    mutate(window_open = ifelse(win_cnt > 0, T, F)) %>%
    select(Species, Start_Date, week_num, everything())

  if(incl_trapRate) {
    dam_weekly = addTrapRate(dam_weekly,
                             trap_rate,
                             trap_rate_dist)
  }

  return(list('weekStrata' = week_strata,
              'trapData' = trap_yr,
              'dailyData' = dam_daily,
              'weeklyData' = dam_weekly))

}

