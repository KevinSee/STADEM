#' @title Compile weekly data for Lower Granite dam
#'
#' @description Combine window counts and PIT tag data on weekly time-steps
#'
#' @author Kevin See
#'
#' @param wind_data data.frame containing window count data to summarise, queried using \code{queryWindCnts}, and potentially combined across calendar years
#' @param pit_all data.frame containing data about individual PIT tags detected at Lower Granite Dam, produced using \code{queryPITtagData}
#' @param trap_df data.frame as created by \code{readLGRtrapDB}.
#' @param incl_clip_sthd should clipped steelhead in the trap be included in this summary? Should match with the window counts. Default is \code{FALSE}.
#' @param incl_chnk_jacks should window counts of Chinook jacks be included with adult counts? Default is \code{TRUE}
#'
#' @import lubridate dplyr
#' @return NULL

summariseLGRweekly = function(wind_data = NULL,
                              pit_all = NULL,
                              trap_df = NULL,
                              incl_clip_sthd = FALSE,
                              sthd_B_run = FALSE,
                              incl_chnk_jacks = TRUE) {

  stopifnot(!is.null(wind_data),
            !is.null(pit_all),
            !is.null(trap_df))

  yr_range = min(unique(year(wind_data$Date))):max(unique(year(wind_data$Date)))

  # determine weekly strata
  week_strata = weeklyStrata(yr_range)

  # For trap database, summarise by spawnyear, species and day
  lgr_trap_daily = summariseLGRtrapDaily(trap_df,
                                         incl_clip_sthd,
                                         sthd_B_run) %>%
    mutate(Date = as.Date(Date))

  # For PIT tag data, summarise by spawnyear, species and day
  pit_daily = summarisePITdataDaily(pit_all)

  # create vector determining if window is open
  wind_open = wind_data %>%
    select(-(Year:Date)) %>%
    mutate(open = ifelse(rowSums(is.na(.)) < ncol(.), T, F)) %>%
    select(open) %>%
    as.matrix() %>%
    as.vector()

  # for window counts, split by species
  wind_data = wind_data %>%
    mutate(window_open = wind_open) %>%
    mutate_at(vars(-Year, -Date, -window_open),
              list(~ifelse(is.na(.), 0, .)))

  if(incl_chnk_jacks) wind_data = wind_data %>%
    mutate(Chinook = Chinook + Jack_Chinook) %>%
    select(-Jack_Chinook)

  if(!incl_clip_sthd) wind_data = wind_data %>%
    select(-Steelhead) %>%
    rename(Steelhead = Wild_Steelhead)

  wind_long = wind_data %>%
    tidyr::gather(Species, win_cnt, -Year, -Date, -window_open) %>%
    filter(Species %in% c('Chinook', 'Steelhead')) %>%
    mutate(SpawnYear = ifelse(Species == 'Chinook', lubridate::year(Date),
                              ifelse(Species == 'Steelhead' & Date >= lubridate::ymd(paste0(lubridate::year(Date), '0701')), lubridate::year(Date) + 1, lubridate::year(Date)))) %>%
    select(Species, SpawnYear, Date, window_open, win_cnt)


  # combine daily data
  lgr_daily = wind_long %>%
    left_join(lgr_trap_daily) %>%
    left_join(pit_daily) %>%
    filter(SpawnYear >= min(yr_range),
           SpawnYear <= max(yr_range),
           !(Species == 'Chinook' & Date < lubridate::ymd(paste(lubridate::year(Date), '0301'))),
           !(Species == 'Chinook' & Date > lubridate::ymd(paste(lubridate::year(Date), '0817'))),
           Date >= min(lubridate::int_start(week_strata)),
           Date <= max(lubridate::int_end(week_strata))) %>%
    mutate_at(vars(win_cnt, trap_fish:reascent_tags_H),
              list(as.integer)) %>%
    # assign week numbers to each day
    mutate(week_num_org = NA)

  for(i in 1:length(week_strata)) {
    lgr_daily$week_num_org[with(lgr_daily, which(Date %within% week_strata[i]))] = i
  }

  # summarise daily data by week
  lgr_weekly = lgr_daily %>%
    filter(!is.na(week_num_org),
           SpawnYear >= min(yr_range),
           SpawnYear <= max(yr_range),
           # !(Species == 'Chinook' & SpawnYear == min(yr_range)),
           !(Species == 'Chinook' & Date < lubridate::ymd(paste(lubridate::year(Date), '0301'))),
           !(Species == 'Chinook' & Date > lubridate::ymd(paste(lubridate::year(Date), '0817')))) %>%
    group_by(Species, SpawnYear, week_num_org) %>%
    summarise_at(vars(win_cnt:reascent_tags_H),
                 list(sum),
                 na.rm = T) %>%
    ungroup() %>%
    left_join(lgr_daily %>%
                group_by(Species, SpawnYear, week_num_org) %>%
                summarise(window_open = ifelse(sum(window_open) > 0, T, F))) %>%
    mutate(Start_Date = int_start(week_strata)[week_num_org]) %>%
    group_by(Species, SpawnYear) %>%
    mutate(week_num = week_num_org - min(week_num_org) + 1) %>%
    ungroup() %>%
    select(Species:week_num_org, week_num, Start_Date, window_open, everything())

  #
  # # assign week numbers to each day
  # pit_daily = pit_daily %>%
  #   mutate(week_num_org = NA)
  # wind_long = wind_long %>%
  #   mutate(week_num_org = NA)
  # lgr_trap_daily = lgr_trap_daily %>%
  #   mutate(week_num_org = NA)
  #
  # for(i in 1:length(week_strata)) {
  #   pit_daily$week_num_org[with(pit_daily, which(Date %within% week_strata[i]))] = i
  #   wind_long$week_num_org[with(wind_long, which(Date %within% week_strata[i]))] = i
  #   lgr_trap_daily$week_num_org[with(lgr_trap_daily, which(Date %within% week_strata[i]))] = i
  # }
  #
  #
  # # summarise PIT tag data weekly
  # pit_weekly = pit_daily %>%
  #   filter(!is.na(week_num_org),
  #                 SpawnYear >= min(yr_range),
  #                 SpawnYear <= max(yr_range),
  #                 # !(Species == 'Chinook' & SpawnYear == min(yr_range)),
  #                 !(Species == 'Chinook' & Date < ymd(paste(year(Date), '0301'))),
  #                 !(Species == 'Chinook' & Date > ymd(paste(year(Date), '0817')))) %>%
  #   group_by(Species, SpawnYear, week_num_org) %>%
  #   summarise_at(vars(tot_tags:reascent_tags_H), list(sum), na.rm = T) %>%
  #   ungroup()
  #
  # # summarise trap data weekly
  # lgr_trap_weekly = lgr_trap_daily %>%
  #   filter(!is.na(week_num_org),
  #                 SpawnYear >= min(yr_range),
  #                 SpawnYear <= max(yr_range),
  #                 # !(Species == 'Chinook' & SpawnYear == min(yr_range)),
  #                 !(Species == 'Chinook' & Date < ymd(paste(year(Date), '0301'))),
  #                 !(Species == 'Chinook' & Date > ymd(paste(year(Date), '0817')))) %>%
  #   group_by(Species, SpawnYear, week_num_org) %>%
  #   summarise_at(vars(trap_fish:n_invalid), list(sum), na.rm = T) %>%
  #   ungroup()
  #
  # # summarise window counts weekly
  # wind_weekly = wind_long %>%
  #   filter(!is.na(week_num_org),
  #                 SpawnYear >= min(yr_range),
  #                 SpawnYear <= max(yr_range),
  #                 # !(Species == 'Chinook' & SpawnYear == min(yr_range)),
  #                 !(Species == 'Chinook' & Date < ymd(paste(year(Date), '0301'))),
  #                 !(Species == 'Chinook' & Date > ymd(paste(year(Date), '0817')))) %>%
  #   group_by(Species, SpawnYear, week_num_org) %>%
  #   summarise(win_cnt = sum(win_cnt, na.rm = T),
  #             window_open = ifelse(sum(window_open > 0), T, F)) %>%
  #   ungroup()
  #
  # # combine weekly summaries
  # lgr_weekly = wind_weekly %>%
  #   full_join(lgr_trap_weekly) %>%
  #   full_join(pit_weekly) %>%
  #   mutate(Start_Date = int_start(week_strata)[week_num_org]) %>%
  #   group_by(Species, SpawnYear) %>%
  #   mutate(week_num = week_num_org - min(week_num_org) + 1) %>%
  #   ungroup() %>%
  #   select(Species:week_num_org, week_num, Start_Date, window_open, everything()) %>%
  #   mutate_at(vars(win_cnt, trap_fish:reascent_tags_H), list(ifelse(is.na(.), 0, .))) %>%
  #   mutate_at(vars(win_cnt, trap_fish:reascent_tags_H), list(as.integer))



  # do daily and weekly data have same number of fish at window and trap in total?
  all_fish_counted = identical(lgr_weekly %>%
                                 group_by(Species, SpawnYear) %>%
                                 summarise(tot_cnts = sum(win_cnt),
                                           tot_trap = sum(trap_fish, na.rm = T)),
                               lgr_daily %>%
                                 group_by(Species, SpawnYear) %>%
                                 summarise(tot_cnts = sum(win_cnt, na.rm = T),
                                           tot_trap = sum(trap_fish, na.rm=T)))

  stopifnot(all_fish_counted)

  rm(trap_df)

  return(list('week_strata' = week_strata,
              'lgr_weekly' = lgr_weekly))
}
