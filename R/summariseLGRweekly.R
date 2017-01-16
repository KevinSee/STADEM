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
#' @export
#' @return NULL

summariseLGRweekly = function(wind_data = NULL,
                              pit_all = NULL,
                              trap_df = NULL,
                              incl_clip_sthd = FALSE,
                              sthd_B_run = FALSE,
                              incl_chnk_jacks = TRUE) {

  stopifnot(!is.null(wind_data))
  stopifnot(!is.null(pit_all))

  yr_range = min(unique(year(wind_data$Date))):max(unique(year(wind_data$Date)))

  # determine weekly strata
  week_strata = weeklyStrata(yr_range)

  if(is.null(trap_df)) {
    load('R/sysdata.rda')
    trap_df = lgr_trap
    rm(lgr_trap)
    # trap_df$Date = ymd(trap_df$Date)
  }

  # For trap database, summarise by spawnyear, species and day
  lgr_trap_daily = summariseLGRtrapDaily(trap_df,
                                         incl_clip_sthd,
                                         sthd_B_run)
  lgr_trap_daily$Date = ymd(lgr_trap_daily$Date)

  # For PIT tag data, summarise by spawnyear, species and day
  pit_daily = summarisePITdataDaily(pit_all)

  # for window counts, split by species
  wind_open = ifelse(wind_data %>%
                       select(-(Year:Date)) %>%
                       is.na() %>%
                       rowSums() > 0, F, T)

  wind_data = wind_data %>%
    dplyr::mutate(window_open = wind_open) %>%
    dplyr::mutate_each(funs(ifelse(is.na(.), 0, .)), -Year, -Date, -window_open)

  if(incl_chnk_jacks) wind_data = wind_data %>%
    dplyr::mutate(Chinook = Chinook + Jack_Chinook) %>%
    dplyr::select(-Jack_Chinook)

  if(!incl_clip_sthd) wind_data = wind_data %>%
    dplyr::select(-Steelhead) %>%
    dplyr::rename(Steelhead = Wild_Steelhead)

  wind_long = wind_data %>%
    dplyr::select(Year, Date, window_open, Chinook, Steelhead) %>%
    gather(Species, win_cnt, -Year, -Date, -window_open)

  # assign week numbers to each day
  pit_daily$week_num_org = wind_long$week_num_org = lgr_trap_daily$week_num_org = NA
  for(i in 1:length(week_strata)) {
    pit_daily$week_num_org[with(pit_daily, which(Date %within% week_strata[i]))] = i
    wind_long$week_num_org[with(wind_long, which(Date %within% week_strata[i]))] = i
    lgr_trap_daily$week_num_org[with(lgr_trap_daily, which(Date %within% week_strata[i]))] = i
  }

  # summarise PIT tag data weekly
  pit_weekly = pit_daily %>%
    dplyr::filter(!is.na(week_num_org)) %>%
    group_by(Species, SpawnYear, week_num_org) %>%
    summarise_each(funs(sum(., na.rm = T)), tot_tags:reascent_tags_H) %>%
    ungroup()

  lgr_daily = wind_long %>%
    full_join(lgr_trap_daily) %>%
    left_join(pit_daily) %>%
    dplyr::mutate(SpawnYear = ifelse(Species == 'Chinook', year(Date),
                                     ifelse(Date >= ymd(paste0(year(Date), '0701')), year(Date) + 1, year(Date)))) %>%
    dplyr::filter(!is.na(week_num_org),
                  SpawnYear >= min(yr_range),
                  SpawnYear <= max(yr_range),
                  !(Species == 'Chinook' & SpawnYear == min(yr_range)),
                  !(Species == 'Chinook' & Date < ymd(paste(year(Date), '0301'))),
                  !(Species == 'Chinook' & Date > ymd(paste(year(Date), '0817'))))

  lgr_daily = lgr_daily %>%
    group_by(Species, SpawnYear) %>%
    summarise(min_week = min(week_num_org)) %>%
    left_join(lgr_daily) %>%
    group_by(Species, SpawnYear) %>%
    dplyr::mutate(week_num = week_num_org - min_week + 1) %>%
    dplyr::select(-min_week) %>%
    dplyr::select(Species, SpawnYear, Year, Date, week_num_org, week_num, everything()) %>%
    ungroup()

  # transform to weekly summaries
  lgr_weekly = lgr_daily %>%
    group_by(Species, SpawnYear, Year, week_num_org, week_num) %>%
    summarise_each(funs(sum(., na.rm=T)), -Date) %>%
    dplyr::mutate(window_open = ifelse(window_open > 0, T, F)) %>%
    left_join(lgr_daily %>%
                group_by(Species, SpawnYear, week_num_org, week_num) %>%
                summarise(Start_Date = min(Date))) %>%
    ungroup() %>%
    dplyr::select(Species, SpawnYear, Year, week_num_org, week_num, Start_Date, everything())

  # do daily and weekly data have same number of fish at window and trap in total?
  all_fish_counted = identical(group_by(lgr_weekly, Species, SpawnYear) %>%
                                 summarise(tot_cnts = sum(win_cnt),
                                           tot_trap = sum(trap_fish)),
                               group_by(lgr_daily, Species, SpawnYear) %>%
                                 summarise(tot_cnts = sum(win_cnt, na.rm = T),
                                           tot_trap = sum(trap_fish, na.rm=T)))

  stopifnot(all_fish_counted)

  rm(trap_df)

  return(list('week_strata' = week_strata,
              'lgr_weekly' = lgr_weekly))
}
