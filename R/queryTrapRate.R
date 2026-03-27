#' @title DART reported trap rate
#'
#' @description Query DART for the reported trap rate at Lower Granite dam.
#'
#' @author Kevin See
#'
#' @param week_strata vector of intervals delimiting the weekly strata to summarize mark-recapture data over, as returned by \code{weeklyStrata()}.
#' @param spp species to query trap rate for. Possible choices are: \code{Chinook}, \code{Steelhead}, or \code{Coho}.
#' @param return_weekly Should the results be summarised on weekly basis? Default is \code{TRUE}.
#'
#' @source \url{https://www.cbr.washington.edu/dart}
#'
#' @import lubridate dplyr vroom
#' @export
#' @return NULL

queryTrapRate = function(week_strata = NULL,
                         spp = c('Steelhead', 'Chinook', 'Coho'),
                         return_weekly = T) {

  # use steelhead as default
  spp = match.arg(spp)

  # match up species code with species name
  spp_code <- dplyr::case_when(spp == "Chinook" ~ 1,
                               spp == "Coho" ~ 2,
                               spp == "Steelhead" ~ 3,
                               spp == "Sockeye" ~ 4,
                               .default = NA_real_)
  if(is.na(spp_code)) {
    stop("Species name not part of this query")
  }

  # assign user agent to the GitHub repo for this package
  # ua = httr::user_agent('https://github.com/KevinSee/STADEM')

  url_req = 'https://www.cbr.washington.edu/dart/cs/data/GRAtrap'

  trap_rate_dart = NULL
  for(yr in sort(unique(lubridate::year(lubridate::int_start(week_strata))))) {
    parsed = vroom::vroom(paste(url_req,
                                paste0('pit_adult_valid_', yr, '_', spp_code, '.csv'),
                                sep = "/"),
                          show_col_types = F) |>
      try()
    if(inherits(parsed, "try-error")) {
      message(paste('DART returned no trap rate data for',
                    spp,
                    'in',
                    yr, '\n'))
      # stop()
      next
    }

    parsed <-
      parsed |>
      dplyr::mutate(
        dplyr::across(Date,
                      lubridate::mdy),
        dplyr::across(DOY,
                      as.integer)) |>
      dplyr::rename(n_Samples = `#Samples`,
                    n_SbyC = `#SbyC`,
                    n_Close = `#Close`)

    if(is.null(trap_rate_dart)) trap_rate_dart = parsed

    else trap_rate_dart = trap_rate_dart %>%
      bind_rows(parsed)
  }

  # deal with fact that some days have two rows in data (different rates during different times of day)
  trap_rate_obs = trap_rate_dart %>%
    dplyr::group_by(Date, Year, DOY) %>%
    dplyr::summarise_at(vars(SampTime:n_Close, TotFishTrap:ALLPit),
                        list(sum),
                        na.rm = T) %>%
    dplyr::mutate(SecondsInDay = 2 * (60*60*24)) %>% # multiplied by 2 because there are 2 flumes in trap
    dplyr::left_join(trap_rate_dart %>%
                       dplyr::group_by(Date, Year, DOY) %>%
                       dplyr::summarise_at(vars(Rate:ActualRateInclusiveTime),
                                           list(~weighted.mean(., na.rm = T, w = TotalTime)))) %>%
    dplyr::mutate(trap_open = ifelse(TotalTime > 0, T, F),
                  RateCalc = TotalTimeInclusive / SecondsInDay) %>%
    dplyr::ungroup()

  if(!return_weekly) return(trap_rate_obs)

  # assign week number
  trap_rate_obs$week_num = NA
  for(i in 1:length(week_strata)) {
    trap_rate_obs$week_num[with(trap_rate_obs, which(Date %within% week_strata[i]))] = i
  }

  # summarize by week
  trap_rate_obs_wk = trap_rate_obs %>%
    dplyr::filter(!is.na(week_num)) %>%
    dplyr::group_by(Year, week_num) %>%
    dplyr::summarise(Start_Date = min(Date)) %>%
    dplyr::left_join(dplyr::tibble(week_num = 1:length(week_strata),
                                   SecondsInWeek = as.numeric(lubridate::as.duration(week_strata), 'seconds'))) %>%
    dplyr::left_join(trap_rate_obs %>%
                       dplyr::filter(!is.na(week_num)) %>%
                       dplyr::group_by(Year, week_num) %>%
                       dplyr::summarise_at(vars(SampTime:n_Close, TotFishTrap:ALLPit, trap_open),
                                           list(sum),
                                           na.rm = T)) %>%
    dplyr::left_join(trap_rate_obs %>%
                       dplyr::filter(!is.na(week_num)) %>%
                       dplyr::group_by(Year, week_num) %>%
                       dplyr::summarise_at(vars(Rate:ActualRateInclusiveTime),
                                           list(mean),
                                           na.rm = T)) %>%
    dplyr::mutate(CalcRate = TotalTimeInclusive / SecondsInWeek,
                  trap_open = ifelse(trap_open > 0, T, F)) %>%
    dplyr::select(Year, week_num, Start_Date, everything()) %>%
    dplyr::ungroup()

  return(trap_rate_obs_wk)
}
