#' @title DART reported trap rate
#'
#' @description Query DART for the reported trap rate at Lower Granite dam.
#'
#' @author Kevin See
#'
#' @param week_strata vector of intervals delimiting the weekly strata to summarize mark-recapture data over, as returned by \code{weeklyStrata()}.
#' @param spp species to query trap rate for. Possible choices are: \code{Chinook} or \code{Steelhead}
#' @param return_weekly Should the results be summarised on weekly basis? Default is \code{TRUE}.
#'
#' @source \url{http://www.cbr.washington.edu/dart}
#'
#' @import lubridate httr dplyr
#' @importFrom plyr ddply
#' @export
#' @return NULL

queryTrapRate = function(week_strata = NULL,
                         spp = c('Steelhead', 'Chinook'),
                         return_weekly = T) {

  # match up species code with species name
  spp_code_df = data.frame(Species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                           code = 1:4)

  # use steelhead as default
  spp = match.arg(spp)
  spp_code = spp_code_df$code[match(spp, spp_code_df$Species)]

  # # use only steelhead, since that covers an entire year
  # spp_code = 3

  # # assign user agent to the GitHub repo for this package
  # ua = httr::user_agent('https://github.com/KevinSee/STADEM')
  #
  # # compose url with query
  # url_req = 'http://www.cbr.washington.edu/dart/cs/php/lib/file_wrapper.php'
  #
  # trap_rate_dart = NULL
  # for(yr in sort(unique(lubridate::year(lubridate::int_start(week_strata))))) {
  #   web_req = httr::GET(url_req, ua,
  #                       query = list(type = 'csv',
  #                                    fname = paste0('pit_adult_valid_', yr, '_', spp_code, '.csv'),
  #                                    dname = 'inc'))
  #
  #   if(httr::content(web_req,
  #                    'text',
  #                    encoding = 'ISO-8859-1') == '') {
      # stop(paste('DART returned no trap rate data for', spp, 'in', lubridate::year(lubridate::int_start(week_strata)[1]), '\n'))
  #   }
  #
  #   # what encoding to use?
  #   # stringi::stri_enc_detect(httr::content(web_req, "raw"))
  #
  #   # parse the response
  #   parsed = httr::content(web_req,
  #                          'text',
  #                          encoding = 'ISO-8859-1') %>%
  #     readr::read_delim(delim = ',',
  #                       col_names = T) %>%
  #     mutate(Date = mdy(Date),
  #            DOY = as.integer(DOY)) %>%
  #     rename(n_Samples = `#Samples`,
  #            n_SbyC = `#SbyC`,
  #            n_Close = `#Close`)
  #
  #   if(is.null(trap_rate_dart)) trap_rate_dart = parsed
  #
  #   else trap_rate_dart = trap_rate_dart %>%
  #     bind_rows(parsed)
  #
  # }

  trap_rate_dart = NULL
  for(yr in sort(unique(lubridate::year(lubridate::int_start(week_strata))))) {
    parsed = try(read_csv(paste(url_req,
                            paste0('pit_adult_valid_', yr, '_', spp_code, '.csv'),
                            sep = "/"),
                      show_col_types = F))
    if(class(parsed)[1] == "try-error") {
      message(paste('DART returned no trap rate data for',
                    spp,
                    'in',
                    yr, '\n'))
      # stop()
      next
    }

    parsed <- parsed |>
      mutate(across(Date,
                    mdy),
             across(DOY,
                    as.integer)) |>
      rename(n_Samples = `#Samples`,
             n_SbyC = `#SbyC`,
             n_Close = `#Close`)

    if(is.null(trap_rate_dart)) trap_rate_dart = parsed

    else trap_rate_dart = trap_rate_dart %>%
        bind_rows(parsed)
  }

  # deal with fact that some days have two rows in data (different rates during different times of day)
  trap_rate_obs = trap_rate_dart %>%
    group_by(Date, Year, DOY) %>%
    summarise_at(vars(SampTime:n_Close, TotFishTrap:ALLPit),
                 list(sum),
                 na.rm = T) %>%
    mutate(SecondsInDay = 2 * (60*60*24)) %>% # multiplied by 2 because there are 2 flumes in trap
    left_join(trap_rate_dart %>%
                group_by(Date, Year, DOY) %>%
                summarise_at(vars(Rate:ActualRateInclusiveTime),
                             list(~weighted.mean(., na.rm = T, w = TotalTime)))) %>%
    mutate(trap_open = ifelse(TotalTime > 0, T, F),
           RateCalc = TotalTimeInclusive / SecondsInDay) %>%
    ungroup()

  if(!return_weekly) return(trap_rate_obs)

  # assign week number
  trap_rate_obs$week_num = NA
  for(i in 1:length(week_strata)) {
    trap_rate_obs$week_num[with(trap_rate_obs, which(Date %within% week_strata[i]))] = i
  }

  # summarise by week
  trap_rate_obs_wk = trap_rate_obs %>%
    filter(!is.na(week_num)) %>%
    group_by(Year, week_num) %>%
    summarise(Start_Date = min(Date)) %>%
    left_join(tibble(week_num = 1:length(week_strata),
                     SecondsInWeek = as.numeric(as.duration(week_strata), 'seconds'))) %>%
    left_join(trap_rate_obs %>%
                filter(!is.na(week_num)) %>%
                group_by(Year, week_num) %>%
                summarise_at(vars(SampTime:n_Close, TotFishTrap:ALLPit, trap_open),
                             list(sum),
                             na.rm = T)) %>%
    left_join(trap_rate_obs %>%
                filter(!is.na(week_num)) %>%
                group_by(Year, week_num) %>%
                summarise_at(vars(Rate:ActualRateInclusiveTime),
                             list(mean),
                             na.rm = T)) %>%
    mutate(CalcRate = TotalTimeInclusive / SecondsInWeek,
           trap_open = ifelse(trap_open > 0, T, F)) %>%
    select(Year, week_num, Start_Date, everything()) %>%
    ungroup()

  return(trap_rate_obs_wk)
}
