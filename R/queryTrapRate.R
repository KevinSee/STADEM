#' @title DART reported trap rate
#'
#' @description Estimate the trap rate at Lower Granite by using a mark-recapture model. The trap is the marking event, and detections at the weir at the top of the fish ladder is the 2nd capture event.
#'
#' @author Kevin See
#'
#' @param week_strata vector of intervals delimiting the weekly strata to summarize mark-recapture data over. Returned as one part of the \code{summariseLGRweekly} function.
#' @param return_weekly Should the results be summarised on weekly basis? Default is \code{TRUE}.
#'
#' @source \url{http://www.cbr.washington.edu/dart}
#'
#' @import lubridate httr dplyr
#' @importFrom plyr ddply
#' @export
#' @return NULL

queryTrapRate = function(week_strata = NULL,
                         return_weekly = T) {

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/KevinSee/damEscapement')

  # compose url with query
  url_req = 'http://www.cbr.washington.edu/dart/cs/php/lib/file_wrapper.php'

  # only use steelhead, since that covers an entire year
  spp_code = 3

  trap_rate_dart = NULL
  for(yr in sort(unique(year(int_start(week_strata))))) {
    web_req = GET(url_req, ua,
                  query = list(type = 'csv',
                               fname = paste0('pit_adult_valid_', yr, '_', spp_code, '.csv'),
                               dname = 'inc'))

    # what encoding to use?
    # stringi::stri_enc_detect(content(web_req, "raw"))

    # parse the response
    parsed = httr::content(web_req,
                           'parsed',
                           encoding = 'ISO-8859-1') %>%
      mutate(Year = yr,
             Date = mdy(Date),
             DOY = as.integer(DOY)) %>%
      dplyr::rename(n_Samples = `#Samples`,
                    n_SbyC = `#SbyC`,
                    n_Close = `#Close`)

    if(is.null(trap_rate_dart)) trap_rate_dart = parsed

    else trap_rate_dart = trap_rate_dart %>%
      bind_rows(parsed)

  }

  # trap_rate_dart = plyr::ddply(data.frame(Year = sort(unique(year(int_start(week_strata))))),
  #                              .(Year),
  #                              function(x) {
  #                                # send query to DART
  #                                web_req = GET(url_req, ua,
  #                                              query = list(type = 'csv',
  #                                                           fname = paste0('pit_adult_valid_', x$Year, '_', spp_code, '.csv'),
  #                                                           dname = 'inc'))
  #
  #                                # what encoding to use?
  #                                # stringi::stri_enc_detect(content(web_req, "raw"))
  #
  #                                # parse the response
  #                                parsed = httr::content(web_req,
  #                                                       'parsed',
  #                                                       encoding = 'ISO-8859-1') %>%
  #                                  mutate(Date = mdy(Date),
  #                                         DOY = as.integer(DOY)) %>%
  #                                  dplyr::rename(n_Samples = `#Samples`,
  #                                                n_SbyC = `#SbyC`,
  #                                                n_Close = `#Close`)
  #
  #                                return(parsed)
  #                              },
  #                              .progress = 'none') %>%
  #   tbl_df()

  # deal with fact that some days have two rows in data (different rates during different times of day)
  trap_rate_obs = trap_rate_dart %>%
    group_by(Date, Year, DOY) %>%
    summarise_each(funs(sum(., na.rm = T)), SampTime:n_Close) %>%
    mutate(SecondsInDay = 2 * (60*60*24)) %>% # multiplied by 2 because there are 2 flumes in trap
    left_join(trap_rate_dart %>%
                group_by(Date, Year, DOY) %>%
                summarise_each(funs(weighted.mean(., na.rm = T, w = TotalTime)), Rate:ActualRateInclusiveTime)) %>%
    mutate(trap_open = ifelse(TotalTime > 0, T, F),
           RateCalc = TotalTimeInclusive / SecondsInDay,
           week_num_org = NA) %>%
    ungroup()

  if(!return_weekly) return(trap_rate_obs)

  # assign week number
  for(i in 1:length(week_strata)) {
    trap_rate_obs$week_num_org[with(trap_rate_obs, which(Date %within% week_strata[i]))] = i
  }
  # summarise by week
  trap_rate_obs_wk = trap_rate_obs %>%
    group_by(Year, week_num_org) %>%
    summarise(tot_time = sum(TotalTimeInclusive),
              sec_in_week = sum(SecondsInDay),
              mean_goal = mean(Rate, na.rm = T),
              mean_obs = mean(ActualRateInclusiveTime, na.rm = T),
              mean_rate = mean(RateCalc),
              calc_rate = tot_time / sec_in_week,
              trap_open = ifelse(sum(trap_open) > 0, T, F)) %>%
    ungroup() %>%
    left_join(data.frame(week_num_org = 1:length(week_strata),
                         Start_Date = int_start(week_strata))) %>%
    select(Year, week_num_org, Start_Date, everything())

  return(trap_rate_obs_wk)
}
