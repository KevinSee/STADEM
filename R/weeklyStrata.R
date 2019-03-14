#' @title Generate weekly strata
#'
#' @description Based on spawn year and species
#'
#' @author Kevin See
#'
#' @param start_date character vector of date (\code{YYYYMMDD}) when query should start
#' @param end_date character vector of date (\code{YYYYMMDD}) when query should end
#' @param strata_beg 3 letter code for the day of the week each weekly strata should begin on. Default value is \code{'Mon'}.
#' @param last_strata_min minimum length (in days) for the final strata. Default value is 3.
#'
#' @import lubridate dplyr
#' @export
#' @return NULL
#' @examples weeklyStrata(2010:2012)

weeklyStrata = function(start_date = NULL,
                        end_date = NULL,
                        strata_beg = 'Mon',
                        last_strata_min = 3) {

  # need a start date
  stopifnot(!is.null(start_date))

  # set default end date (1 year after start date)
  if(is.null(end_date)) end_date = format(lubridate::ymd(start_date) + years(1) - days(1), '%Y%m%d')

  # turn start / end date character vectors into actual date objects
  startDate = lubridate::ymd(start_date)
  endDate = lubridate::ymd(end_date)

  if(endDate < startDate) stop('start_date comes after end_date')

  # how many weeks total?
  n_weeks = difftime(endDate, startDate,
                     units = 'weeks') %>%
    as.numeric() %>%
    ceiling()

  # to set up strata to begin on certain day of the week
  if(is.null(strata_beg)) strata_start = startDate

  if(!is.null(strata_beg)) {
    strata_start = tibble(date = startDate + lubridate::days(0:as.integer(difftime(endDate, startDate, units = 'days')))) %>%
      mutate(day = lubridate::wday(date, label = T)) %>%
      filter(day == strata_beg) %>%
      slice(1) %>%
      select(date) %>%
      as.matrix() %>%
      as.character() %>%
      lubridate::ymd()
  }


  # put together dataframe of start and end times for each strata
  strata_df = tibble(start_date = startDate,
                     end_date = strata_start - lubridate::dseconds(1)) %>%
    bind_rows(tibble(start_date = strata_start + lubridate::weeks(0:n_weeks)) %>%
                mutate(end_date = start_date + lubridate::days(7) - lubridate::dseconds(1))) %>%
    filter(start_date < end_date,
           start_date < endDate)

  strata_df$end_date[nrow(strata_df)] = endDate + lubridate::days(1) - lubridate::dseconds(1)

  if(as.numeric(as.duration(lubridate::interval(strata_df$start_date[nrow(strata_df)], strata_df$end_date[nrow(strata_df)])), 'days') < last_strata_min) {
    strata_df = strata_df %>%
      slice(-nrow(strata_df))
    strata_df$end_date[nrow(strata_df)] = endDate + lubridate::days(1) - lubridate::dseconds(1)
  }

  if(as.numeric(as.duration(interval(strata_df$start_date[1], strata_df$end_date[1])), 'days') < last_strata_min) {
    strata_df = strata_df %>%
      slice(-1)

    strata_df$start_date[1] = startDate
  }

  week_strata = lubridate::interval(strata_df$start_date, strata_df$end_date)

  return(week_strata)
}
