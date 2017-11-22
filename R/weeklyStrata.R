#' @title Generate weekly strata
#'
#' @description Based on spawn year and species
#'
#' @author Kevin See
#'
#' @param spawn_yr spawn year to divide into weekly strata
#' @param spp choices are either \code{Chinook} or \code{Steelhead}
#' @param start_day date (\code{month / day}) when strata should start
#' @param end_day date (\code{month / day}) when strata should end
#' @param strata_beg 3 letter code for the day of the week each weekly strata should begin on.
#' @param last_strata_min minimum length (in days) for the final strata
#'
#' @import lubridate dplyr
#' @export
#' @return NULL
#' @examples weeklyStrata(2010:2012)

weeklyStrata = function(spawn_yr,
                        spp = c('Chinook', 'Steelhead'),
                        start_day = NULL,
                        end_day = NULL,
                        strata_beg = NULL,
                        last_strata_min = 3) {

  # need a year
  stopifnot(!is.null(spawn_yr))

  # set up default start and end days
  if(is.null(start_day)) start_day = ifelse(spp == 'Chinook', '0301',
                                            ifelse(spp == 'Steelhead', '0701', NA))
  if(is.null(end_day)) end_day = ifelse(spp == 'Chinook', '0817',
                                        ifelse(spp == 'Steelhead', '0630', NA))

  # first day to be include?
  first_date = ifelse(spp == 'Steelhead',
                      as.character(lubridate::ymd(paste(spawn_yr - 1, start_day))),
                      as.character(lubridate::ymd(paste(spawn_yr, start_day)))) %>%
    ymd()

  # last day to be included?
  last_date = lubridate::ymd(paste(spawn_yr, end_day))

  # how many weeks total?
  n_weeks = difftime(last_date, first_date,
                     units = 'weeks') %>%
    as.numeric() %>%
    ceiling()

  # to set up strata to begin on certain day of the week
  if(is.null(strata_beg)) strata_start = first_date

  if(!is.null(strata_beg)) {
    strata_start = dplyr::tibble(date = first_date + lubridate::days(0:as.integer(difftime(last_date, first_date, units = 'days')))) %>%
      dplyr::mutate(day = lubridate::wday(date, label = T)) %>%
      dplyr::filter(day == strata_beg) %>%
      dplyr::slice(1) %>%
      dplyr::select(date) %>%
      as.matrix() %>%
      as.character() %>%
      lubridate::ymd()
  }


  # put together dataframe of start and end times for each strata
  strata_df = dplyr::tibble(start_date = first_date,
                            end_date = strata_start - lubridate::dseconds(1)) %>%
    dplyr::bind_rows(dplyr::tibble(start_date = strata_start + lubridate::weeks(0:n_weeks)) %>%
                       dplyr::mutate(end_date = start_date + lubridate::days(7) - lubridate::dseconds(1))) %>%
    dplyr::filter(start_date < end_date,
                  start_date < last_date)

  strata_df$end_date[nrow(strata_df)] = last_date + lubridate::days(1) - lubridate::dseconds(1)

  if(as.numeric(as.duration(lubridate::interval(strata_df$start_date[nrow(strata_df)], strata_df$end_date[nrow(strata_df)])), 'days') < last_strata_min) {
    strata_df = strata_df %>%
      dplyr::slice(-nrow(strata_df))
    strata_df$end_date[nrow(strata_df)] = last_date + lubridate::days(1) - lubridate::dseconds(1)
  }

  if(as.numeric(as.duration(interval(strata_df$start_date[1], strata_df$end_date[1])), 'days') < last_strata_min) {
    strata_df = strata_df %>%
      dplyr::slice(-1)

    strata_df$start_date[1] = first_date
  }

  week_strata = lubridate::interval(strata_df$start_date, strata_df$end_date)

  return(week_strata)
}
