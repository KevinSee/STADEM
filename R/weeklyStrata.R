#' @title Generate weekly strata
#'
#' @description Based on year range, and start dates
#'
#' @author Kevin See
#'
#' @param years which years should go into the weekly strata
#' @param start_day day to start strata, in the format \code{month} \code{day}
#' @param end_day day to end strata, if needed
#'
#' @import lubridate dplyr
#' @export
#' @return NULL
#' @examples weeklyStrata(2010:2012)

weeklyStrata = function(years,
                        start_day = c('0701', '0301'),
                        end_day = NULL) {

  start_day = match.arg(start_day)

  for(yr in years) {
    if(yr == years[1]) strata_dates = data.frame(start_date = ymd(paste0(yr, start_day)) + weeks(0:51)) %>%
        mutate(end_date = start_date + days(7) - dseconds(1))
    if(yr > years[1]) strata_dates = strata_dates %>%
        bind_rows(data.frame(start_date = ymd(paste0(yr, start_day)) + weeks(0:51)) %>%
                    mutate(end_date = start_date + days(7) - dseconds(1)))
  }

  strata_df = strata_dates %>%
    filter(!(month(end_date) == 6 & mday(end_date) > 23)) %>%
    bind_rows(strata_dates %>%
                filter(month(end_date) == 6,
                       mday(end_date) > 23) %>%
                mutate(end_date = ymd(paste0(year(start_date), '0701')) - dseconds(1))) %>%
    arrange(start_date)

  if(!is.null(end_day)) {
    strata_df = strata_df %>%
      filter(start_date < ymd(paste0(year(start_date), end_day))) %>%
      mutate(end_date = ifelse(end_date > ymd(paste0(year(end_date), end_day)), ymd(paste0(year(end_date), end_day)), end_date) + origin)
  }

  week_strata = interval(strata_df$start_date, strata_df$end_date)

  return(week_strata)
}
