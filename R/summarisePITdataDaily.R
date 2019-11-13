#' @title Summarise PIT tag data
#'
#' @description Summarise night passage and re-ascension data from PIT tags at Lower Granite Dam on a daily time-step
#'
#' @author Kevin See
#'
#' @param pit_data data.frame containing PIT tag data to summarise, queried using \code{queryPITtagData}
#'
#' @import lubridate dplyr
#' @export
#' @return NULL
#' @examples summarisePITdataDaily(pit_all)

summarisePITdataDaily = function(pit_data) {
  # clean up a few columns
  pit_data = pit_data %>%
    mutate(Period = factor(Period,
                           levels = c('D', 'N')),
           ReAscent = if_else(!is.na(TagIDAscentCount) & TagIDAscentCount > 1,
                              T, F),
           SpawnYear = if_else(Species == 'Chinook',
                               Year,
                               if_else(Species == 'Steelhead' & Date >= ymd(paste0(Year, '0701')),
                                       Year + 1,
                                       Year)))

  # summarise rates by day
  lgr_night_reasc_daily = pit_data %>%
    select(Species, SpawnYear, Date, TagID, RearType, Period, ReAscent) %>%
    distinct() %>%
    arrange(Species, SpawnYear, TagID, Date) %>%
    group_by(Species, SpawnYear, Date) %>%
    summarise(tot_tags = n_distinct(TagID),
              day_tags = n_distinct(TagID[Period=='D']),
              reascent_tags = n_distinct(TagID[ReAscent]),
              tot_tags_W = n_distinct(TagID[RearType == 'W']),
              day_tags_W = n_distinct(TagID[Period=='D' & RearType == 'W']),
              reascent_tags_W = n_distinct(TagID[ReAscent & RearType == 'W']),
              tot_tags_H = n_distinct(TagID[RearType == 'H']),
              day_tags_H = n_distinct(TagID[Period=='D' & RearType == 'H']),
              reascent_tags_H = n_distinct(TagID[ReAscent & RearType == 'H'])) %>%
    ungroup()

  return(lgr_night_reasc_daily)
}
