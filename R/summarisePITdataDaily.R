#' @title Summarise PIT tag data
#'
#' @description Summarise night passage and re-ascension data from PIT tags at Lower Granite Dam on a daily time-step
#'
#' @author Kevin See
#'
#' @param pit_data data.frame containing PIT tag data to summarise, queried using \code{queryPITtagData}
#' @param spring_summer_chnk should the PIT tag data be filtered to exclude fall and winter run Chinook? Default is \code{FALSE}
#'
#' @import lubridate dplyr
#' @export
#' @return NULL
#' @examples summarisePITdataDaily(pit_all)

summarisePITdataDaily = function(pit_data,
                                 spring_summer_chnk = F) {
  # clean up a few columns
  pit_data = pit_data %>%
    mutate(Period = factor(Period, levels = c('D', 'N')),
           ReAscent = ifelse(TagIDAscentCount > 1, T, F),
           SpawnYear = ifelse(Species == 'Chinook', Year,
                              ifelse(Species == 'Steelhead' & Date >= ymd(paste0(Year, '0701')), Year + 1, Year)))

  # if necessary, delete Chinook that aren't "Spring/Summer"
  if(spring_summer_chnk) pit_data  = pit_data %>%
      filter(!(Species == 'Chinook' & (Date < ymd(paste0(year(Date), '0301')) | Date > ymd(paste0(year(Date), '0817')))))

  # summarise rates by day
  lgr_night_reasc_daily = pit_data %>%
    select(Species, SpawnYear, Date, TagID, RearType, Period, ReAscent) %>%
    distinct() %>%
    arrange(Species, SpawnYear, TagID, Date) %>%
    group_by(Species, SpawnYear, Date) %>%
    summarise(tot_tags = n_distinct(TagID),
              day_tags = length(unique(TagID[Period=='D'])),
              reascent_tags = length(unique(TagID[ReAscent])),
              tot_tags_W = length(unique(TagID[RearType == 'W'])),
              day_tags_W = length(unique(TagID[Period=='D' & RearType == 'W'])),
              reascent_tags_W = length(unique(TagID[ReAscent & RearType == 'W'])),
              tot_tags_H = length(unique(TagID[RearType == 'H'])),
              day_tags_H = length(unique(TagID[Period=='D' & RearType == 'H'])),
              reascent_tags_H = length(unique(TagID[ReAscent & RearType == 'H']))) %>%
    ungroup()

  return(lgr_night_reasc_daily)
}
