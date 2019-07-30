#' @title PIT tag based trap rate
#'
#' @description Estimate the trap rate at Lower Granite by using the proportion of previously PIT tagged fish observed pass a location that were found in the fish trap at that location.
#'
#' @author Kevin See
#'
#' @param trap_dataframe dataframe containing data from the fish trap, including columns named \code{Tag.ID}, \code{CollectionDate} and \code{SRR}.
#' @param week_strata vector of intervals delimiting the weekly strata to summarize mark-recapture data over, as returned by the \code{weeklyStrata} function.
#' @param mark_site PTAGIS code for the site at the location being queried for trap rate. This is used to filter out fish that were marked at the fish trap.
#'
#' @import lubridate dplyr
#' @export
#' @return NULL

tagTrapRate = function(trap_dataframe = NULL,
                       week_strata = NULL,
                       mark_site = c('LGRLDR', 'LGR')) {  # added 'LGR' to the list because <2009-09-25 fish tagged in the trapped were labeled 'LGR'


  # query PIT tag detections at Lower Granite
  pit_gra = bind_rows(queryPITtagObs(spp = 'Chinook',
                                     yr = lubridate::year(lubridate::int_start(week_strata[length(week_strata)])),
                                     start_day = paste(lubridate::month(lubridate::int_start(week_strata[1])),
                                                       lubridate::mday(lubridate::int_start(week_strata[1])),
                                                       sep = '/'),
                                     end_day = paste(lubridate::month(lubridate::int_end(week_strata[length(week_strata)])),
                                                     lubridate::mday(lubridate::int_end(week_strata[length(week_strata)])),
                                                     sep = '/'),
                                     span_yrs = ifelse(lubridate::year(lubridate::int_start(week_strata[length(week_strata)])) - lubridate::year(lubridate::int_start(week_strata[1])) == 0, F, T)) %>%
                        filter(lubridate::month(ObsTime) <= 8 | (lubridate::month(ObsTime) == 8 & lubridate::day(ObsTime) <= 17)),
                      queryPITtagObs(spp = 'Steelhead',
                                     yr = lubridate::year(lubridate::int_start(week_strata[length(week_strata)])),
                                     start_day = paste(lubridate::month(lubridate::int_start(week_strata[1])),
                                                       lubridate::mday(lubridate::int_start(week_strata[1])),
                                                       sep = '/'),
                                     end_day = paste(lubridate::month(lubridate::int_end(week_strata[length(week_strata)])),
                                                     lubridate::mday(lubridate::int_end(week_strata[length(week_strata)])),
                                                     sep = '/'),
                                     span_yrs = ifelse(lubridate::year(lubridate::int_start(week_strata[length(week_strata)])) - lubridate::year(lubridate::int_start(week_strata[1])) == 0, F, T))) %>%
    arrange(ObsTime)


  # filter out tags that were put in at Lower Granite
  gra_tags = pit_gra %>%
    filter(MarkSite %in% mark_site) %>%  # look for more than one mark_site
    select(TagID) %>%
    distinct() %>%
    as.matrix() %>%
    as.character()

  trap_rate_mr = trap_dataframe %>%
    filter(!Tag.ID %in% gra_tags,
           !is.na(Tag.ID)) %>%
    select(TagID = Tag.ID, TrapDate = Date, SRR) %>%
    full_join(pit_gra %>%
                filter(!TagID %in% gra_tags) %>%
                select(TagID, SpRRT, MarkSite, ReleaseDate, matches('Time')), by = 'TagID') %>%
    mutate(Species = ifelse(grepl('^1', SpRRT) | grepl('^1', SRR),
                            'Chinook',
                            ifelse(grepl('^3', SpRRT) | grepl('^3', SRR),
                                   'Steelhead', NA))) %>%
    mutate(ObsDate = lubridate::floor_date(ObsTime, unit = 'days'),
           diff = as.numeric(difftime(ObsDate, TrapDate, units = 'days'))) %>%
    mutate_at(vars(TrapDate, ObsDate),
              funs(lubridate::ymd)) %>%
    mutate(modDate = if_else(is.na(TrapDate), ObsDate, TrapDate)) %>%
    # mutate(modDate = if_else(abs(diff) < 2, modDate, floor_date(MaxTime, unit = 'days'))) %>%
    mutate(inTrap = if_else(!is.na(TrapDate), T, F))

  # assign week number
  trap_rate_mr$week_num = NA
  for(i in 1:length(week_strata)) {
    trap_rate_mr$week_num[which(lubridate::ymd(trap_rate_mr$modDate) %within% week_strata[i])] = i
  }

  prop_rate = trap_rate_mr %>%
    filter(!is.na(ObsTime)) %>%
    group_by(week_num) %>%
    summarise(n_trap = sum(inTrap),
              n_tot = n_distinct(TagID),
              rate = n_trap / n_tot,
              rate_se = sqrt((rate * (1 - rate)) / n_tot),
              rate_cv = rate_se / rate)

  return(prop_rate)

}

