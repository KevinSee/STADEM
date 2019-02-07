#' @title LGR Trap Database
#'
#' @description Read in data from LGR trap database
#'
#' @author Kevin See
#'
#' @param trap_path file path where a csv file containing the data from the fish trap is located
#' @param date_range vector of length 2, with minimum and maximum dates to read.
#'
#' @import readr lubridate dplyr
#' @export
#' @return NULL
#' @examples # do not run; #readLGRtrapDB()

readLGRtrapDB = function(trap_path = NULL,
                         date_range = NULL) {

  if(is.null(trap_path)) {
    stop('File path to trap database needed.')
  }

  lgr_trap = readr::read_csv(trap_path) %>%
    rename(Tag.ID = LGDNumPIT) %>%
    mutate(Date = lubridate::floor_date(CollectionDate, unit = 'day'),
           SppCode = LGDSpecies,
           Tag.ID = as.character(Tag.ID),
           Tag.ID = ifelse(nchar(Tag.ID) < 3, NA, Tag.ID),
           # try to correct some incorrect species codes
           SppCode = ifelse(LGDSpecies != PtagisSpecies & GenSpecies %in% c(1, 3), as.integer(as.character(GenSpecies)), SppCode),
           Species = ifelse(SppCode == 1, 'Chinook', ifelse(SppCode == 3, 'Steelhead', NA)))

  if(is.null(date_range)) {
    date_range = range(lgr_trap$Date)
  }

  lgr_trap = lgr_trap %>%
    # drop data from other species, and other runs of Chinook
    filter(Species %in% c('Chinook', 'Steelhead'),
           # !(Species == 'Chinook' & Date > lubridate::ymd(paste0(lubridate::year(Date), '0817'))),
           # filter for date range
           Date >= date_range[1],
           Date < date_range[2],
           # filter out juveniles, keep only adults
           LGDLifeStage == 'RF',
           # drop sort-by-code fish
           (PTAgisSxCGRAObs != 'Yes' | is.na(PTAgisSxCGRAObs) ) )


  return(lgr_trap)
}
