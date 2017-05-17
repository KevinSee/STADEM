#' @title LGR Trap Database
#'
#' @description Read in data from LGR trap database
#'
#' @author Kevin See
#'
#' @param filepath path to the LGR trap database as csv file
#' @param date_range vector of length 2, with minimum and maximum dates to read.
#'
#' @import lubridate dplyr
#' @export
#' @return NULL
#' @examples # do not run; #readLGRtrapDB()

readLGRtrapDB = function(filepath = NULL,
                         date_range = NULL) {

  stopifnot(!is.null(filepath))

  lgr_trap = read.csv(filepath) %>%
    tbl_df() %>%
    dplyr::rename(Tag.ID = LGDNumPIT) %>%
    dplyr::mutate(Date = floor_date(ymd_hms(CollectionDate), unit = 'day'),
                  SppCode = LGDSpecies,
                  # try to correct some incorrect species codes
                  SppCode = ifelse(LGDSpecies != PtagisSpecies & GenSpecies %in% c(1, 3), as.integer(as.character(GenSpecies)), SppCode),
                  Species = ifelse(SppCode == 1, 'Chinook', ifelse(SppCode == 3, 'Steelhead', NA))) %>%
    # drop data from other species, and other runs of Chinook
    dplyr::filter(Species %in% c('Chinook', 'Steelhead'),
                  !(Species == 'Chinook' & Date > lubridate::ymd(paste0(lubridate::year(Date), '0817'))),
                  # filter for date range
                  Date >= date_range[1],
                  Date < date_range[2],
                  # filter out juveniles, keep only adults
                  LGDLifeStage == 'RF',
                  # drop sort-by-code fish
                  (PTAgisSxCGRAObs != 'Yes' | is.na(PTAgisSxCGRAObs) ) )

  return(lgr_trap)
}
