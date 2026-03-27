#' @title LGR Trap Database
#'
#' @description Read in data from LGR trap database
#'
#' @author Kevin See
#'
#' @param trap_path file path where a csv file containing the data from the fish trap is located
#' @param date_range vector of length 2, with minimum and maximum dates to read.
#'
#' @import lubridate dplyr vroom
#' @importFrom Hmisc mdb.get
#' @export
#' @return NULL
#' @examples # do not run; #readLGRtrapDB()

readLGRtrapDB = function(trap_path = NULL,
                         date_range = NULL) {

  if(is.null(trap_path)) {
    stop('File path to trap database needed.')
  }

  if(grepl('csv$', trap_path)) {
    lgr_trap <-
      vroom::vroom(trap_path,
                   show_col_types = FALSE) |>
      suppressWarnings()
  }

  if(grepl('accdb$', trap_path)) {
    # tableNms <- tibble(tableNames = Hmisc::mdb.get(trap_path, tables = TRUE))
    lgr_trap = Hmisc::mdb.get(trap_path,
                              tables = 'tblLGDMasterCombineExport') %>%
      as_tibble
  }

  # fix CollectionDate if it's not read in as a date
  if(sum(class(lgr_trap$CollectionDate) %in% c('character', 'factor')) > 0 ) {
    lgr_trap <- lgr_trap %>%
      mutate(CollectionDate = lubridate::mdy_hms(CollectionDate))
  }

  lgr_trap = lgr_trap %>%
    mutate(Date = lubridate::floor_date(CollectionDate, unit = 'day'),
           SppCode = LGDSpecies,
           LGDNumPIT = as.character(LGDNumPIT),
           LGDNumPIT = ifelse(nchar(LGDNumPIT) < 3, NA, LGDNumPIT),
           # try to correct some incorrect species codes
           # SppCode = ifelse(LGDSpecies != PtagisSpecies & GenSpecies %in% c(1, 3), as.integer(as.character(GenSpecies)), SppCode),
           Species = ifelse(SppCode == 1, 'Chinook', ifelse(SppCode == 3, 'Steelhead', ifelse(SppCode == 2, 'Coho', NA))))

  if(is.null(date_range)) {
    date_range = range(lgr_trap$Date, na.rm = T)
  }

  lgr_trap = lgr_trap %>%
    # drop data from other species, and other runs of Chinook
    filter(Species %in% c('Chinook', 'Steelhead', 'Coho'),
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
