#' @title LGR Trap Database
#'
#' @description Read in data from LGR trap database
#'
#' @author Kevin See
#'
#' @param filepath path to the LGR trap database as csv file
#' @param spring_summer_chnk should the PIT tag data be filtered to exclude fall and winter run Chinook? Default is \code{TRUE}
#'
#' @import lubridate dplyr
#' @export
#' @return NULL
#' @examples # do not run; #readLGRtrapDB()

readLGRtrapDB = function(filepath = 'data-raw/tblLGDMasterCombineExportJodyW.csv',
                         spring_summer_chnk = T) {

  lgr_trap = read.csv(filepath) %>%
    tbl_df() %>%
    dplyr::rename(Tag.ID = LGDNumPIT) %>%
    mutate(Date = floor_date(ymd_hms(CollectionDate), unit = 'day'),
           SppCode = LGDSpecies,
        # try to correct some incorrect species codes
           SppCode = ifelse(LGDSpecies != PtagisSpecies & GenSpecies %in% c(1, 3), as.integer(as.character(GenSpecies)), SppCode),
           Species = ifelse(SppCode == 1, 'Chinook', ifelse(SppCode == 3, 'Steelhead', NA))) %>%
    # drop data from other species, and other runs of Chinook
    filter(Species %in% c('Chinook', 'Steelhead'))

  if(spring_summer_chnk) lgr_trap = lgr_trap %>%
      filter(!(Species == 'Chinook' & Date > ymd(paste0(year(Date), '0817'))))

  return(lgr_trap)
}
