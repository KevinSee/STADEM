#' @title Summarise LGR Trap Database
#'
#' @description Summarise LGR trap database on dailiy time-step
#'
#' @author Kevin See
#'
#' @param trap_df data.frame as created by \code{readLGRtrapDB}.
#' @param incl_clip_sthd should clipped steelhead in the trap be included in this summary? Should match with the window counts. Default is \code{FALSE}.
#' @param sthd_B_run should numbers of B run steelhead be reported? These are defined as wild steelhead greater than 780mm in length. Default is \code{FALSE}.
#'
#' @import lubridate dplyr
#' @export
#' @return NULL
#' @examples #summariseLGRtrapDaily(readLGRtrapDB())

summariseLGRtrapDaily = function(trap_df,
                                 incl_clip_sthd = FALSE,
                                 sthd_B_run = FALSE) {

  # summarise by spawnyear, species and day
  lgr_trap_filter = trap_df %>%
    # filter out juveniles, keep only adults
    dplyr::filter(LGDLifeStage == 'RF',
                  # filter out sort by code fish
                  is.na(PTAgisSxCGRAObs))

  if(!incl_clip_sthd) lgr_trap_filter = lgr_trap_filter %>%
      # use all Chinook in the trap, but only unclipped steelhead (to match with windown counts)
      dplyr::filter(Species == 'Chinook' | (Species == 'Steelhead' & LGDMarkAD == 'AI'))

  lgr_trap_daily = lgr_trap_filter %>%
    dplyr::mutate(SpawnYear = gsub('^SY', '', SpawnYear),
                  SpawnYear = as.integer(SpawnYear),
                  SpawnYear = ifelse(!is.na(SpawnYear), SpawnYear, ifelse(Species == 'Chinook', year(Date), ifelse(Species == 'Steelhead' & Date >= ymd(paste0(year(Date), '0701')), year(Date) + 1, year(Date))))) %>%
    group_by(Species, SpawnYear, Date) %>%
    summarize(trap_fish = length(MasterID),
              Wild.morph = sum(LGDRear=='W'),
              Hatch.morph = sum(LGDRear=='H'),
              NA.morph = sum(is.na(LGDRear)),
              Wild.PBT = sum(grepl('W$', SRR)),
              HNC.PBT = sum(grepl('H$', SRR) & LGDMarkAD=='AI'),
              Hatch.PBT = sum(grepl('H$', SRR) & LGDMarkAD=='AD'),
              NA.PBT = sum(is.na(SRR)),
              n_invalid = sum(LGDValid != 1),
              tot_B_run_sthd = sum(Species == 'Steelhead' & LGDFLmm >= 780 & grepl('W$', SRR))) %>%
    ungroup() %>%
    # dplyr::mutate(tot_B_run_sthd = ifelse(Species == 'Steelhead', tot_B_run_sthd, NA)) %>%
    dplyr::arrange(Species, Date)

  if(!sthd_B_run) lgr_trap_daily = lgr_trap_daily %>%
    dplyr::select(-tot_B_run_sthd)

  return(lgr_trap_daily)

}
