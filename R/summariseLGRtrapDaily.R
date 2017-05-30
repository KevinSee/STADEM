#' @title Summarise LGR Trap Database
#'
#' @description Summarise LGR trap database on dailiy time-step
#'
#' @author Kevin See
#'
#' @param trap_df data.frame as created by \code{readLGRtrapDB}.
#' @param spp species to summarise trap data for. Possible species are: \code{Chinook} and \code{Steelhead}.
#' @param incl_clip_sthd should clipped steelhead in the trap be included in this summary? Should match with the window counts. Default is \code{TRUE}.
#' @param sthd_B_run should numbers of B run steelhead be reported? These are defined as wild steelhead greater than 780mm in length. Default is \code{FALSE}.
#'
#' @import lubridate dplyr
#' @return NULL
#' @export
#' @examples #summariseLGRtrapDaily(readLGRtrapDB())

summariseLGRtrapDaily = function(trap_df,
                                 spp = c('Chinook', 'Steelhead'),
                                 incl_clip_sthd = TRUE,
                                 sthd_B_run = FALSE) {

  spp = match.arg(spp)

  # filter for selected species
  trap_df = trap_df %>%
    dplyr::filter(Species == spp)

  # drop unclipped steelhead if only focusing on unclipped fish
  if(!incl_clip_sthd & spp == 'Steelhead') trap_df = trap_df %>%
      # use only unclipped steelhead (to match with windown counts)
      dplyr::filter(LGDMarkAD == 'AI')

  # summarise by spawnyear, species and day
  lgr_trap_daily = trap_df %>%
    # dplyr::mutate(SpawnYear = gsub('^SY', '', SpawnYear),
    #               SpawnYear = as.integer(SpawnYear),
    #               SpawnYear = ifelse(!is.na(SpawnYear), SpawnYear, ifelse(Species == 'Chinook', year(Date), ifelse(Species == 'Steelhead' & Date >= ymd(paste0(year(Date), '0701')), year(Date) + 1, year(Date))))) %>%
    dplyr::group_by(Date) %>%
    dplyr::summarize(trap_fish = length(MasterID),
                     Wild.morph = sum(LGDRear=='W'),
                     Hatch.morph = sum(LGDRear=='H'),
                     NA.morph = sum(LGDRear %in% c('U', 'NI')),
                     Wild.PBT = sum(grepl('W$', SRR)),
                     HNC.PBT = sum(grepl('H$', SRR) & LGDMarkAD=='AI'),
                     Hatch.PBT = sum(grepl('H$', SRR) & LGDMarkAD=='AD'),
                     NA.PBT = sum(!(grepl('W$', SRR) | grepl('H$', SRR))),
                     n_invalid = sum(LGDValid != 1)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Date)

  if(sthd_B_run) lgr_trap_daily = lgr_trap_daily %>%
    dplyr::left_join(trap_df %>%
                dplyr::group_by(Date) %>%
                dplyr::summarize(tot_B_run_sthd = sum(Species == 'Steelhead' & LGDFLmm >= 780 & grepl('W$', SRR))) %>%
                dplyr::ungroup()) %>%
    dplyr::select(Date:NA.PBT, tot_B_run_sthd, n_invalid)

  lgr_trap_daily = lgr_trap_daily %>%
    dplyr::mutate(Date = ymd(Date))

  return(lgr_trap_daily)

}
