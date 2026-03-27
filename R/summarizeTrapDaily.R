#' @title Summarize Trap Database
#'
#' @description Summarize a trap database on daily time-step
#'
#' @author Kevin See
#'
#' @param trap_df data.frame such as the one returned by \code{readLGRtrapDB}.
#' @param spp species to summarise trap data for. Possible species are: \code{Chinook}, \code{Steelhead}, and \code{Coho}.
#' @param incl_clip_sthd should clipped steelhead in the trap be included in this summary? Should match with the window counts. Default is \code{TRUE}.
#' @param sthd_B_run should numbers of B run steelhead be reported? These are defined as wild steelhead greater than 780mm in length. Default is \code{FALSE}.
#'
#' @import lubridate dplyr stringr
#' @return NULL
#' @export
#' @examples #summariseTrapDaily(readLGRtrapDB())

summarizeTrapDaily = function(trap_df,
                              spp = c('Chinook', 'Steelhead', 'Coho'),
                              incl_clip_sthd = TRUE,
                              sthd_B_run = FALSE) {

  spp = match.arg(spp)

  trap_df = trap_df %>%
    janitor::clean_names()

  # standardize selected column names
  trap_col_nms <- names(trap_df)
  trap_col_nms <- case_when(trap_col_nms == "event_date" ~ "date",
                            trap_col_nms == "species_run_rear_type" ~ "srr",
                            trap_col_nms == "lgd_mark_ad" ~ "ad_clip",
                            trap_col_nms == "lgd_sex" ~ "sex",
                            trap_col_nms == "lgdf_lmm" ~ "length",
                            trap_col_nms == "lgd_rear" ~ "origin",
                            trap_col_nms == "lgd_valid" ~ "valid",
                            .default = trap_col_nms)
  names(trap_df) <- trap_col_nms

  if(!"cwt" %in% names(trap_df) & "lgd_tags_all" %in% names(trap_df)) {
    trap_df <-
      trap_df |>
      dplyr::mutate(cwt = case_when(stringr::str_detect(lgd_tags_all, "Wire") ~ TRUE,
                                    stringr::str_detect(lgd_tags_all, "Wire", negate = T) ~ FALSE,
                                    is.na(lgd_tags_all) ~ NA,
                                    .default = NA))
  }

  if(!"valid" %in% names(trap_df)) {
    trap_df <-
      trap_df |>
      dplyr::mutate(valid = 1)
  }

  # make "ad_clip" logical
  if(!inherits(trap_df$ad_clip, "logical")) {
    trap_df <-
      trap_df %>%
      dplyr::mutate(across(ad_clip,
                           ~ case_when(. == "AD" ~ T,
                                       . == "AI" ~ F,
                                       .default = NA)))
  }


  # filter for selected species
  if(!"species" %in% names(trap_df) & "srr" %in% names(trap_df)) {
    trap_df <-
      trap_df |>
      dplyr::mutate(species_code = stringr::str_sub(srr, 1, 1),
                    species = dplyr::case_when(species_code == "0" ~ "Unknown",
                                               species_code == "1" ~ "Chinook",
                                               species_code == "2" ~ "Coho",
                                               species_code == "3" ~ "Steelhead",
                                               species_code == "4" ~ "Sockeye",
                                               species_code == "5" ~ "Chum",
                                               species_code == "6" ~ "Pink",
                                               species_code == "7" ~ "Bull Trout",
                                               species_code == "8" ~ "Cutthroat Trout",
                                               species_code == "9" ~ "Other",
                                               .default = NA_character_)) |>
      dplyr::select(-species_code)
  }

  trap_df <-
    trap_df |>
    dplyr::mutate(across(species,
                         ~ stringr::str_to_title(.))) |>
    dplyr::filter(species == spp)


  # drop unclipped steelhead if only focusing on unclipped fish
  # use only unclipped steelhead (to match with window counts)
  if(!incl_clip_sthd & spp == 'Steelhead'){
    trap_df <-
      trap_df |>
      filter(!ad_clip)
  }


  # summarise by spawnyear, species and day
  trap_daily = trap_df %>%
    group_by(date) %>%
    summarize(trap_fish = n(),
              Wild.morph = sum(origin == 'W'),
              Hatch.morph = sum(origin == 'H'),
              NA.morph = sum(origin %in% c('U', 'NI')),
              Wild.PBT = sum(str_detect(srr, "W$")),
              HNC.PBT = sum(str_detect(srr, "H$") & !ad_clip),
              Hatch.PBT = sum(str_detect(srr, "H$") & ad_clip),
              NA.PBT = sum(!(str_detect(srr, "H$") |
                               str_detect(srr, "W$"))),
              n_invalid = sum(valid != 1)) %>%
    ungroup() %>%
    arrange(date)

  if(sthd_B_run) {
    trap_daily <-
      trap_daily |>
      dplyr::left_join(trap_df  |>
                dplyr::group_by(date)  |>
                dplyr::summarize(tot_B_run_sthd = sum(species == 'Steelhead' & length >= 780 & stringr::str_detect(srr, 'W$')),
                          .groups = "drop"),
              by = dplyr::join_by(date)) |>
      dplyr::relocate(tot_B_run_sthd,
                      .before = "n_invalid")
  }

  trap_daily <-
    trap_daily |>
    dplyr::mutate(dplyr::across(date,
                                lubridate::ymd))

  return(trap_daily)

}
