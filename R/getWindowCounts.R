#' @title Compile Window Counts
#'
#' @description Query and compile window count results using the \code{queryWindowCnts} function.
#'
#' @author Kevin See
#'
#' @inheritParams queryWindowCnts
#' @param incl_jacks should jacks be included in the window count totals? \code{T / F} Default is `FALSE`
#' @param sthd_type should window counts of steelhead be for all steelhead, or only unclipped (i.e. wild) fish? Default is \code{all}.
#'
#' @import dplyr tidyr
#' @export
#' @return NULL
#' @examples getWindowCounts(start_date = '20150301', end_date = '20150817')

getWindowCounts = function(dam = c('LWG', 'WFF', 'BON', 'TDA', 'JDA', 'MCN', 'IHR', 'LMN', 'LGS', 'PRO', 'ROZ', 'PRD', 'WAN', 'RIS', 'TUM', 'RRH', 'WEL', 'ZOS'),
                           spp = c('Chinook', 'Coho', 'Sockeye', 'Steelhead', 'Wild_Steelhead', 'Shad', 'Jack_Chinook', 'Jack_Coho', 'Jack_Sockeye', 'Jack_Steelhead', 'Lamprey', 'Bull_Trout'),
                           start_date = NULL,
                           end_date = NULL,
                           incl_jacks = F,
                           sthd_type = c('all', 'unclipped')) {

  # need a start date
  stopifnot(!is.null(start_date))

  # pull out default dam
  dam = match.arg(dam)

  # default species?
  spp = match.arg(spp)

  # include clipped steelhead in counts, or unclipped only?
  sthd_type = match.arg(sthd_type)

  if(spp == 'Steelhead' & sthd_type == 'unclipped') spp = 'Wild_Steelhead'

  # match up species code with species name
  spp_code <- dplyr::case_when(spp == "Chinook" ~ "fc",
                               spp == "Coho" ~ "fk",
                               spp == "Sockeye" ~ "fb",
                               spp == "Steelhead" ~ "fs",
                               spp == "Wild_Steelhead" ~ "fsw",
                               spp == "Shad" ~ "fa",
                               spp == "Jack_Chinook" ~ "fcj",
                               spp == "Jack_Coho" ~ "fkj",
                               spp == "Jack_Sockeye" ~ "fbj",
                               spp == "Jack_Steelhead" ~ "fsj",
                               spp == "Lamprey" ~ "fl",
                               spp == "Bull_Trout" ~ "ft",
                               .default = NA_character_)

  win_cnts = queryWindowCnts(dam,
                             spp_code,
                             start_date,
                             end_date)

  if(!incl_jacks) {

    win_cnts <-
      win_cnts %>%
      tidyr::pivot_longer(-c(Location:Date),
                          names_to = "Species",
                          values_to = "win_cnt") |>
      dplyr::select(Species, everything())

  # } else if(incl_jacks & grepl("j$", spp_code)) {
  } else if(incl_jacks) {

    jack_cnts = try(queryWindowCnts(dam,
                                    paste0(spp_code, "j"),
                                    start_date,
                                    end_date))
    stopifnot(class(jack_cnts) != 'try-error')

    adult_cnts = win_cnts
    names(adult_cnts)[match(spp, names(adult_cnts))] = 'adults'
    names(jack_cnts)[grepl(spp, names(jack_cnts))] = 'jacks'

    win_cnts = adult_cnts %>%
      full_join(jack_cnts,
                by = join_by(Location,
                             Year,
                             Date)) %>%
      mutate(across(c(adults,
                      jacks),
                    ~ tidyr::replace_na(., 0))) |>
      mutate(win_cnt = adults + jacks) %>%
      mutate(Species = spp) %>%
      select(Location,
             Species, Year, Date,
             win_cnt)
  }

  return(win_cnts)
}
