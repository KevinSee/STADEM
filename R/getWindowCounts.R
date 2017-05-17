#' @title Compile Window Counts
#'
#' @description Query and compile window count results using the \code{queryWindowCnts} function.
#'
#' @author Kevin See
#'
#' @param dam the dam code for the dam you wish to query for window counts. Possible codes are: WFF (Willamette Falls), BON (Bonneville), TDA (The Dalles), JDA (John Day), MCN (McNary), IHR (Ice Harbor), LMN (Lower Monumental), LGS (Little Goose), LWG (Lower Granite), PRO (Prosser), ROZ (Roza), PRD (Priest Rapids), WAN (Wanapum), RIS (Rock Island), TUM (Tumwater), RRH (Rocky Reach), WEL (Wells), ZOS (Zosel)
#' @param spp species to query window counts for. Possible species are: Chinook, Coho, Sockeye, Steelhead, Wild_Steelhead (unclipped), Shad, Jack_Chinook, Jack_Coho, Jack_Sockeye, Jack_Steelhead, Lamprey, Bull_Trout
#' @param spawn_yr spawn year to query for window counts.
#' @param start_day date (\code{month / day}) when query should start
#' @param end_day date (\code{month / day}) when query should end
#' @param incl_jacks should jacks be included in the window count totals? \code{T / F}
#' @param sthd_type should window counts of steelhead be for all steelhead, or only unclipped (i.e. wild) fish? Default is \code{all}.
#'
#' @import lubridate dplyr
#' @export
#' @return NULL
#' @examples getWindowCounts(spawn_yr = 2015)

getWindowCounts = function(dam = c('LWG', 'WFF', 'BON', 'TDA', 'JDA', 'MCN', 'IHR', 'LMN', 'LGS', 'PRO', 'ROZ', 'PRD', 'WAN', 'RIS', 'TUM', 'RRH', 'WEL', 'ZOS'),
                           spp = c('Chinook', 'Coho', 'Sockeye', 'Steelhead', 'Wild_Steelhead', 'Shad', 'Jack_Chinook', 'Jack_Coho', 'Jack_Sockeye', 'Jack_Steelhead', 'Lamprey', 'Bull_Trout'),
                           spawn_yr = NULL,
                           start_day = NULL,
                           end_day = NULL,
                           incl_jacks = F,
                           sthd_type = c('all', 'unclipped')) {

  # need a year
  stopifnot(!is.null(spawn_yr))

  # pull out default dam
  dam = match.arg(dam)

  # default species?
  spp = match.arg(spp)

  # set up default start and end days
  if(dam == 'LWG' & spp == 'Chinook' & is.null(start_day)) start_day = '03/01'
  if(dam == 'LWG' & spp == 'Chinook' & is.null(end_day)) end_day = '08/17'

  if(dam == 'LWG' & grepl('Steelhead', spp) & is.null(start_day)) start_day = '07/01'
  if(dam == 'LWG' & grepl('Steelhead', spp) & is.null(end_day)) end_day = '06/30'

  # include clipped steelhead in counts, or unclipped only?
  sthd_type = match.arg(sthd_type)

  if(spp == 'Steelhead' & sthd_type == 'unclipped') spp = 'Wild_Steelhead'

  # match up species code with species name
  spp_code_df = tibble(Species = c('Chinook', 'Coho', 'Sockeye', 'Steelhead', 'Wild_Steelhead', 'Shad', 'Jack_Chinook', 'Jack_Coho', 'Jack_Sockeye', 'Jack_Steelhead', 'Lamprey', 'Bull_Trout'),
                       code = c('fc', 'fk', 'fb', 'fs', 'fsw', 'fa', 'fcj', 'fkj', 'fbj', 'fsj', 'fl', 'ft'),
                       num_code = c(1,2,4,3, rep(NA, 8)))


  spp_code = spp_code_df$code[spp_code_df$Species == spp]

  win_cnts = queryWindowCnts(dam,
                  spp_code,
                  spawn_yr,
                  start_day,
                  end_day)

  if(!incl_jacks) win_cnts = win_cnts %>%
    gather(Species, win_cnt, -(Year:Date)) %>%
    select(Species, everything())

  if(incl_jacks) {
    jack_code = spp_code_df$code[spp_code_df$Species == paste('Jack', spp, sep = '_')]
    jack_cnts = try(queryWindowCnts(dam,
                                    jack_code,
                                    spawn_yr,
                                    start_day,
                                    end_day))
    stopifnot(class(jack_cnts) != 'try-error')

    adult_cnts = win_cnts
    names(adult_cnts)[match(spp, names(adult_cnts))] = 'adults'
    names(jack_cnts)[grepl(spp, names(jack_cnts))] = 'jacks'

    win_cnts = adult_cnts %>%
      full_join(jack_cnts) %>%
      mutate_at(vars(adults, jacks), funs(ifelse(is.na(.), 0, .))) %>%
      mutate(win_cnt = adults + jacks) %>%
      mutate(Species = spp) %>%
      select(Species, Year, Date, win_cnt)
  }

  return(win_cnts)
}
