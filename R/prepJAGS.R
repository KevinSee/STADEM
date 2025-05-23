#' @title Compile JAGS data
#'
#' @description Compile data into format for the JAGS total escapement model
#'
#' @author Kevin See
#'
#' @param dam_weekly dataframe containing weekly summaries of window counts and trap data. Part of what is returned by \code{summSTADEM}.
#' @param sthd_B_run should numbers of B run steelhead be incorporated? These are defined as wild steelhead greater than 780mm in length. Default is \code{FALSE}.
#' @param hw_type what criteria is used to determine hatchery vs. wild? \code{Morph} uses the morphilogical call made at the trap. \code{PBT} uses the genetic information gleaned from parental based tagging.
#' @param wild_tags Should only wild PIT tags be used to estimate daytime passage and re-ascension rates? Default is \code{FALSE}.
#'
#' @import dplyr
#' @export
#' @return NULL
#'
prepJAGS = function(dam_weekly = NULL,
                    sthd_B_run = F,
                    hw_type = c('PBT', 'Morph'),
                    wild_tags = F) {

  stopifnot(!is.null(dam_weekly))

  hw_type = match.arg(hw_type)

  message('Prepping JAGS data \n')

  if(hw_type == 'PBT') dam_weekly = dam_weekly %>%
    rename(wild_fish = Wild.PBT,
           hatch_fish = Hatch.PBT,
           hnc_fish = HNC.PBT)

  if(hw_type == 'Morph') dam_weekly = dam_weekly %>%
    rename(wild_fish = Wild.morph,
           hatch_fish = Hatch.morph) %>%
    mutate(hnc_fish = 0)

  # is ladder open? 1 if yes, 0 if no
  ladder = dam_weekly %>%
    mutate(open = if_else(window_open | trap_open,
                             1, 0)) %>%
    pull(open)

  org_exist = dam_weekly %>%
    select(wild_fish, hnc_fish, hatch_fish) %>%
    colSums()
  org_exist = ifelse(org_exist > 0, 1, org_exist)

  jags_data_list = list('TotLadderWeeks' = nrow(dam_weekly),
                        'ladder' = ladder,
                        'Y.window' = dam_weekly %>%
                          mutate(win_cnt = ifelse(!window_open, NA, win_cnt)) %>%
                          pull(win_cnt),
                        'Y.trap' = dam_weekly %>%
                          mutate(y_trap = trap_fish,
                                 y_trap = ifelse(!trap_open | !trap_valid, NA, y_trap)) %>%
                          pull(y_trap),
                        'trap.fish' = dam_weekly %>%
                          mutate(trap_fish = wild_fish + hnc_fish + hatch_fish) %>%
                          pull(trap_fish),
                        'trap.fish.matrix' = dam_weekly %>%
                          select(wild_fish, hnc_fish, hatch_fish) %>%
                          as.matrix(),
                        'org.exist' = org_exist,
                        # 'trap.rate' = dam_weekly %>%
                        #   mutate(trap_rate = ifelse(!trap_open | !trap_valid | is.na(trap_fish), 0, trap_rate)) %>%
                        #   pull(trap_rate),
                        # 'trap.alpha' = dam_weekly %>%
                        #   pull(trap_alpha),
                        # 'trap.beta' = dam_weekly %>%
                        #   pull(trap_beta),
                        'n.trap.tags' = dam_weekly %>%
                          mutate(n_tags = if_else(trap_valid & trap_open & !is.na(n_poss_tags), n_trap_tags, NA_integer_)) %>%
                          pull(n_tags),
                        'n.poss.tags' = dam_weekly %>%
                          mutate(n_poss_tags = if_else(is.na(n_poss_tags), as.integer(0), n_poss_tags)) %>%
                          pull(n_poss_tags),
                        'Tot.tags' = dam_weekly %>%
                          mutate(Tot_tags = ifelse(is.na(tot_tags), 0, tot_tags)) %>%
                          pull(Tot_tags),
                        'ReAsc.tags' = dam_weekly %>%
                          mutate(ReAsc_tags = ifelse(reascent_tags > tot_tags, tot_tags, reascent_tags),
                                 ReAsc_tags = ifelse(!ladder, NA, ReAsc_tags)) %>%
                          pull(ReAsc_tags),
                        'DC.tags' = dam_weekly %>%
                          mutate(Day_tags = ifelse(day_tags > tot_tags, tot_tags, day_tags),
                                 Day_tags = ifelse(!ladder, NA, Day_tags)) %>%
                          pull(Day_tags)
  )

  # set any negative window counts to 0
  if(sum(jags_data_list$Y.window < 0, na.rm = T) > 0) {
    jags_data_list$Y.window[jags_data_list$Y.window < 0] = 0
  }

  if(wild_tags) {
    jags_data_list[['Tot.Tags']] = dam_weekly %>%
      mutate(Tot_tags = ifelse(is.na(tot_tags_W), 0, tot_tags_W)) %>%
      pull(Tot_tags)
    jags_data_list[['ReAsc.tags']] = dam_weekly %>%
      mutate(ReAsc_tags = ifelse(reascent_tags_W > tot_tags_W, tot_tags_W, reascent_tags_W),
             ReAsc_tags = ifelse(!trap_open, NA, ReAsc_tags)) %>%
      pull(ReAsc_tags)
    jags_data_list[['DC.tags']] = dam_weekly %>%
      mutate(Day_tags = ifelse(day_tags_W > tot_tags_W, tot_tags_W, day_tags_W),
             Day_tags = ifelse(!trap_open, NA, Day_tags)) %>%
      pull(Day_tags)
  }


  if(sthd_B_run) jags_data_list[['B.trap']] = dam_weekly %>%
    pull(tot_B_run_sthd)

  return(jags_data_list)
}
