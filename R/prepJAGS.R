#' @title Compile JAGS data
#'
#' @description Compile data into format for the JAGS total escapement model
#'
#' @author Kevin See
#'
#' @param lgr_weekly dataframe containing weekly summaries of window counts and trap data. Part of what is returned by \code{summSTADEM}.
#' @param sthd_B_run should numbers of B run steelhead be incorporated? These are defined as wild steelhead greater than 780mm in length. Default is \code{FALSE}.
#' @param hw_type what criteria is used to determine hatchery vs. wild? \code{Morph} uses the morphilogical call made at the trap. \code{PBT} uses the genetic information gleaned from parental based tagging.
#' @param wild_tags Should only wild PIT tags be used to estimate daytime passage and re-ascension rates? Default is \code{FALSE}.
#'
#' @import dplyr
#' @export
#' @return NULL
#'
prepJAGS = function(lgr_weekly = NULL,
                    sthd_B_run = F,
                    hw_type = c('PBT', 'Morph'),
                    wild_tags = F) {

  stopifnot(!is.null(lgr_weekly))

  hw_type = match.arg(hw_type)

  cat('Prepping JAGS data \n')

  if(hw_type == 'PBT') lgr_weekly = lgr_weekly %>%
    rename(wild_fish = Wild.PBT,
           hatch_fish = Hatch.PBT,
           hnc_fish = HNC.PBT)

  if(hw_type == 'Morph') lgr_weekly = lgr_weekly %>%
    rename(wild_fish = Wild.morph,
           hatch_fish = Hatch.morph) %>%
    mutate(hnc_fish = 0)

  # is ladder open? 1 if yes, 0 if no
  ladder = lgr_weekly %>%
    mutate(open = if_else(window_open | trap_open,
                             1, 0)) %>%
    pull(open)

  org_exist = lgr_weekly %>%
    select(wild_fish, hnc_fish, hatch_fish) %>%
    colSums()
  org_exist = ifelse(org_exist > 0, 1, org_exist)

  jags_data_list = list('TotLadderWeeks' = nrow(lgr_weekly),
                        'ladder' = ladder,
                        'Y.window' = lgr_weekly %>%
                          mutate(win_cnt = ifelse(!window_open, NA, win_cnt)) %>%
                          pull(win_cnt),
                        'Y.trap' = lgr_weekly %>%
                          mutate(y_trap = trap_fish,
                                 y_trap = ifelse(!trap_open | !trap_valid, NA, y_trap)) %>%
                          pull(y_trap),
                        'trap.fish' = lgr_weekly %>%
                          mutate(trap_fish = wild_fish + hnc_fish + hatch_fish) %>%
                          pull(trap_fish),
                        'trap.fish.matrix' = lgr_weekly %>%
                          select(wild_fish, hnc_fish, hatch_fish) %>%
                          as.matrix(),
                        'org.exist' = org_exist,
                        # 'trap.rate' = lgr_weekly %>%
                        #   mutate(trap_rate = ifelse(!trap_open | !trap_valid | is.na(trap_fish), 0, trap_rate)) %>%
                        #   pull(trap_rate),
                        # 'trap.alpha' = lgr_weekly %>%
                        #   pull(trap_alpha),
                        # 'trap.beta' = lgr_weekly %>%
                        #   pull(trap_beta),
                        'n.trap.tags' = lgr_weekly %>%
                          mutate(n_tags = if_else(trap_valid & trap_open & !is.na(n_poss_tags), n_trap_tags, NA_integer_)) %>%
                          pull(n_tags),
                        'n.poss.tags' = lgr_weekly %>%
                          mutate(n_poss_tags = if_else(is.na(n_poss_tags), as.integer(0), n_poss_tags)) %>%
                          pull(n_poss_tags),
                        'Tot.tags' = lgr_weekly %>%
                          mutate(Tot_tags = ifelse(is.na(tot_tags), 0, tot_tags)) %>%
                          pull(Tot_tags),
                        'ReAsc.tags' = lgr_weekly %>%
                          mutate(ReAsc_tags = ifelse(reascent_tags > tot_tags, tot_tags, reascent_tags),
                                 ReAsc_tags = ifelse(!ladder, NA, ReAsc_tags)) %>%
                          pull(ReAsc_tags),
                        'DC.tags' = lgr_weekly %>%
                          mutate(Day_tags = ifelse(day_tags > tot_tags, tot_tags, day_tags),
                                 Day_tags = ifelse(!ladder, NA, Day_tags)) %>%
                          pull(Day_tags)
  )

  # set any negative window counts to 0
  if(sum(jags_data_list$Y.window < 0, na.rm = T) > 0) {
    jags_data_list$Y.window[jags_data_list$Y.window < 0] = 0
  }

  if(wild_tags) {
    jags_data_list[['Tot.Tags']] = lgr_weekly %>%
      mutate(Tot_tags = ifelse(is.na(tot_tags_W), 0, tot_tags_W)) %>%
      pull(Tot_tags)
    jags_data_list[['ReAsc.tags']] = lgr_weekly %>%
      mutate(ReAsc_tags = ifelse(reascent_tags_W > tot_tags_W, tot_tags_W, reascent_tags_W),
             ReAsc_tags = ifelse(!trap_open, NA, ReAsc_tags)) %>%
      pull(ReAsc_tags)
    jags_data_list[['DC.tags']] = lgr_weekly %>%
      mutate(Day_tags = ifelse(day_tags_W > tot_tags_W, tot_tags_W, day_tags_W),
             Day_tags = ifelse(!trap_open, NA, Day_tags)) %>%
      pull(Day_tags)
  }


  if(sthd_B_run) jags_data_list[['B.trap']] = lgr_weekly %>%
    pull(tot_B_run_sthd)

  return(jags_data_list)
}
