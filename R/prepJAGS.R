#' @title Compile JAGS data
#'
#' @description Compile data into format for the JAGS total escapement model
#'
#' @author Kevin See
#'
#' @param lgr_weekly dataframe containing weekly summaries of window counts and trap data. Part of what is returned by \code{summSTADEM}.
#' @param sthd_B_run should numbers of B run steelhead be incorporated? These are defined as wild steelhead greater than 780mm in length. Default is \code{FALSE}.
#' @param hw_type what criteria is used to determine hatchery vs. wild? \code{Morph} uses the morphilogical call made at the trap. \code{PBT} uses the genetic information gleaned from parental based tagging.
#'
#' @import dplyr
#' @importFrom plyr dlply
#' @export
#' @return NULL
#'
prepJAGS = function(lgr_weekly = NULL,
                    sthd_B_run = F,
                    hw_type = c('PBT', 'Morph')) {

  stopifnot(!is.null(lgr_weekly))

  hw_type = match.arg(hw_type)

  if(hw_type == 'PBT') lgr_weekly = lgr_weekly %>%
    rename(wild_fish = Wild.PBT,
           hatch_fish = Hatch.PBT,
           hnc_fish = HNC.PBT)

  if(hw_type == 'Morph') lgr_weekly = lgr_weekly %>%
    rename(wild_fish = Wild.morph,
           hatch_fish = Hatch.morph,
           hnc_fish = HNC.morph)

  # filter out weeks when ladder not open
  lgr_weekly = lgr_weekly %>%
    filter(window_open | trap_open)

  # month_vec = month(lgr_weekly$Start_Date)
  # spp = unique(lgr_weekly$Species)
  #
  org_exist = lgr_weekly %>%
    select(wild_fish, hnc_fish, hatch_fish) %>%
    colSums()
  org_exist = ifelse(org_exist > 0, 1, org_exist)

  jags_data_list = list('TotLadderWeeks' = nrow(lgr_weekly),
                        'Y.window' = lgr_weekly %>%
                          mutate(win_cnt = ifelse(!window_open, NA, win_cnt)) %>%
                          select(win_cnt) %>%
                          as.matrix() %>% as.vector(),
                        'Y.trap' = lgr_weekly %>%
                          mutate(y_trap = trap_fish,
                                 y_trap = ifelse(!trap_open | !trap_valid, NA, y_trap)) %>%
                          select(y_trap) %>%
                          as.matrix() %>% as.vector(),
                        # 'trap.fish' = lgr_weekly %>% select(trap_fish) %>% as.matrix() %>% as.vector(),
                        'trap.fish' = lgr_weekly %>%
                          mutate(trap_fish = wild_fish + hnc_fish + hatch_fish) %>%
                          select(trap_fish) %>%
                          as.matrix() %>% as.vector(),
                        # 'wild.fish' = lgr_weekly %>% select(wild_fish) %>% as.matrix() %>% as.vector(),
                        # 'hatch.fish' = lgr_weekly %>% select(hatch_fish) %>% as.matrix() %>% as.vector(),
                        # 'hnc.fish' = lgr_weekly %>% select(hnc_fish) %>% as.matrix() %>% as.vector(),
                        'trap.fish.matrix' = lgr_weekly %>%
                          select(wild_fish, hnc_fish, hatch_fish) %>%
                          as.matrix(),
                        'org.exist' = org_exist,
                        # 'trap.rate' = lgr_weekly %>% mutate(trap_rate = ifelse(!trap_open | !trap_valid | is.na(trap_fish), 0, trap_rate)) %>%
                        #   select(trap_rate) %>% as.matrix() %>% as.vector(),
                        'trap.alpha' = lgr_weekly %>%
                          select(trap_alpha) %>%
                          as.matrix() %>% as.vector(),
                        'trap.beta' = lgr_weekly %>%
                          select(trap_beta) %>%
                          as.matrix() %>% as.vector(),
                        'Tot.tags' = lgr_weekly %>%
                          mutate(Tot_tags = ifelse(is.na(tot_tags_W), 0, tot_tags_W)) %>%
                          select(Tot_tags) %>% as.matrix() %>% as.vector(),
                        'ReAsc.tags' = lgr_weekly %>%
                          mutate(ReAsc_tags = ifelse(reascent_tags_W > tot_tags_W, tot_tags_W, reascent_tags_W),
                                 ReAsc_tags = ifelse(!trap_open, NA, ReAsc_tags)) %>%
                          select(ReAsc_tags) %>%
                          as.matrix() %>% as.vector(),
                        'DC.tags' = lgr_weekly %>%
                          mutate(Day_tags = ifelse(day_tags_W > tot_tags_W, tot_tags_W, day_tags_W),
                                 Day_tags = ifelse(!trap_open, NA, Day_tags)) %>%
                          select(Day_tags) %>%
                          as.matrix() %>% as.vector()
  )

  if(sthd_B_run) jags_data_list[['B.trap']] = lgr_weekly %>%
    select(tot_B_run_sthd) %>%
    as.matrix() %>% as.vector()

  return(jags_data_list)
}
