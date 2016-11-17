#' @title Compile JAGS data
#'
#' @description Compile data into format for the JAGS total escapement model
#'
#' @author Kevin See
#'
#' @param lgr_weekly dataframe containing weekly summaries of window counts and trap data. Part of what is returned by \code{summariseLGRweekly}.
#' @param sthd_B_run should numbers of B run steelhead be incorporated? These are defined as wild steelhead greater than 780mm in length. Default is \code{FALSE}.
#' @param use_historical should older historical data about night passage rates be incorporated? Default is \code{FALSE}.
#'
#' @import dplyr
#' @importFrom plyr dlply
#' @export
#' @return NULL
#'
prepJAGS = function(lgr_weekly = NULL,
                    sthd_B_run = F,
                    use_historical = F,
                    type = c('PBT', 'Morph')) {

  stopifnot(!is.null(lgr_weekly))

  type = match.arg(type)

  if(type == 'PBT') lgr_weekly = lgr_weekly %>%
    rename(wild_fish = Wild.PBT,
           hatch_fish = Hatch.PBT,
           hnc_fish = HNC.PBT)

  if(type == 'Morph') lgr_weekly = lgr_weekly %>%
    rename(wild_fish = Wild.morph,
           hatch_fish = Hatch.morph,
           hnc_fish = HNC.morph)

  if(use_historical) {
    data(night_passage)
    night_means = night_passage %>%
      mutate(first_date = round_date(ymd(paste(Year, Month, '1')), 'month'),
             month_num = month(ymd(paste(Year, Month, '1'))),
             last_date = round_date(ymd(paste(Year, month_num + 1, '1')), 'month') - days(1),
             month_days = as.integer(last_date - first_date) + 1) %>%
      group_by(Species, Month, month_days) %>%
      summarise_each(funs(mean), tot_fish, day_fish) %>%
      ungroup()

    hist_month_means = expand.grid(list(Month = month(1:12, label = T),
                                        Species = c('Chinook', 'Steelhead'))) %>%
      full_join(night_means)
  }

  jags_data_list = dlply(lgr_weekly, .(Species, SpawnYear), function(x) {
    x = x %>%
      filter(window_open | trap_open)

    month_vec = month(x$Start_Date)
    spp = unique(x$Species)

    org_exist = x %>% select(wild_fish, hnc_fish, hatch_fish) %>% colSums()
    org_exist = ifelse(org_exist > 0, 1, org_exist)

    data_list = list('TotLadderWeeks' = nrow(x),
                     'Y.window' = x %>% mutate(win_cnt = ifelse(!window_open, NA, win_cnt)) %>%
                       select(win_cnt) %>% as.matrix() %>% as.vector(),
                     'Y.trap' = x %>%
                       mutate(y_trap = trap_fish,
                              y_trap = ifelse(!trap_open | !trap_valid, NA, y_trap)) %>%
                       select(y_trap) %>% as.matrix() %>% as.vector(),
                     # 'trap.fish' = x %>% select(trap_fish) %>% as.matrix() %>% as.vector(),
                     'trap.fish' = x %>% mutate(trap_fish = wild_fish + hnc_fish + hatch_fish) %>% select(trap_fish) %>% as.matrix() %>% as.vector(),
                     # 'wild.fish' = x %>% select(wild_fish) %>% as.matrix() %>% as.vector(),
                     # 'hatch.fish' = x %>% select(hatch_fish) %>% as.matrix() %>% as.vector(),
                     # 'hnc.fish' = x %>% select(hnc_fish) %>% as.matrix() %>% as.vector(),
                     'trap.fish.matrix' = x %>% select(wild_fish, hnc_fish, hatch_fish) %>% as.matrix(),
                     'org.exist' = org_exist,
                     # 'trap.rate' = x %>% mutate(trap_rate = ifelse(!trap_open | !trap_valid | is.na(trap_fish), 0, trap_rate)) %>%
                     #   select(trap_rate) %>% as.matrix() %>% as.vector(),
                     'trap.alpha' = x %>% select(trap_alpha) %>% as.matrix() %>% as.vector(),
                     'trap.beta' = x %>% select(trap_beta) %>% as.matrix() %>% as.vector(),
                     'Tot.tags' = x %>% mutate(Tot_tags = ifelse(is.na(tot_tags_W), 0, tot_tags_W)) %>%
                       select(Tot_tags) %>% as.matrix() %>% as.vector(),
                     'ReAsc.tags' = x %>% mutate(ReAsc_tags = ifelse(reascent_tags_W > tot_tags_W, tot_tags_W, reascent_tags_W),
                                                 ReAsc_tags = ifelse(!trap_open, NA, ReAsc_tags)) %>%
                       select(ReAsc_tags) %>% as.matrix() %>% as.vector(),
                     'DC.tags' = x %>% mutate(Day_tags = ifelse(day_tags_W > tot_tags_W, tot_tags_W, day_tags_W),
                                              Day_tags = ifelse(!trap_open, NA, Day_tags)) %>%
                       select(Day_tags) %>% as.matrix() %>% as.vector()
    )

    if(sthd_B_run) data_list[['B.trap']] = x %>% select(tot_B_run_sthd) %>% as.matrix() %>% as.vector()

    if(use_historical) {
      # weighting of historical day time passage, vs observed tags
      hist_tags_all = hist_month_means %>%
        filter(Species == spp) %>%
        select(tot_fish) %>% as.matrix() %>% as.vector()
      hist_tags_all = hist_tags_all[month_vec]

      tot.fish = expand.grid(list(Month = month(1:12, label = T),
                                  Year = filter(night_passage,
                                                Species == spp,
                                                !is.na(tot_fish)) %>%
                                    select(Year) %>%
                                    distinct() %>% as.matrix() %>% as.vector())) %>%
        left_join(night_passage %>%
                    filter(Species == spp)) %>%
        select(Month, Year, tot_fish) %>%
        spread(Year, tot_fish, fill = 0) %>%
        select(-Month) %>%
        as.matrix()

      day.fish = expand.grid(list(Month = month(1:12, label = T),
                                  Year = filter(night_passage,
                                                Species == spp,
                                                !is.na(tot_fish)) %>%
                                    select(Year) %>%
                                    distinct() %>% as.matrix() %>% as.vector())) %>%
        left_join(night_passage %>%
                    filter(Species == spp)) %>%
        select(Month, Year, day_fish) %>%
        spread(Year, day_fish, fill = 0) %>%
        select(-Month) %>%
        as.matrix()

      obs_tags = mutate(x,
                        obs_tags = ifelse(is.na(tot_tags_W), 0, tot_tags_W)) %>%
        select(obs_tags) %>% as.matrix() %>% as.vector()
      hist_tags = hist_tags_all * median(obs_tags / hist_tags_all, na.rm=T)
      # calculate weighting of historic to observed tags
      rho = hist_tags / (hist_tags + obs_tags)
      rho[is.na(rho)] = 0

      data_list[['month.vec']] = month_vec
      data_list[['n.hist.yrs']] = ncol(tot.fish)
      data_list[['tot.fish']] = tot.fish
      data_list[['day.fish']] = day.fish
      data_list[['rho']] = rho
    }

    return(data_list)
  })

  return(jags_data_list)
}
