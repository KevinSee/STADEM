#' @title Compile RTMB data
#'
#' @description Compile data into format for the RTMB total escapement model
#'
#' @author Kevin See
#'
#' @inheritParams prepJAGS
#' @import dplyr
#' @export
#' @return NULL
#'
prepRTMB = function(dam_weekly = NULL,
                    sthd_B_run = F,
                    hw_type = c('PBT', 'Morph'),
                    wild_tags = F) {

  stopifnot(!is.null(dam_weekly))

  hw_type = match.arg(hw_type)

  message('Prepping data for RTMB \n')

  if(hw_type == 'PBT') {
    dam_weekly <-
      dam_weekly %>%
      rename(wild_fish = Wild.PBT,
             hatch_fish = Hatch.PBT,
             hnc_fish = HNC.PBT)
  } else if(hw_type == 'Morph') {
    dam_weekly = dam_weekly %>%
      rename(wild_fish = Wild.morph,
             hatch_fish = Hatch.morph) %>%
      mutate(hnc_fish = 0)
  }

  # org_exist = dam_weekly %>%
  #   select(wild_fish, hnc_fish, hatch_fish) %>%
  #   colSums()
  # org_exist = ifelse(org_exist > 0, 1, org_exist)

  mod_data <-
    dam_weekly |>
    # is ladder open? 1 if yes, 0 if no
    mutate(ladder = case_when(window_open | trap_open ~ 1,
                              .default = 0),
           Y_window = case_when(!window_open ~ NA_real_,
                                .default = win_cnt),
           # set any negative window counts to 0
           across(Y_window,
                  ~ case_when(. < 0 ~ 0,
                              .default = .)),
           Y_trap = case_when(!trap_open | !trap_valid ~ NA_real_,
                              .default = trap_fish),
           trap_fish = wild_fish + hnc_fish + hatch_fish,
           n_trap_tags = case_when(trap_valid & trap_open & !is.na(n_poss_tags) ~ n_trap_tags,
                                   .default = NA_integer_),
           n_poss_tags = case_when(is.na(n_poss_tags) ~ as.integer(0),
                                   .default = n_poss_tags),
           tot_tags = case_when(is.na(tot_tags) ~ 0,
                                .default = tot_tags),
           reasc_tags = case_when(reascent_tags > tot_tags & ladder ~ tot_tags,
                                  !ladder ~ NA_real_,
                                  .default = reascent_tags),
           day_tags = case_when(day_tags > tot_tags & ladder ~ tot_tags,
                                !ladder ~ NA_real_,
                                .default = day_tags)) |>
    select(week_num,
           ladder,
           Y_window,
           Y_trap,
           trap_fish,
           wild_fish,
           hnc_fish,
           hatch_fish,
           n_trap_tags,
           n_poss_tags,
           tot_tags,
           reasc_tags,
           day_tags) |>
    mutate(across(c(Y_window,
                    Y_trap,
                    n_trap_tags),
                  ~ replace_na(., 0)))


  if(wild_tags) {
    wild_data <-
      dam_weekly |>
      mutate(tot_tags = case_when(is.na(tot_tags_W) ~ 0,
                                  .default = tot_tags_W),
             reasc_tags = case_when(reascent_tags_W > tot_tags_W & ladder ~ tot_tags_W,
                                    !ladder ~ NA_real_,
                                    .default = reascent_tags_W),
             day_tags = case_when(day_tags_W > tot_tags_W & ladder ~ tot_tags_W,
                                  !ladder ~ NA_real_,
                                  .default = day_tags_W))
    mod_data <-
      mod_data |>
      select(tot_tags,
             reasc_tags,
             day_tags) |>
      bind_cols(wild_data) |>
      select(all_of(names(mod_data)))
  }


  if(sthd_B_run) {
    mod_data <-
      mod_data |>
      mutate(B_trap = dam_weekly$tot_B_run_sthd)
  }

  mod_data <-
    mod_data |>
    mutate(across(Y_window:day_tags,
                  ~ case_when(ladder == 0 ~ 0,
                              .default = .))) |>
    filter(ladder == 1)

  return(mod_data)
}
