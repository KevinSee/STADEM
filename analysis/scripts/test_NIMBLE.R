# Author: Kevin See
# Purpose: test STADEM using NIMBLE
# Created: 10/30/24
# Last Modified: 11/7/24
# Notes: NIMBLE help pages: https://r-nimble.org/html_manual/cha-welcome-nimble.html

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(STADEM)
library(nimble)
library(janitor)

#-----------------------------------------------------------------
# set species and spawn year
species = c("Steelhead",
            "Chinook")[2]
yr = 2019
dam_code = c("PRD",
             "LWG")[2]

# what dates should window counts cover?
window_dates <-
  case_when(species == "Steelhead" &
              dam_code == "LWG" ~ c(paste0(yr-1, "0701"),
                                    paste0(yr, "0630")),
            species == "Steelhead" &
              dam_code == "PRD" ~ c(paste0(yr-1, "0601"),
                                    paste0(yr, "0531")),
            species == "Chinook" &
              dam_code == "LWG" ~ c(paste0(yr, "0301"),
                                    paste0(yr, "0817")),
            species == "Chinook" &
              dam_code == "PRD" ~ c(paste0(yr, "0301"),
                                    paste0(yr, "0615")))


if(dam_code == "PRD") {
  trap_dbase <- read_rds("O:Documents/Git/MyProjects/DabomPriestRapidsSthd/analysis/data/derived_data/Bio_Data_2011_2024.rds") |>
    filter(spawn_year == yr)

  if(n_distinct(trap_dbase$event_date) == 1) {
    trap_dbase <-
      trap_dbase |>
      rename(event_date_org = event_date,
             event_date = release_date)
  }

} else if(dam_code == "LWG" & species == "Chinook" & yr == 2019) {
  trap_dbase = readLGRtrapDB(system.file("extdata",
                                         "Chnk2019_TrapDatabase.csv",
                                         package = "STADEM",
                                         mustWork = TRUE))
} else {
  message("Trap data not available")
}

data_list <-
  compileData(spp = species,
              spawn_yr = yr,
              start_date = window_dates[1],
              end_date = window_dates[2],
              dam = dam_code,
              trap_dbase = trap_dbase,
              incl_trapRate = T)

#-------------------------------------------------------
# dam_pit_code = case_when(dam_code == "PRD" ~ "PRA",
#                          dam_code == "LWG" ~ "GRA")

# #--------------------------------------------------------
# # determine weekly strata
# week_strata = weeklyStrata(window_dates[1], window_dates[2])
#
# # query daily window counts
# daily_win_cnt <-
#   getWindowCounts(dam = dam_code,
#                   spp = species,
#                   start_date = window_dates[1],
#                   end_date = window_dates[2],
#                   incl_jacks = F) |>
#   mutate(window_open = if_else(!is.na(win_cnt), T, F))
#
# # For PIT tag data, summarise by spawnyear, species and day
# pit_all = queryPITtagData(damPIT = dam_pit_code,
#                           spp = species,
#                           start_date = window_dates[1],
#                           end_date = window_dates[2])
# # for Priest Rapids, no day/night counts, so consider all periods day
# if(dam_code == "PRD") {
#   pit_all <-
#     pit_all |>
#     mutate(Period = "D")
# }
#
# # summarize on daily scale
# pit_daily <- summarisePITdataDaily(pit_all)
#
# # load biological data from trap
# if(dam_code == "PRD") {
#   trap_data <-
#     read_rds("O:Documents/Git/MyProjects/DabomPriestRapidsSthd/analysis/data/derived_data/Bio_Data_2011_2024.rds") |>
#     filter(spawn_year == yr)
#
#   if(n_distinct(trap_data$event_date) == 1) {
#     trap_data <-
#       trap_data |>
#       mutate(event_date = release_date)
#   }
#
#   trap_daily <-
#     summarizeTrapDaily(trap_data,
#                        spp = species) |>
#     arrange(date) |>
#     filter(date >= int_start(week_strata[1]),
#            date <= int_end(week_strata[length(week_strata)])) |>
#     # assign week numbers to each day
#     rowwise() |>
#     mutate(week_num = which(date %within% week_strata)) |>
#     ungroup() |>
#     relocate(week_num,
#              .after = "Date")
# } else if(dam_code == "LWG" & yr == 2019 & species == "Chinook") {
#   # load compiled data
#   data("stadem_list")
#   trap_daily <-
#     summarizeTrapDaily(stadem_list$trapData,
#                        spp = species)
#   trap_daily <- summariseLGRtrapDaily(stadem_list$trapData)
#   trap_data <-
#     stadem_list$trapData |>
#     rename(pit_tag = Tag.ID,
#            species_run_rear_type = SRR,
#            event_date = CollectionDate)
# }
#
#
#
# # calculate empirical trap rate
# pit_obs <-
#   queryPITtagObs(site = dam_pit_code,
#                  spp = species,
#                  start_date = window_dates[1],
#                  end_date = window_dates[2]) |>
#   janitor::clean_names()
#
# dam_mark_site <- case_when(dam_code == "PRD" ~ "PRDLD1",
#                            dam_code == "LWG" ~ c('LGRLDR', 'LGR'),
#                            .default = NA_character_)
#
# trap_rate_df <-
#   pit_obs |>
#   filter(!mark_site %in% dam_mark_site) |>
#   mutate(obs_date = floor_date(obs_time, unit = "days")) |>
#   group_by(tag_code = tag_id,
#            sp_rrt) |>
#   summarize(across(obs_date,
#                    min),
#             .groups = "drop") |>
#   left_join(trap_data |>
#               select(tag_code = pit_tag,
#                      SRR = species_run_rear_type,
#                      trap_date = event_date) |>
#               mutate(in_trap = T),
#             by = join_by(tag_code)) |>
#   mutate(mod_date = case_when(!is.na(trap_date) ~ trap_date,
#                              is.na(trap_date) ~ obs_date,
#                              .default = NA_Date_),
#          across(in_trap,
#                 ~ replace_na(., F))) |>
#   rowwise() |>
#   mutate(week_num = which(mod_date %within% week_strata)) |>
#   ungroup() |>
#   arrange(mod_date,
#           tag_code) |>
#   group_by(week_num) |>
#   summarise(n_trap_tags = sum(in_trap),
#             n_poss_tags = n_distinct(tag_code),
#             trap_rate = n_trap_tags / n_poss_tags,
#             trap_rate_se = sqrt((trap_rate * (1 - trap_rate)) / n_poss_tags),
#             trap_rate_cv = trap_rate_se / trap_rate,
#             .groups = "drop") |>
#   left_join(tibble(week_num = 1:length(week_strata),
#                    start_date = int_start(week_strata)),
#             by = join_by(week_num)) |>
#   relocate(start_date,
#            .after = "week_num") |>
#   mutate(trap_open = if_else(n_trap_tags > 0, T, F)) %>%
#   mutate(trap_valid = case_when(n_poss_tags < 20 ~ F,
#                                 # n_trap_tags < 7 ~ F,
#                                 ! trap_open ~ F,
#                                 .default = T)) |>
#   select(start_date,
#          week_num,
#          trap_open,
#          trap_valid,
#          everything())
#
# trap_rate_df |>
#   ggplot(aes(x = start_date,
#              y = trap_rate)) +
#   geom_ribbon(aes(ymin = qnorm(0.025, trap_rate, trap_rate_se),
#                   ymax = qnorm(0.975, trap_rate, trap_rate_se)),
#               color = NA,
#               fill = "grey") +
#   geom_line() +
#   geom_point(aes(color = trap_valid,
#                  size = n_poss_tags)) +
#   theme_bw()
#
#
#
# # combine window counts, PIT tag data and trap data for daily dam data
# dam_daily <-
#   daily_win_cnt |>
#   select(-Year) |>
#   left_join(pit_daily,
#             by = join_by(Species, Date)) |>
#   left_join(trap_daily,
#             by = join_by(Date)) |>
#   tidyr::fill(SpawnYear,
#               .direction = "downup") |>
#   # assign week numbers to each day
#   rowwise() |>
#   mutate(week_num_org = which(Date %within% week_strata)) |>
#   ungroup()
#
# # summarize daily data by week
# dam_weekly <-
#   dam_daily %>%
#   filter(!is.na(week_num_org)) %>%
#   group_by(Species,
#            SpawnYear,
#            week_num_org) %>%
#   summarise(
#     across(Date,
#            min),
#     across(c(win_cnt,
#              contains("tags"),
#              trap_fish:NA.PBT),
#            ~ sum(., na.rm = T)),
#     across(window_open,
#            ~ if_else(sum(.) > 0, T, F)),
#     .groups = "drop") |>
#   rename(Start_Date = Date) |>
#   group_by(Species, SpawnYear) %>%
#   mutate(week_num = week_num_org - min(week_num_org) + 1) %>%
#   ungroup() %>%
#   select(Species:week_num_org, week_num, Start_Date, window_open, everything())
#
# # add the estimated trap rate
# dam_weekly <-
#   dam_weekly |>
#   left_join(trap_rate_df,
#             by = join_by(week_num,
#                          Start_Date == start_date)) |>
#   mutate(across(c(trap_open,
#                   trap_valid),
#                 ~ replace_na(., F)))
#
# dam_weekly |>
#   mutate(trap_est = case_when(trap_open ~ trap_fish / trap_rate,
#                               .default = NA_real_),
#          trap_est_se = sqrt(trap_rate_se^2 * (-trap_fish * trap_rate^(-2))^2),
#          trap_cv = trap_est_se / trap_est) |>
#   ggplot(aes(x = Start_Date)) +
#   geom_ribbon(aes(ymin = qnorm(0.025, trap_est, trap_est_se),
#                   ymax = qnorm(0.975, trap_est, trap_est_se)),
#               color = NA,
#               fill = "pink",
#               alpha = 0.3) +
#   geom_line(aes(y = trap_est),
#             color = "red") +
#   geom_point(aes(y = trap_est,
#                  size = n_poss_tags)) +
#   geom_line(aes(y = win_cnt),
#             color = "blue") +
#   theme_bw()
#
#
#
#
# if(hw_type == 'PBT') {
#   dam_weekly <-
#     dam_weekly %>%
#     rename(wild_fish = Wild.PBT,
#            hatch_fish = Hatch.PBT,
#            hnc_fish = HNC.PBT)
# } else if(hw_type == 'Morph') {
#   dam_weekly <-
#     dam_weekly %>%
#     rename(wild_fish = Wild.morph,
#            hatch_fish = Hatch.morph) %>%
#     mutate(hnc_fish = 0)
# }
#
#
#
#
# pit_all |>
#   filter(str_detect(ReleaseSite, "PRDLD1", negate = T)) |>
#   select(Ladder,
#          LadderSide,
#          pit_tag = TagId,
#          Date) |>
#   distinct() |>
#   mutate(in_trap = if_else(pit_tag %in% trap_data$pit_tag |
#                              pit_tag %in% trap_data$second_pit_tag,
#                            T, F)) |>
#   tabyl(LadderSide,
#         in_trap) |>
#   adorn_totals(where = "both") |>
#   adorn_percentages() |>
#   adorn_pct_formatting()
#


# what kind of hatchery / wild calls should be used?
hw_type = c("PBT", "Morph")[1]

# pull out certain pieces of data to feed into model
jags_data_list <- prepJAGS(data_list$weeklyData,
                           hw_type = hw_type)
jags_data_list <-
  list(
    'TotLadderWeeks' = nrow(dam_weekly),
    'ladder' = dam_weekly |>
      pull(window_open) |>
      as.integer(),
    'Y.window' = dam_weekly %>%
      mutate(win_cnt = ifelse(!window_open, NA, win_cnt)) %>%
      pull(win_cnt),
    # 'Y.trap' = dam_weekly %>%
    #   mutate(y_trap = trap_fish,
    #          y_trap = ifelse(!trap_open | !trap_valid, NA, y_trap)) %>%
    #   pull(y_trap),
    # 'trap.fish' = dam_weekly %>%
    #   mutate(trap_fish = wild_fish + hnc_fish + hatch_fish) %>%
    #   pull(trap_fish),
    # 'trap.fish.matrix' = dam_weekly %>%
    #   select(wild_fish, hnc_fish, hatch_fish) %>%
    #   as.matrix(),
    # 'org.exist' = dam_weekly %>%
    #   select(wild_fish, hnc_fish, hatch_fish) %>%
    #   summarize(across(everything(),
    #                    sum)) |>
    #   mutate(across(everything(),
    #                 ~ if_else(. > 0, 1, 0))) |>
    #   as.matrix() |>
    #   as.vector(),
    # 'trap.rate' = dam_weekly %>%
    #   mutate(trap_rate = ifelse(!trap_open | !trap_valid | is.na(trap_fish), 0, trap_rate)) %>%
    #   pull(trap_rate),
    # 'trap.alpha' = dam_weekly %>%
    #   pull(trap_alpha),
    # 'trap.beta' = dam_weekly %>%
    #   pull(trap_beta),
    # 'n.trap.tags' = dam_weekly %>%
    #   mutate(n_tags = if_else(trap_valid & trap_open & !is.na(n_poss_tags), n_trap_tags, NA_integer_)) %>%
    #   pull(n_tags),
    # 'n.poss.tags' = dam_weekly %>%
    #   mutate(n_poss_tags = if_else(is.na(n_poss_tags), as.integer(0), n_poss_tags)) %>%
    #   pull(n_poss_tags),
    'Tot.tags' = dam_weekly %>%
      mutate(Tot_tags = ifelse(is.na(tot_tags), 0, tot_tags)) %>%
      pull(Tot_tags),
    'ReAsc.tags' = dam_weekly %>%
      mutate(ReAsc_tags = ifelse(reascent_tags > tot_tags, tot_tags, reascent_tags),
             ReAsc_tags = ifelse(!window_open, NA, ReAsc_tags)) %>%
      pull(ReAsc_tags),
    'DC.tags' = dam_weekly %>%
      mutate(Day_tags = ifelse(day_tags > tot_tags, tot_tags, day_tags),
             Day_tags = ifelse(!window_open, NA, Day_tags)) %>%
      pull(Day_tags)
  )

#----------------------------------------------------------
# NIMBLE models
source("analysis/scripts/nimble_models.R")

mc <- mc_pois
mc <- mc_nb
mc <- mc_no_trap
mc <- mc_nb_origin
mc <- mc_origin_no_trap

#----------------------------------------------------------
# what parameters to monitor?
jags_params = c('X.tot.all',
                'X.tot.day',
                'X.tot.night',
                'X.tot.reasc',
                'X.tot.new.all',
                'X.tot.new.wild',
                'X.tot.new.hatch',
                'X.tot.new.hnc',
                'X.tot.night.wild',
                'X.tot.reasc.wild',
                'reasc.true',
                'day.true',
                'trap.rate.true',
                'X.sigma',
                'day.sigma',
                'reasc.sigma',
                'r', 'k',
                'org.sigma',
                'org.prop',
                'prop.tagged',
                'X.all')

# determine if some parameters should be dropped because they don't appear in model code
keep_params <-
  map_lgl(jags_params,
          .f = function(x) {
            keep <- sum(str_detect(as.character(mc), x)) > 0
            if(x %in% c("r", "k") & sum(str_detect(as.character(mc), "dnegbin")) == 0) {
              keep = FALSE
            }
            return(keep)
          })

# jags_params[!keep_params]
jags_params <- jags_params[keep_params]


# set some constants
nimble_constants <-
  jags_data_list[c(which(str_detect(names(jags_data_list), "TotLadderWeeks")),
                   which(str_detect(names(jags_data_list), "org.exist")))]

# set the data to be fed to NIMBLE
nimble_data <-
  jags_data_list[-c(which(str_detect(names(jags_data_list), "TotLadderWeeks")),
                    which(str_detect(names(jags_data_list), "org.exist")))]

# # build, compile and run NIMBLE model step by step
# nim_mod <-
#   nimbleModel(mc,
#               data = nimble_data,
#               constants = nimble_constants,
#               inits = jagsInits(nimble_data,
#                                 params = "X.log.all"))
# nim_mod$initializeInfo()
# nim_mod$getNodeNames()
# nim_mod$plotGraph()
#
#
# comp_mod <-
#   compileNimble(nim_mod)
#
# nim_config <-
#   configureMCMC(nim_mod,
#                 print = T)
# nim_config$addMonitors(jags_params)
#
# nim_mcmc <-
#   buildMCMC(nim_config)
# comp_mcmc <-
#   compileNimble(nim_mcmc,
#                 resetFunctions = TRUE)
#
# mcmc_samp <-
#   runMCMC(comp_mcmc,
#           nchains = 4,
#           nburnin = 5000,
#           niter = 10000,
#           thin = 10,
#           progressBar = TRUE,
#           samples = T,
#           samplesAsCodaMCMC = T,
#           summary = T)
#
# rm(nim_mod,
#    comp_mod,
#    nim_config,
#    nim_mcmc,
#    comp_mcmc)

# compile and run NIMBLE model in one step
mcmc_samp <-
  nimbleMCMC(code = mc,
             constants = nimble_constants,
             data = nimble_data,
             inits = vector('list', 4) |>
               purrr::map(.f = function(x) {
                 jagsInits(nimble_data)
               }),
             monitors = jags_params,
             nchains = 4,
             nburnin = 5000,
             niter = 10000,
             thin = 10,
             setSeed = T,
             progressBar = TRUE,
             samples = T,
             samplesAsCodaMCMC = T,
             summary = T)

mcmc_samp$summary$all.chains |>
  as_tibble(rownames = "param")

mcmc_samp$summary$all.chains |>
  as_tibble(rownames = "param") |>
  filter(str_detect(param, "sigma"))

dam_weekly = data_list$weeklyData

mcmc_samp$summary$all.chains |>
  as_tibble(rownames = "param") |>
  filter(str_detect(param, "\\[", negate = T))

mcmc_samp$summary$all.chains |>
  as_tibble(rownames = "param_org") |>
  clean_names() |>
  rename(sd = st_dev,
         lci = x95_percent_ci_low,
         uci = x95_percent_ci_upp) |>
  filter(str_detect(param_org, "\\[", negate = F)) |>
  mutate(week = as.integer(str_extract(param_org, "[0-9]+")),
         param = str_split_i(param_org, "\\[", i = 1)) |>
  select(-param_org) |>
  relocate(param,
           week,
           .before = 0) |>
  filter(str_detect(param, "X.all")) |>
  left_join(dam_weekly |>
              mutate(trap_est = case_when(trap_open & trap_rate > 0 ~ trap_fish / trap_rate,
                                          .default = NA_real_),
                     trap_est_se = sqrt(trap_rate_se^2 * (-trap_fish * trap_rate^(-2))^2)) |>
              mutate(adj_win_cnt = case_when(tot_tags > 0 & day_tags > 0 ~ win_cnt / (day_tags / tot_tags),
                                              .default = win_cnt)),
            by = join_by(week == week_num)) |>
  ggplot(aes(x = start_date)) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci),
              fill = "gray") +
  geom_line(aes(y = mean)) +
  geom_point(aes(y = mean),
             size = 2) +
  geom_point(aes(y = win_cnt),
             size = 4,
             pch = 19,
             color = "lightblue") +
  geom_point(aes(y = adj_win_cnt),
             size = 4,
             pch = 19,
             color = "blue") +
  # geom_errorbar(aes(ymin = qnorm(0.025, trap_est, trap_est_se),
  #                ymax = qnorm(0.975, trap_est, trap_est_se)),
  #            color = "red",
  #            width = 0) +
  geom_point(aes(y = trap_est),
             size = 4,
             pch = 19,
             color = "red") +
  # coord_cartesian(ylim = c(0, 2500)) +
  theme_bw()

mcmc_samp$summary$all.chains |>
  as_tibble(rownames = "param_org") |>
  clean_names() |>
  rename(sd = st_dev,
         lci = x95_percent_ci_low,
         uci = x95_percent_ci_upp) |>
  filter(str_detect(param_org, "\\[", negate = F)) |>
  mutate(week = as.integer(str_extract(param_org, "[0-9]+")),
         param = str_split_i(param_org, "\\[", i = 1)) |>
  select(-param_org) |>
  relocate(param,
           week,
           .before = 0) |>
  filter(str_detect(param, "day.true") |
           str_detect(param, "reasc.true")) |>
  mutate(across(param,
                ~ str_remove(., "\\.true"))) |>
  left_join(dam_weekly,
            by = join_by(week == week_num)) |>
  ggplot(aes(x = start_date,
             y = median,
             color = param,
             fill = param)) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci),
              color = NA,
              alpha = 0.2) +
  geom_line() +
  # geom_point(aes(size = tot_tags)) +
  geom_point(aes(y = day_tags / tot_tags,
                 size = tot_tags,
                 color = 'day'),
             shape = 1) +
  geom_point(aes(y = reascent_tags / tot_tags,
                 size = tot_tags,
                 color = 'reasc'),
             shape = 1) +
  scale_color_brewer(palette = 'Set1',
                     name = 'Rate',
                     labels = c('day' = 'Daytime Passage',
                                'reasc' = 'Reascension')) +
  scale_fill_brewer(palette = 'Set1',
                    name = 'Rate',
                    labels = c('day' = 'Daytime Passage',
                               'reasc' = 'Reascension')) +
  labs(x = "Date",
       y = "Estimate",
       size = "Total Tags") +
  theme_bw()


mcmc_samp$summary$all.chains |>
  as_tibble(rownames = "param_org") |>
  clean_names() |>
  rename(sd = st_dev,
         lci = x95_percent_ci_low,
         uci = x95_percent_ci_upp) |>
  filter(str_detect(param_org, "\\[", negate = F)) |>
  mutate(week = as.integer(str_extract(param_org, "[0-9]+")),
         param = str_split_i(param_org, "\\[", i = 1),
         origin = str_split_i(param_org, "\\,", i = 2),
         across(origin,
                ~ str_remove(., "\\]")),
         across(origin,
                str_squish),
         across(origin,
                ~ as.factor(as.integer(.)))) |>
  # select(-param_org) |>
  relocate(param,
           origin,
           week,
           .before = 0) |>
  filter(str_detect(param, "org.prop")) |>
  left_join(dam_weekly,
            by = join_by(week == week_num)) |>
  ggplot(aes(x = start_date,
             y = median,
             color = origin,
             fill = origin)) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci),
              color = NA,
              alpha = 0.2) +
  geom_line() +
  geom_point(aes(size = trap_fish)) +
  scale_color_brewer(palette = 'Set1',
                     name = 'Origin',
                     labels = c('1' = 'NOR',
                                '2' = 'HNC',
                                '3' = "HOR")) +
  scale_fill_brewer(palette = 'Set1',
                    name = 'Origin',
                    labels = c('1' = 'NOR',
                               '2' = 'HNC',
                               '3' = "HOR")) +
  labs(x = "Date",
       y = "Estimate",
       size = "Fish in Trap") +
  theme_bw()



model_file_nm = tempfile('STADEM_JAGS_model', fileext = ".txt")

# what distribution to use for window counts?
win_model = c('pois', 'neg_bin', 'neg_bin2', 'quasi_pois', 'log_space')[2]

#-----------------------------------------------------------------
# run STADEM model
#-----------------------------------------------------------------
stadem_mod = runSTADEMmodel(file_name = model_file_nm,
                            mcmc_chainLength = 40000,
                            mcmc_burn = 10000,
                            mcmc_thin = 30,
                            mcmc_chains = 4,
                            jags_data = jags_data_list,
                            seed = 5,
                            weekly_params = T,
                            win_model = win_model,
                            trap_est = T)

plotTimeSeries(stadem_mod,
               dam_weekly |>
                 rename(Start_Date = start_date) |>
                 mutate(trap_est = case_when(trap_open ~ trap_fish / trap_rate,
                                             .default = NA_real_),
                        trap_est_se = sqrt(trap_rate_se^2 * (-trap_fish * trap_rate^(-2))^2))) +
  theme_bw()

summ <- summary(stadem_mod)
stadem_plot <-
  summ$statistics |>
  as_tibble(rownames = "param_org") |>
  left_join(summ$quantiles |>
              as_tibble(rownames = "param_org"),
            by = join_by(param_org)) |>
  clean_names() |>
  rename(median = x50_percent,
         lci = x2_5_percent,
         uci = x97_5_percent) |>
  select(-c(ends_with("_se"),
            ends_with("_percent"))) |>
  relocate(median,
           .after = "mean")

stadem_plot |>
  filter(str_detect(param_org, "\\[", negate = T))

stadem_plot |>
  filter(str_detect(param_org, "\\[", negate = F)) |>
  mutate(week = as.integer(str_extract(param_org, "[0-9]+")),
         param = str_split_i(param_org, "\\[", i = 1)) |>
  select(-param_org) |>
  relocate(param,
           week,
           .before = 0) |>
  filter(str_detect(param, "day.true") |
           str_detect(param, "reasc.true")) |>
  mutate(across(param,
                ~ str_remove(., "\\.true"))) |>
  left_join(dam_weekly,
            by = join_by(week == week_num)) |>
  ggplot(aes(x = start_date,
             y = median,
             color = param,
             fill = param)) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci),
              color = NA,
              alpha = 0.2) +
  geom_line() +
  # geom_point(aes(size = tot_tags)) +
  geom_point(aes(y = day_tags / tot_tags,
                 size = tot_tags,
                 color = 'day'),
             shape = 1) +
  geom_point(aes(y = reascent_tags / tot_tags,
                 size = tot_tags,
                 color = 'reasc'),
             shape = 1) +
  scale_color_brewer(palette = 'Set1',
                     name = 'Rate',
                     labels = c('day' = 'Daytime Passage',
                                'reasc' = 'Reascension')) +
  scale_fill_brewer(palette = 'Set1',
                    name = 'Rate',
                    labels = c('day' = 'Daytime Passage',
                               'reasc' = 'Reascension')) +
  labs(x = "Date",
       y = "Estimate",
       size = "Total Tags") +
  theme_bw()
