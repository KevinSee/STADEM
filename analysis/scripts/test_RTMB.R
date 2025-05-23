# Author: Kevin See
# Purpose: test STADEM using NIMBLE
# Created: 4/17/25
# Last Modified: 4/21/25
# Notes: RTMB intro: https://cloud.r-project.org/web/packages/RTMB/vignettes/RTMB-introduction.html

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
# library(STADEM)
devtools::load_all()
library(RTMB)
library(janitor)

theme_set(theme_bw())

#-----------------------------------------------------------------
# define some functions
inv_logit <- function(x) {
  1 / (1 + exp(-x))
}

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
# trap data
if(dam_code == "PRD") {
  if(species == "Steelhead") {
    trap_dbase <- read_rds("O:Documents/Git/MyProjects/DabomPriestRapidsSthd/analysis/data/derived_data/Bio_Data_2011_2024.rds") |>
      filter(spawn_year == yr)
  } else if(species == "Chinook") {
    trap_dbase <- read_rds("O:Documents/Git/MyProjects/DabomPriestRapidsSpChnk/analysis/data/derived_data/Bio_Data_2021_2024.rds") |>
      filter(spawn_year == yr) |>
      mutate(trap_yr = spawn_year)
  }

  if(nrow(trap_dbase) == 0) {
    message("Trap data not available")
    break()
  }

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
# what kind of hatchery / wild calls should be used?
hw_type = c("PBT", "Morph")[1]

# pull out certain pieces of data to feed into model
jags_data_list <- prepJAGS(data_list$weeklyData,
                           hw_type = hw_type) |>
  clean_names() |>
  rlang::set_names(nm = function(x) {
    str_replace(x, "^y_", "Y_") |>
      str_replace("^re_asc_", "reasc_") |>
      str_replace("^dc_", "day_")
  })

jags_data_list$Y_window

# intersect(names(jags_data_list), names(mod_data))
# setdiff(names(jags_data_list), names(mod_data))
# setdiff(names(mod_data), names(jags_data_list))

#----------------------------------------------------------
# NIMBLE models
source("analysis/scripts/rtmb_models.R")

f <- obj_pois
f <- obj_nb
f <- obj_no_trap
# f <- obj_nb_origin


#-------------------------------------------------------
mod_data <-
  prepRTMB(data_list$weeklyData,
           hw_type = hw_type) |>
  mutate(obs_eta = reasc_tags / tot_tags,
         obs_theta = day_tags / tot_tags,
         obs_trap_rate = n_trap_tags / n_poss_tags)



parameters <-
  list(
    X_1 = -1,                # initial state of X
    sdX_ln = 3,              # log of process error SD of abundance (in log space)
    eps_X = rep(0, length(mod_data$week_num) - 1),   # process errors of X in log space

    eta_1 = 0,               # initial eta (re-ascension rate in logit space)
    sdEta_ln = 3,            # log of process error SD of eta (in logit space)
    eps_eta = rep(0, length(mod_data$week_num) - 1),       # process errors of re-ascension in logit space

    theta_1 = 9,             # initial eta (daytime passage rate in logit space)
    sdTheta_ln = 3,          # log of process error SD of theta (in logit space)
    eps_theta = rep(0, length(mod_data$week_num) - 1)  ,     # process errors of daytime passage in logit space

    ln_r = 5,               # over-dispersion parameter for negative binomial distribution

    mean_nu = -1,
    sdNu_ln = 2,
    eps_nu = rep(0, length(mod_data$week_num))
    # trap_rate_logit = logit(0.2)


    # phi_1 = c(1, 1, 1),
    # sdPhi = 2,
    # eps_phi = matrix(0,
    #                  ncol = 3,
    #                  nrow = max(mod_data$week_num) - 1),
    # # phi = matrix(0,
    # #              nrow = 3,
    # #              ncol = max(mod_data$week_num)),
    # omega = jags_data_list$trap_fish_matrix / jags_data_list$trap_fish,

  )


rm(obj, opt, sdr)

obj <- MakeADFun(f,
                 parameters,
                 # silent = F,
                 random = c("eps_X",
                            "eps_eta",
                            "eps_theta",
                            "eps_nu"),
                 map = list(#eps_theta = as.factor(rep(NA,length(parameters$eps_eta))),
                            # sdTheta_ln = as.factor(NA),
                            # theta_1 = as.factor(NA))#,
                            # eta_1 = as.factor(NA),
                            eps_nu = as.factor(rep(NA, length(parameters$eps_nu))),
                            mean_nu = as.factor(NA),
                            sdNu_ln = as.factor(NA),
                            ln_r = as.factor(NA))
                 )
obj$gr()


opt <- nlminb(start = obj$par,
              objective = obj$fn,
              gradient = obj$gr)

opt$convergence
opt$message

sdr <- sdreport(obj)
sdr
summary(sdr) |>
  as_tibble(rownames = "param") |>
  filter(str_detect(param, "^sd", negate = F),
         str_detect(param, "_", negate = T))

summary(sdr) |>
  as_tibble(rownames = "param") |>
  filter(param %in% c("ln_r",
                      "r"))

summary(sdr) |>
  as_tibble(rownames = "param") |>
  clean_names() |>
  filter(str_detect(param, "tot"))

# as.list(sdr, "Est", report=TRUE) ## ADREPORT estimates
# as.list(sdr, "Std", report=TRUE) ## ADREPORT uncertainties

# as.list(sdr, "Est", report=TRUE) |>
#   pluck("X_tot")
#
# as.list(sdr, "Est", report=TRUE) |>
#   pluck("X_tot_day")

sum(mod_data$Y_window)


summary(sdr) |>
  as_tibble(rownames = "param") |>
  clean_names() |>
  filter(str_detect(param, "^eps")) |>
  ggplot(aes(x = estimate)) +
  geom_histogram() +
  geom_vline(xintercept = 0,
             color = "red",
             linetype = 2) +
  theme_bw() +
  facet_wrap(~ param)

summary(sdr) |>
  as_tibble(rownames = "param") |>
  clean_names() |>
  filter(param == "X_D") |>
  mutate(lci = qnorm(0.025, estimate, std_error),
         uci = qnorm(0.975, estimate, std_error)) |>
  bind_cols(mod_data) |>
  # mutate(week_num = 1:n()) |>
  ggplot(aes(x = week_num,
             y = estimate)) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci),
              color = NA,
              fill = "gray") +
  geom_point() +
  geom_line() +
  geom_point(aes(y = Y_window),
             color = "red",
             shape = 1,
             size = 4) +
  theme_bw() +
  labs(x = "Week Num",
       y = "Daytime Fish")

summary(sdr) |>
  as_tibble(rownames = "param") |>
  clean_names() |>
  filter(param %in% c("X",
                      "X_D",
                      "X_uniq",
                      "X_trap")) |>
  mutate(lci = qnorm(0.025, estimate, std_error),
         uci = qnorm(0.975, estimate, std_error)) |>
  mutate(week_num = rep(mod_data$week_num,
                        n_distinct(param))) |>
  left_join(mod_data,
            by = join_by(week_num)) |>
  ggplot(aes(x = week_num,
             y = estimate,
             color = param,
             fill = param)) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci),
              color = NA,
              alpha = 0.2) +
  geom_point(size = 3) +
  geom_line() +
  geom_point(aes(y = Y_window),
             color = "black",
             shape = 1,
             size = 4,
             show.legend = F) +
  # scale_y_continuous(trans = "log",
  #                    breaks = scales::breaks_pretty()) +
  theme_bw()

summary(sdr) |>
  as_tibble(rownames = "param") |>
  clean_names() |>
  filter(param %in% c("X",
                      "X_D",
                      "X_uniq",
                      "X_trap")) |>
  mutate(lci = qnorm(0.025, estimate, std_error),
         uci = qnorm(0.975, estimate, std_error)) |>
  mutate(week_num = rep(mod_data$week_num,
                        n_distinct(param))) |>
  left_join(mod_data,
            by = join_by(week_num)) |>
  filter(Y_window > 0) |>
  ggplot(aes(x = Y_window,
             y = estimate)) +
  geom_abline(linetype = 2,
              color = "darkgray") +
  geom_errorbar(aes(ymin = lci,
                    ymax = uci),
                width = 0) +
  geom_point() +
  facet_wrap(~ param) +
  scale_x_continuous(transform = "log",
                     breaks = scales::breaks_pretty()) +
  scale_y_continuous(transform = "log",
                     breaks = scales::breaks_pretty()) +
  labs(x = "Window Count",
       y = "Estimate")




summary(sdr) |>
  as_tibble(rownames = "param") |>
  clean_names() |>
  filter(param == "trap_rate") |>
  mutate(across(estimate,
                ~ if_else(. < 0, 0, .))) |>
  mutate(lci = qnorm(0.025, estimate, std_error),
         uci = qnorm(0.975, estimate, std_error),
         across(lci,
                ~ case_when(. < 0 ~ 0,
                            .default = .)),
         across(uci,
                ~ case_when(. > 1 ~ 1,
                            .default = .))) |>
  mutate(week_num = rep(mod_data$week_num,
                        n_distinct(param))) |>
  left_join(mod_data |>
              mutate(obs_nu_sd = sqrt((obs_trap_rate * (1 - obs_trap_rate)) / n_poss_tags),
                     nu_lci = qnorm(0.025, obs_trap_rate, obs_nu_sd),
                     nu_uci = qnorm(0.975, obs_trap_rate, obs_nu_sd),
                     across(nu_lci,
                            ~ case_when(. < 0 ~ 0,
                                        .default = .)),
                     across(nu_uci,
                            ~ case_when(. > 1 ~ 1,
                                        .default = .))),
            by = join_by(week_num)) |>
  ggplot(aes(x = week_num,
             y = estimate)) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci),
              color = NA,
              fill = "gray",
              alpha = 0.5) +
  geom_errorbar(aes(ymin = nu_lci,
                    ymax = nu_uci),
                width = 0,
                color = "red") +
  geom_point(size = 2) +
  geom_line() +
  geom_point(aes(y = obs_trap_rate,
                 size = n_poss_tags),
             color = "red",
             shape = 1) +
  # geom_line(aes(y = obs_trap_rate),
  #           color = "red") +
  theme_bw() +
  labs(x = "Week Number",
       y = "Trap Rate",
       size = "Possible Tags")

summary(sdr) |>
  as_tibble(rownames = "param") |>
  clean_names() |>
  filter(param == "eta") |>
  mutate(lci = qnorm(0.025, estimate, std_error),
         uci = qnorm(0.975, estimate, std_error)) |>
  mutate(week_num = rep(mod_data$week_num,
                        n_distinct(param))) |>
  left_join(mod_data,
            by = join_by(week_num)) |>
  ggplot(aes(x = week_num,
             y = estimate)) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci),
              color = NA,
              fill = "gray",
              alpha = 0.5) +
  geom_point() +
  geom_line() +
  geom_point(aes(y = obs_eta,
                 size = tot_tags),
             color = "red",
             shape = 1,
             size = 4) +
  theme_bw() +
  # scale_y_continuous(trans = "logit") +
  labs(title = "Re-Ascension Rate")

summary(sdr) |>
  as_tibble(rownames = "param") |>
  clean_names() |>
  filter(param == "theta") |>
  mutate(lci = qnorm(0.025, estimate, std_error),
         uci = qnorm(0.975, estimate, std_error)) |>
  mutate(week_num = rep(mod_data$week_num,
                        n_distinct(param))) |>
  left_join(mod_data,
            by = join_by(week_num)) |>
  ggplot(aes(x = week_num,
             y = estimate)) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci),
              color = NA,
              fill = "gray",
              alpha = 0.5) +
  geom_point() +
  geom_line() +
  geom_point(aes(y = obs_theta,
                 size = tot_tags),
             color = "red",
             shape = 1,
             size = 4) +
  theme_bw() +
  labs(title = "Daytime Passage Rate")



summary(sdr) |>
  as_tibble(rownames = "param") |>
  clean_names() |>
  filter(param %in% c("eta",
                      "theta")) |>
  mutate(lci = qnorm(0.025, estimate, std_error),
         uci = qnorm(0.975, estimate, std_error),
         across(lci,
                ~ case_when(. < 0 ~ 0,
                            .default = .)),
         across(uci,
                ~ case_when(. > 1 ~ 1,
                            .default = .))) |>
  mutate(week_num = rep(mod_data$week_num,
                        n_distinct(param))) |>
  left_join(mod_data |>
              select(week_num,
                     tot_tags,
                     starts_with("obs")) |>
              pivot_longer(starts_with("obs"),
                           names_to = "param",
                           values_to = "obs") |>
              mutate(across(param,
                            ~ str_remove(., "^obs_"))),
            by = join_by(param,
                         week_num)) |>
  mutate(across(param,
                ~ case_match(.,
                             "eta" ~ "Re-Asc.",
                             "theta" ~ "Day",
                             .default = .))) |>
  ggplot(aes(x = week_num,
             y = estimate,
             color = param,
             fill = param)) +
  geom_ribbon(aes(ymin = lci,
                  ymax = uci),
              color = NA,
              # fill = "gray",
              alpha = 0.3) +
  geom_point(aes(y = obs,
                 size = tot_tags)) +
  # geom_line(aes(y = obs)) +
  # geom_point() +
  geom_line() +
  theme_bw() +
  # scale_y_continuous(trans = "logit") +
  # facet_wrap(~ param,
  #            scales = "free_y") +
  labs(x = "Week Number",
       y = "Rate",
       size = "Obs. Tags")

names(obj$env$obs)

osa <- oneStepPredict(obj,
                      observation.name = names(obj$env$obs)[1],
                      # method="fullGaussian",
                      # discrete=FALSE)
                      method = "cdf",
                      discrete = T)
qqnorm(osa$res); abline(0,1)







parameters <-
  list(
    phi_1 = c(0, 0),
    phi_ref = rep(0, max(mod_data$week_num)),
    sdPhi = 2,
    eps_phi = matrix(0,
                     ncol = 3,
                     nrow = max(mod_data$week_num) - 1),
    phi = matrix(0,
                 nrow = 3,
                 ncol = max(mod_data$week_num))
    # omega = jags_data_list$trap_fish_matrix / jags_data_list$trap_fish,
  )

f <- function(params) {
  getAll(mod_data,
         params)
  ## Initialize joint negative log likelihood
  nll <- 0
  # proportion of different origins
  # multivariate normal process errors for origin props
  nll <-
    nll - sum(dmvnorm(eps_phi,
                      mu = c(0, 0),
                      Sigma = diag(rep(sdPhi, 2)^2),
                      log = T))

  # phi vectors
  for(j in 1:2) {
    phi[,j] <- phi_1[j] + cumsum(c(0, eps_phi[,j]))
  }

  # phi_exp <- exp(phi)
  for(i in 1:nrow(phi_exp)) {
    omega[i,] <- c(exp(phi[i,]) / (exp(phi_ref[i]) + sum(exp(phi[i,]))),
                   exp(phi_ref[i]) / (exp(phi_ref[i]) + sum(exp(phi[i,]))))

    # omega_ll[i] <- if_else(trap_fish[i] > 0,
    #                        dmultinom(x = c(wild_fish[i],
    #                                        hnc_fish[i],
    #                                        hatch_fish[i]),
    #                                  prob = omega[i,],
    #                                  log = T),
    #                        0)
    omega_ll[i] <- dmultinom(x = c(wild_fish[i],
                                   hnc_fish[i],
                                   hatch_fish[i]),
                             prob = omega[i,],
                             log = T)
  }

  nll <-
    nll - sum(omega_ll)

  ADREPORT(omega)
  ## Return
  nll
}

rm(obj, opt, sdr)

obj <- MakeADFun(f,
                 parameters,
                 # silent = F,
                 random = c("eps_phi")
                 # random = c("eps_X",
                 #            "eps_eta",
                 #            "eps_theta")
)

opt <- nlminb(start = obj$par,
              objective = obj$fn,
              gradient = obj$gr)

sdr <- sdreport(obj)
sdr


summary(sdr) |>
  as_tibble(rownames = "param") |>
  clean_names() |>
  filter(str_detect(param, "omega"))


mod_data2 <-
  mod_data |>
  summarize(across(c(trap_fish,
                     wild_fish,
                     hnc_fish,
                     hatch_fish),
                   sum))

parameters <-
  list(
    phi1 = 0,
    phi2 = 0,
    phi3 = 0
  )

gmmap <-
  list(
    phi3 = factor(NA)
  )

f <- function(params) {
  getAll(mod_data2,
         params)
  ## Initialize joint negative log likelihood
  nll <- 0
  # proportion of different origins
  omega <- c(exp(phi1) / (exp(phi1) + exp(phi2) + exp(phi3)),
             exp(phi2) / (exp(phi1) + exp(phi2) + exp(phi3)),
             exp(phi3) / (exp(phi1) + exp(phi2) + exp(phi3)))

  nll <-
    nll - dmultinom(x = c(wild_fish,
                          hnc_fish,
                          hatch_fish),
                    prob = omega,
                    log = T)

  ADREPORT(omega)
  ## Return
  nll
}

parameters <-
  list(
    phi = c(0, 0),
    phi_ref = 0
  )

gmmap <-
  list(
    phi_ref = factor(NA)
  )

f <- function(params) {
  getAll(mod_data2,
         params)
  ## Initialize joint negative log likelihood
  nll <- 0
  # proportion of different origins
  omega <- c(exp(phi) / (exp(phi_ref) + sum(exp(phi))),
             exp(phi_ref) / (exp(phi_ref) + sum(exp(phi))))

  nll <-
    nll - dmultinom(x = c(wild_fish,
                          hnc_fish,
                          hatch_fish),
                    prob = omega,
                    log = T)

  ADREPORT(omega)
  ## Return
  nll
}


rm(obj, opt, sdr)

obj <- MakeADFun(f,
                 parameters,
                 map = gmmap)

opt <- nlminb(start = obj$par,
              objective = obj$fn,
              gradient = obj$gr)

sdr <- sdreport(obj)
sdr
summary(sdr) |>
  as_tibble(rownames = "param") |>
  clean_names() |>
  filter(str_detect(param, "omega"))
