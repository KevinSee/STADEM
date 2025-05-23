#----------------------------------------------------------
# RTMB models
library(RTMB)

obj_pois <-
  function(params) {

    getAll(mod_data,
           params)

    # ## Optional (enables extra RTMB features)
    # Y_window <- OBS(Y_window)

    ## Initialize joint negative log likelihood
    nll <- 0
    # random walk of X_D
    nll <-
      nll - sum(dnorm(eps_X,
                      mean = 0,
                      sd = exp(sdX_ln),
                      log = T))

    # what is true X_D
    X_log <- X_1 + cumsum(c(0, eps_X))
    # transform back out of log space
    X <- exp(X_log)

    # re-ascension rate
    # random walk of re_ascension rate
    nll <-
      nll - sum(dnorm(eps_eta,
                      mean = 0,
                      sd = exp(sdEta_ln),
                      log = T))

    # true eta in logit space
    eta_logit <- eta_1 + cumsum(c(0, eps_eta))
    # transform out of logit space
    eta <- inv_logit(eta_logit)

    # daytime passage rate
    # random walk of re_ascension rate
    nll <-
      nll - sum(dnorm(eps_theta,
                      mean = 0,
                      sd = exp(sdTheta_ln),
                      log = T))

    # true theta in logit space
    theta_logit <- theta_1 + cumsum(c(0, eps_theta))
    # transform out of logit space
    theta <- inv_logit(theta_logit)
    # fish passing during the day
    X_D <- X * theta

    ## Data
    # window counts
    # Using Poisson (no over-dispersion)
    nll <-
      nll - sum(dpois(x = Y_window,
                      lambda = X_D,
                      log = T))

    # estimating the trap rate
    nll <-
      nll - sum(dnorm(eps_nu,
                      mean = 0,
                      sd = exp(sdNu_ln),
                      log = T))
    trap_rate <- inv_logit(mean_nu + eps_nu)

    # using tags detected ascending ladder, and tags detected in trap (multiple species)
    nll <-
      nll - sum(dbinom(x = n_trap_tags,
                       size = n_poss_tags,
                       prob = trap_rate,
                       log = T))

    # estimate abundance using trap counts (species specific)
    # Poisson trick, but no constraints
    nll <-
      nll - sum(dpois(x = Y_trap,
                      lambda = X * trap_rate,
                      log = T))

    # re-ascension tags
    nll <-
      nll - sum(dbinom(x = reasc_tags,
                       size = tot_tags,
                       prob = eta,
                       log = T))

    # daytime tags
    nll <-
      nll - sum(dbinom(x = day_tags,
                       size = tot_tags,
                       prob = theta,
                       log = T))


    ## calculate some outputs
    # standard errors
    sdX <- exp(sdX_ln)
    sdEta <- exp(sdEta_ln)
    sdTheta <- exp(sdTheta_ln)
    sdNu <- exp(sdNu_ln)
    # unique fish
    X_uniq <- X * (1 - eta)
    # estimate of total fish from trap
    X_trap <- Y_trap / trap_rate
    # calculate totals for the season
    X_tot <- sum(X)
    X_tot_day <- sum(X_D)
    X_tot_uniq <- sum(X_uniq)
    X_tot_trap <- sum(X_trap)

    # get predicted daytime fish
    ADREPORT(X)
    ADREPORT(X_D)
    ADREPORT(X_uniq)
    ADREPORT(X_trap)
    ADREPORT(X_tot)
    ADREPORT(X_tot_day)
    ADREPORT(X_tot_uniq)
    ADREPORT(X_tot_trap)
    ADREPORT(eta)
    ADREPORT(theta)
    ADREPORT(sdX)
    ADREPORT(sdEta)
    ADREPORT(sdTheta)
    ADREPORT(sdNu)
    ADREPORT(trap_rate)

    ## Return
    nll
  }

obj_no_trap <-
  function(params) {

    getAll(mod_data,
           params)

    # ## Optional (enables extra RTMB features)
    # Y_window <- OBS(Y_window)

    ## Initialize joint negative log likelihood
    nll <- 0
    # random walk of X_D
    nll <-
      nll - sum(dnorm(eps_X,
                      mean = 0,
                      sd = exp(sdX_ln),
                      log = T))

    # what is true X_D
    X_log <- X_1 + cumsum(c(0, eps_X))
    # transform back out of log space
    X <- exp(X_log)

    # re-ascension rate
    # random walk of re_ascension rate
    nll <-
      nll - sum(dnorm(eps_eta,
                      mean = 0,
                      sd = exp(sdEta_ln),
                      log = T))

    # true eta in logit space
    eta_logit <- eta_1 + cumsum(c(0, eps_eta))
    # transform out of logit space
    eta <- inv_logit(eta_logit)

    # daytime passage rate
    # random walk of re_ascension rate
    nll <-
      nll - sum(dnorm(eps_theta,
                      mean = 0,
                      sd = exp(sdTheta_ln),
                      log = T))

    # true theta in logit space
    theta_logit <- theta_1 + cumsum(c(0, eps_theta))
    # transform out of logit space
    theta <- inv_logit(theta_logit)
    # fish passing during the day
    X_D <- X * theta

    ## Data
    # window counts
    # Using Poisson (no over-dispersion)
    nll <-
      nll - sum(dpois(x = Y_window,
                      lambda = X_D,
                      log = T))

    # re-ascension tags
    nll <-
      nll - sum(dbinom(x = reasc_tags,
                       size = tot_tags,
                       prob = eta,
                       log = T))

    # daytime tags
    nll <-
      nll - sum(dbinom(x = day_tags,
                       size = tot_tags,
                       prob = theta,
                       log = T))


    ## calculate some outputs
    # standard errors
    sdX <- exp(sdX_ln)
    sdEta <- exp(sdEta_ln)
    sdTheta <- exp(sdTheta_ln)
    # unique fish
    X_uniq <- X * (1 - eta)
    # calculate totals for the season
    X_tot <- sum(X)
    X_tot_day <- sum(X_D)
    X_tot_uniq <- sum(X_uniq)

    # get predicted daytime fish
    ADREPORT(X)
    ADREPORT(X_D)
    ADREPORT(X_uniq)
    ADREPORT(X_tot)
    ADREPORT(X_tot_day)
    ADREPORT(X_tot_uniq)
    ADREPORT(eta)
    ADREPORT(theta)
    ADREPORT(sdX)
    ADREPORT(sdEta)
    ADREPORT(sdTheta)

    ## Return
    nll
  }


obj_nb <-
  function(params) {

    getAll(mod_data,
           params)

    # ## Optional (enables extra RTMB features)
    # Y_window <- OBS(Y_window)

    ## Initialize joint negative log likelihood
    nll <- 0
    # random walk of X_D
    nll <-
      nll - sum(dnorm(eps_X,
                      mean = 0,
                      sd = exp(sdX_ln),
                      log = T))

    # what is true X_D
    X_log <- X_1 + cumsum(c(0, eps_X))
    # transform back out of log space
    X <- exp(X_log)

    # re-ascension rate
    # random walk of re_ascension rate
    nll <-
      nll - sum(dnorm(eps_eta,
                      mean = 0,
                      sd = exp(sdEta_ln),
                      log = T))

    # true eta in logit space
    eta_logit <- eta_1 + cumsum(c(0, eps_eta))
    # transform out of logit space
    eta <- inv_logit(eta_logit)

    # daytime passage rate
    # random walk of re_ascension rate
    nll <-
      nll - sum(dnorm(eps_theta,
                      mean = 0,
                      sd = exp(sdTheta_ln),
                      log = T))

    # true theta in logit space
    theta_logit <- theta_1 + cumsum(c(0, eps_theta))
    # transform out of logit space
    theta <- inv_logit(theta_logit)
    # fish passing during the day
    X_D <- X * theta

    ## Data
    # window counts
    # Using negative binomial (with potential over-dispersion)
    # if r is large, approximately Poisson
    # if r is small, over-dispersion exists
    # ensure that r is positive
    r <- exp(ln_r)
    p <- r / (r + X_D)
    nll <-
      nll - sum(dnbinom(x = Y_window,
                        prob = p,
                        size = r,
                        log = T))

    # estimating the trap rate
    nll <-
      nll - sum(dnorm(eps_nu,
                      mean = 0,
                      sd = exp(sdNu_ln),
                      log = T))
    trap_rate <- inv_logit(mean_nu + eps_nu)

    # using tags detected ascending ladder, and tags detected in trap (multiple species)
    nll <-
      nll - sum(dbinom(x = n_trap_tags,
                       size = n_poss_tags,
                       prob = trap_rate,
                       log = T))

    # estimate abundance using trap counts (species specific)
    # Poisson trick, but no constraints
    nll <-
      nll - sum(dpois(x = Y_trap,
                      lambda = X * trap_rate,
                      log = T))

    # re-ascension tags
    nll <-
      nll - sum(dbinom(x = reasc_tags,
                       size = tot_tags,
                       prob = eta,
                       log = T))

    # daytime tags
    nll <-
      nll - sum(dbinom(x = day_tags,
                       size = tot_tags,
                       prob = theta,
                       log = T))


    ## calculate some outputs
    # standard errors
    sdX <- exp(sdX_ln)
    sdEta <- exp(sdEta_ln)
    sdTheta <- exp(sdTheta_ln)
    sdNu <- exp(sdNu_ln)
    # unique fish
    X_uniq <- X * (1 - eta)
    # estimate of total fish from trap
    X_trap <- Y_trap / trap_rate
    # calculate totals for the season
    X_tot <- sum(X)
    X_tot_day <- sum(X_D)
    X_tot_uniq <- sum(X_uniq)
    X_tot_trap <- sum(X_trap)

    # get predicted daytime fish
    ADREPORT(X)
    ADREPORT(X_D)
    ADREPORT(X_uniq)
    ADREPORT(X_trap)
    ADREPORT(X_tot)
    ADREPORT(X_tot_day)
    ADREPORT(X_tot_uniq)
    ADREPORT(X_tot_trap)
    ADREPORT(eta)
    ADREPORT(theta)
    ADREPORT(sdX)
    ADREPORT(sdEta)
    ADREPORT(sdTheta)
    ADREPORT(sdNu)
    ADREPORT(r)
    ADREPORT(p)
    ADREPORT(trap_rate)

    ## Return
    nll
  }

obj_nb_origin <-
  function(params) {

    getAll(mod_data,
           params)

    # ## Optional (enables extra RTMB features)
    # Y_window <- OBS(Y_window)

    ## Initialize joint negative log likelihood
    nll <- 0
    # random walk of X_D
    nll <-
      nll - sum(dnorm(eps_X,
                      mean = 0,
                      sd = exp(sdX_ln),
                      log = T))

    # what is true X_D
    X_log <- X_1 + cumsum(c(0, eps_X))
    # transform back out of log space
    X <- exp(X_log)

    # re-ascension rate
    # random walk of re_ascension rate
    nll <-
      nll - sum(dnorm(eps_eta,
                      mean = 0,
                      sd = exp(sdEta_ln),
                      log = T))

    # true eta in logit space
    eta_logit <- eta_1 + cumsum(c(0, eps_eta))
    # transform out of logit space
    eta <- inv_logit(eta_logit)

    # daytime passage rate
    # random walk of re_ascension rate
    nll <-
      nll - sum(dnorm(eps_theta,
                      mean = 0,
                      sd = exp(sdTheta_ln),
                      log = T))

    # true theta in logit space
    theta_logit <- theta_1 + cumsum(c(0, eps_theta))
    # transform out of logit space
    theta <- inv_logit(theta_logit)
    # fish passing during the day
    X_D <- X * theta

    ## Data
    # window counts
    # Using negative binomial (with potential over-dispersion)
    # if r is large, approximately Poisson
    # if r is small, over-dispersion exists
    # ensure that r is positive
    r <- exp(ln_r)
    p <- r / (r + X_D)
    nll <-
      nll - sum(dnbinom(x = Y_window,
                        prob = p,
                        size = r,
                        log = T))

    # estimating the trap rate
    nll <-
      nll - sum(dnorm(eps_nu,
                      mean = 0,
                      sd = exp(sdNu_ln),
                      log = T))
    trap_rate <- inv_logit(mean_nu + eps_nu)

    # using tags detected ascending ladder, and tags detected in trap (multiple species)
    nll <-
      nll - sum(dbinom(x = n_trap_tags,
                       size = n_poss_tags,
                       prob = trap_rate,
                       log = T))

    # estimate abundance using trap counts (species specific)
    # Poisson trick, but no constraints
    nll <-
      nll - sum(dpois(x = Y_trap,
                      lambda = X * trap_rate,
                      log = T))

    # re-ascension tags
    nll <-
      nll - sum(dbinom(x = reasc_tags,
                       size = tot_tags,
                       prob = eta,
                       log = T))

    # daytime tags
    nll <-
      nll - sum(dbinom(x = day_tags,
                       size = tot_tags,
                       prob = theta,
                       log = T))

    # proportion of different origins
    # multivariate normal process errors for origin props
    nll <-
      nll - sum(dmvnorm(eps_phi,
                        mu = c(0, 0, 0),
                        Sigma = diag(rep(sdPhi, 3)^2),
                        log = T))

    # phi vectors
    for(j in 1:3) {
      phi[,j] <- phi_1[j] + cumsum(c(0, eps_phi[,j]))
    }
    phi_exp <- exp(phi)
    for(i in 1:nrow(phi_exp)) {
      omega[i,] <- phi_exp[i,] / sum(phi_exp[i,])
      omega_ll[i] <- if_else(trap_fish[i] > 0,
                             dmultinom(x = c(wild_fish[i],
                                             hnc_fish[i],
                                             hatch_fish[i]),
                                       prob = omega[i,],
                                       log = T),
                             0)
    }

    nll <-
      nll - sum(omega_ll)



    ## calculate some outputs
    # standard errors
    sdX <- exp(sdX_ln)
    sdEta <- exp(sdEta_ln)
    sdTheta <- exp(sdTheta_ln)
    sdNu <- exp(sdNu_ln)
    # unique fish
    X_uniq <- X * (1 - eta)
    # estimate of total fish from trap
    X_trap <- Y_trap / trap_rate
    # calculate totals for the season
    X_tot <- sum(X)
    X_tot_day <- sum(X_D)
    X_tot_uniq <- sum(X_uniq)
    X_tot_trap <- sum(X_trap)

    # get predicted daytime fish
    ADREPORT(X)
    ADREPORT(X_D)
    ADREPORT(X_uniq)
    ADREPORT(X_trap)
    ADREPORT(X_tot)
    ADREPORT(X_tot_day)
    ADREPORT(X_tot_uniq)
    ADREPORT(X_tot_trap)
    ADREPORT(eta)
    ADREPORT(theta)
    ADREPORT(sdX)
    ADREPORT(sdEta)
    ADREPORT(sdTheta)
    ADREPORT(sdNu)
    ADREPORT(r)
    ADREPORT(p)
    ADREPORT(trap_rate)

    ## Return
    nll
  }
