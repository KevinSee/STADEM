#----------------------------------------------------------
# NIMBLE models
library(nimble)

mc_pois <-
  nimbleCode({

    ####################################
    # priors
    ####################################
    # process error in log space, use a half-Cauchy distribution (equivalent to t-distribution with 1 degree of freedom)
    X.sigma ~ T(dt(0, 0.001, 1), 0,)

    # initial state, half-Cauchy distribution
    X.all.prior1 ~ T(dt(0, 0.001, 1), 0,)
    X.log.all[1] <- log(X.all.prior1)

    # modeling proportion of fish available for window counts
    day.true.prior1 ~ dunif(0, 1) # daytime ascension rate for week 1
    day.true.logit[1] <- logit(day.true.prior1)
    day.sigma ~ T(dt(0, 0.001, 1), 0,) # process error on daytime (window open) proportion - half-Cauchy

    # modeling proportion of fish re-ascending the dam
    reasc.true.prior1 ~ dunif(0, 1)  # re-ascension rate for week 1
    reasc.true.logit[1] <- logit(reasc.true.prior1)
    reasc.sigma ~ T(dt(0, 0.001, 1), 0,) # process error on re-ascension proportion - half-Cauchy

    # parameter clean-up
    for(i in 1:TotLadderWeeks) {
      day.true[i] <- ilogit(day.true.logit[i]) * ladder[i]
      reasc.true[i] <- ilogit(reasc.true.logit[i]) * ladder[i]
    }

    ####################################
    ## True state of nature
    ####################################

    for(i in 2:TotLadderWeeks) {
      # random walks
      X.log.all[i] ~ dnorm(X.log.all[i-1], X.sigma)
      day.true.logit[i] ~ dnorm(day.true.logit[i-1], day.sigma)
      reasc.true.logit[i] ~ dnorm(reasc.true.logit[i-1], reasc.sigma)
    }

    # derived parameters
    for(i in 1:TotLadderWeeks) {

      X.all[i] <- round(exp(X.log.all[i]) * ladder[i])
      X.day[i] <- round(X.all[i] * day.true[i])
      X.night[i] <- X.all[i] - X.day[i]
      X.reasc[i] <- round(X.all[i] * reasc.true[i])

      X.new.tot[i] <- round(X.all[i] * (1 - reasc.true[i]))

    }

    ####################################
    ## What we observe
    ####################################

    for(i in 1:TotLadderWeeks) {
      # at window
      # Poisson
      Y.window[i] ~ dpois(X.all[i])

      # in trap
      # uncertainty in trap rate
      trap.rate.true[i] ~ dbeta(1, 1)
      n.trap.tags[i] ~ dbin(trap.rate.true[i], n.poss.tags[i])
      Y.trap[i] ~ dbin(trap.rate.true[i], X.all[i])

      # day-time tags
      DC.tags[i] ~ dbin(day.true[i], Tot.tags[i])

      # re-ascension tags
      ReAsc.tags[i] ~ dbin(reasc.true[i], Tot.tags[i])
    }

    ####################################
    ## Summary statistics
    ####################################

    X.tot.all <- sum(X.all[1:TotLadderWeeks])
    X.tot.day <- sum(X.day[1:TotLadderWeeks])
    X.tot.night <- sum(X.night[1:TotLadderWeeks])
    X.tot.reasc <- sum(X.reasc[1:TotLadderWeeks])

    X.tot.new.all <- sum(X.new.tot[1:TotLadderWeeks])
  })

mc_no_trap <-
  nimbleCode({

    ####################################
    # priors
    ####################################
    # process error in log space, use a half-Cauchy distribution (equivalent to t-distribution with 1 degree of freedom)
    X.sigma ~ T(dt(0, 0.001, 1), 0,)

    # initial state, half-Cauchy distribution
    X.all.prior1 ~ T(dt(0, 0.001, 1), 0,)
    X.log.all[1] <- log(X.all.prior1)

    # modeling proportion of fish available for window counts
    day.true.prior1 ~ dunif(0, 1) # daytime ascension rate for week 1
    day.true.logit[1] <- logit(day.true.prior1)
    day.sigma ~ T(dt(0, 0.001, 1), 0,) # process error on daytime (window open) proportion - half-Cauchy

    # modeling proportion of fish re-ascending the dam
    reasc.true.prior1 ~ dunif(0, 1)  # re-ascension rate for week 1
    reasc.true.logit[1] <- logit(reasc.true.prior1)
    reasc.sigma ~ T(dt(0, 0.001, 1), 0,) # process error on re-ascension proportion - half-Cauchy

    # parameter clean-up
    for(i in 1:TotLadderWeeks) {
      day.true[i] <- ilogit(day.true.logit[i]) * ladder[i]
      reasc.true[i] <- ilogit(reasc.true.logit[i]) * ladder[i]
    }

    ####################################
    ## True state of nature
    ####################################

    for(i in 2:TotLadderWeeks) {
      # random walks
      X.log.all[i] ~ dnorm(X.log.all[i-1], X.sigma)
      day.true.logit[i] ~ dnorm(day.true.logit[i-1], day.sigma)
      reasc.true.logit[i] ~ dnorm(reasc.true.logit[i-1], reasc.sigma)
    }

    # derived parameters
    for(i in 1:TotLadderWeeks) {

      X.all[i] <- round(exp(X.log.all[i]) * ladder[i])
      X.day[i] <- round(X.all[i] * day.true[i])
      X.night[i] <- X.all[i] - X.day[i]
      X.reasc[i] <- round(X.all[i] * reasc.true[i])

      X.new.tot[i] <- round(X.all[i] * (1 - reasc.true[i]))

    }

    ####################################
    ## What we observe
    ####################################

    for(i in 1:TotLadderWeeks) {
      # at window
      # Poisson
      Y.window[i] ~ dpois(X.all[i])

      # day-time tags
      DC.tags[i] ~ dbin(day.true[i], Tot.tags[i])

      # re-ascension tags
      ReAsc.tags[i] ~ dbin(reasc.true[i], Tot.tags[i])
    }

    ####################################
    ## Summary statistics
    ####################################

    X.tot.all <- sum(X.all[1:TotLadderWeeks])
    X.tot.day <- sum(X.day[1:TotLadderWeeks])
    X.tot.night <- sum(X.night[1:TotLadderWeeks])
    X.tot.reasc <- sum(X.reasc[1:TotLadderWeeks])

    X.tot.new.all <- sum(X.new.tot[1:TotLadderWeeks])
  })


mc_nb <-
  nimbleCode({

    ####################################
    # priors
    ####################################
    # process error in log space, use a half-Cauchy distribution (equivalent to t-distribution with 1 degree of freedom)
    X.sigma ~ T(dt(0, 0.001, 1), 0,)

    # initial state, half-Cauchy distribution
    X.all.prior1 ~ T(dt(0, 0.001, 1), 0,)
    X.log.all[1] <- log(X.all.prior1)

    # for over-dispersed negative binomial
    # overdispersed if r is small, approximately Poisson if r is very large
    # use a half-Cauchy distribution (equivalent to t-distribution with 1 degree of freedom)
    r ~ T(dt(0, 0.001, 1), 0,)
    k <- 1/r

    # modeling proportion of fish available for window counts
    day.true.prior1 ~ dunif(0, 1) # daytime ascension rate for week 1
    day.true.logit[1] <- logit(day.true.prior1)
    day.sigma ~ T(dt(0, 0.001, 1), 0,) # process error on daytime (window open) proportion - half-Cauchy

    # modeling proportion of fish re-ascending the dam
    reasc.true.prior1 ~ dunif(0, 1)  # re-ascension rate for week 1
    reasc.true.logit[1] <- logit(reasc.true.prior1)
    reasc.sigma ~ T(dt(0, 0.001, 1), 0,) # process error on re-ascension proportion - half-Cauchy

    # parameter clean-up
    for(i in 1:TotLadderWeeks) {
      day.true[i] <- ilogit(day.true.logit[i]) * ladder[i]
      reasc.true[i] <- ilogit(reasc.true.logit[i]) * ladder[i]
    }

    ####################################
    ## True state of nature
    ####################################

    for(i in 2:TotLadderWeeks) {
      # random walks
      X.log.all[i] ~ dnorm(X.log.all[i-1], X.sigma)
      day.true.logit[i] ~ dnorm(day.true.logit[i-1], day.sigma)
      reasc.true.logit[i] ~ dnorm(reasc.true.logit[i-1], reasc.sigma)
    }

    # derived parameters
    for(i in 1:TotLadderWeeks) {

      X.all[i] <- round(exp(X.log.all[i]) * ladder[i])
      X.day[i] <- round(X.all[i] * day.true[i])
      X.night[i] <- X.all[i] - X.day[i]
      X.reasc[i] <- round(X.all[i] * reasc.true[i])

      X.new.tot[i] <- round(X.all[i] * (1 - reasc.true[i]))

    }

    ####################################
    ## What we observe
    ####################################

    for(i in 1:TotLadderWeeks) {
      # at window
      # Negative binomial
      p[i] <- r / (r + X.day[i])
      Y.window[i] ~ dnegbin(p[i], r)

      # in trap
      # uncertainty in trap rate
      trap.rate.true[i] ~ dbeta(1, 1)
      n.trap.tags[i] ~ dbin(trap.rate.true[i], n.poss.tags[i])
      Y.trap[i] ~ dbin(trap.rate.true[i], X.all[i])

      # day-time tags
      DC.tags[i] ~ dbin(day.true[i], Tot.tags[i])

      # re-ascension tags
      ReAsc.tags[i] ~ dbin(reasc.true[i], Tot.tags[i])
    }

    ####################################
    ## Summary statistics
    ####################################

    X.tot.all <- sum(X.all[1:TotLadderWeeks])
    X.tot.day <- sum(X.day[1:TotLadderWeeks])
    X.tot.night <- sum(X.night[1:TotLadderWeeks])
    X.tot.reasc <- sum(X.reasc[1:TotLadderWeeks])

    X.tot.new.all <- sum(X.new.tot[1:TotLadderWeeks])
  })

mc_nb_origin <-
  nimbleCode({

    ####################################
    # priors
    ####################################
    # process error in log space, use a half-Cauchy distribution (equivalent to t-distribution with 1 degree of freedom)
    X.sigma ~ T(dt(0, 0.001, 1), 0,)

    # initial state, half-Cauchy distribution
    X.all.prior1 ~ T(dt(0, 0.001, 1), 0,)
    X.log.all[1] <- log(X.all.prior1)

    # for over-dispersed negative binomial
    # overdispersed if r is small, approximately Poisson if r is very large
    # use a half-Cauchy distribution (equivalent to t-distribution with 1 degree of freedom)
    r ~ T(dt(0, 0.001, 1), 0,)
    k <- 1/r

    # modeling proportion of fish available for window counts
    day.true.prior1 ~ dunif(0, 1) # daytime ascension rate for week 1
    day.true.logit[1] <- logit(day.true.prior1)
    day.sigma ~ T(dt(0, 0.001, 1), 0,) # process error on daytime (window open) proportion - half-Cauchy

    # modeling proportion of fish re-ascending the dam
    reasc.true.prior1 ~ dunif(0, 1)  # re-ascension rate for week 1
    reasc.true.logit[1] <- logit(reasc.true.prior1)
    reasc.sigma ~ T(dt(0, 0.001, 1), 0,) # process error on re-ascension proportion - half-Cauchy

    # modeling proportion of fish that are wild, hatchery no-clip and hatchery
    # set probability for any type of fish that was never caught to 0
    # prior on log odds ratio for initial week
    for(j in 1:2) {
      org.phi[1,j] ~ dunif(-3, 3)
      exp.org.phi[1,j] <- exp(org.phi[1,j]) * org.exist[j]
    }

    # set hatchery as baseline
    for(i in 1:TotLadderWeeks) {
      org.phi[i,3] <- 0
      exp.org.phi[i,3] <- exp(org.phi[i,3]) * org.exist[3]
      # get sum of all phis
      sum.exp.phi[i] <- sum(exp.org.phi[i,1:3])
    }

    # extract initial movement probabilities for week 1
    for(j in 1:3) {
      # org.prop[1,j] <- ifelse(org.exist[j] == 0, 0, exp.org.phi[1,j] / sum.exp.phi[1])
      org.prop[1,j] <- exp.org.phi[1,j] / sum.exp.phi[1]
    }

    # variation in time-varying random walk movement probabilities
    org.sigma ~ T(dt(0, 0.001, 1), 0,) # half-Cauchy

    for(i in 2:TotLadderWeeks) {
      for(j in 1:2) {
        epsilon[i,j] ~ dnorm(0, org.sigma)
        # set phi for any type of fish that was never caught to 0
        # org.phi[i,j] <- ifelse(org.exist[j] == 0, 0, org.phi[i - 1, j] + epsilon[i,j])
        org.phi[i,j] <- org.phi[i - 1, j] + epsilon[i,j]
        exp.org.phi[i,j] <- exp(org.phi[i,j]) * org.exist[j]
      }

      for (j in 1:3) {
        org.prop[i,j] <- exp.org.phi[i,j] / sum.exp.phi[i]
      }
    }

    # parameter clean-up
    for(i in 1:TotLadderWeeks) {
      day.true[i] <- ilogit(day.true.logit[i]) * ladder[i]
      reasc.true[i] <- ilogit(reasc.true.logit[i]) * ladder[i]
    }

    ####################################
    ## True state of nature
    ####################################

    for(i in 2:TotLadderWeeks) {
      # random walks
      X.log.all[i] ~ dnorm(X.log.all[i-1], X.sigma)
      day.true.logit[i] ~ dnorm(day.true.logit[i-1], day.sigma)
      reasc.true.logit[i] ~ dnorm(reasc.true.logit[i-1], reasc.sigma)
    }

    # derived parameters
    for(i in 1:TotLadderWeeks) {

      X.all[i] <- round(exp(X.log.all[i]) * ladder[i])
      X.day[i] <- round(X.all[i] * day.true[i])
      X.night[i] <- X.all[i] - X.day[i]
      X.reasc[i] <- round(X.all[i] * reasc.true[i])

      X.all.wild[i] <- round(X.all[i] * org.prop[i,1])
      X.all.hnc[i] <- round(X.all[i] * org.prop[i,2])
      X.all.hatch[i] <- round(X.all[i] * org.prop[i,3])

      X.new.tot[i] <- round(X.all[i] * (1 - reasc.true[i]))
      X.new.wild[i] <- round(X.new.tot[i] * org.prop[i,1])
      X.new.hnc[i] <- round(X.new.tot[i] * org.prop[i,2])
      X.new.hatch[i] <- round(X.new.tot[i] * org.prop[i,3])

      X.reasc.wild[i] <- X.all.wild[i] - X.new.wild[i]
      X.night.wild[i] <- X.new.wild[i] * (1 - day.true[i])
    }

    ####################################
    ## What we observe
    ####################################

    for(i in 1:TotLadderWeeks) {
      # at window
      # Negative binomial
      p[i] <- r / (r + X.day[i])
      Y.window[i] ~ dnegbin(p[i], r)

      # in trap
      # uncertainty in trap rate
      trap.rate.true[i] ~ dbeta(1, 1)
      n.trap.tags[i] ~ dbin(trap.rate.true[i], n.poss.tags[i])
      Y.trap[i] ~ dbin(trap.rate.true[i], X.all[i])

      # fish in trap by origin
      trap.fish.matrix[i,1:3] ~ dmulti(org.prop[i,1:3], trap.fish[i])

      # day-time tags
      DC.tags[i] ~ dbin(day.true[i], Tot.tags[i])

      # re-ascension tags
      ReAsc.tags[i] ~ dbin(reasc.true[i], Tot.tags[i])
    }

    ####################################
    ## Summary statistics
    ####################################

    X.tot.all <- sum(X.all[1:TotLadderWeeks])
    X.tot.day <- sum(X.day[1:TotLadderWeeks])
    X.tot.night <- sum(X.night[1:TotLadderWeeks])
    X.tot.reasc <- sum(X.reasc[1:TotLadderWeeks])

    X.tot.all.wild <- sum(X.all.wild[1:TotLadderWeeks])
    X.tot.all.hatch <- sum(X.all.hatch[1:TotLadderWeeks])
    X.tot.all.hnc <- sum(X.all.hnc[1:TotLadderWeeks])

    X.tot.new.all <- sum(X.new.tot[1:TotLadderWeeks])
    X.tot.new.wild <- sum(X.new.wild[1:TotLadderWeeks])
    X.tot.new.hatch <- sum(X.new.hatch[1:TotLadderWeeks])
    X.tot.new.hnc <- sum(X.new.hnc[1:TotLadderWeeks])

    X.tot.night.wild <- sum(X.night.wild[1:TotLadderWeeks])
    X.tot.reasc.wild <- sum(X.reasc.wild[1:TotLadderWeeks])

    # combine all hatchery fish
    X.tot.new.hatch.all = X.tot.new.hatch + X.tot.new.hnc

    # proportion of wild fish tagged
    prop.tagged <- sum(trap.fish.matrix[,1]) / X.tot.new.wild
  })

mc_origin_no_trap <-
  nimbleCode({

    ####################################
    # priors
    ####################################
    # process error in log space, use a half-Cauchy distribution (equivalent to t-distribution with 1 degree of freedom)
    X.sigma ~ T(dt(0, 0.001, 1), 0,)

    # initial state, half-Cauchy distribution
    X.all.prior1 ~ T(dt(0, 0.001, 1), 0,)
    X.log.all[1] <- log(X.all.prior1)

    # modeling proportion of fish available for window counts
    day.true.prior1 ~ dunif(0, 1) # daytime ascension rate for week 1
    day.true.logit[1] <- logit(day.true.prior1)
    day.sigma ~ T(dt(0, 0.001, 1), 0,) # process error on daytime (window open) proportion - half-Cauchy

    # modeling proportion of fish re-ascending the dam
    reasc.true.prior1 ~ dunif(0, 1)  # re-ascension rate for week 1
    reasc.true.logit[1] <- logit(reasc.true.prior1)
    reasc.sigma ~ T(dt(0, 0.001, 1), 0,) # process error on re-ascension proportion - half-Cauchy

    # modeling proportion of fish that are wild, hatchery no-clip and hatchery
    # set probability for any type of fish that was never caught to 0
    # prior on log odds ratio for initial week
    for(j in 1:2) {
      org.phi[1,j] ~ dunif(-3, 3)
      exp.org.phi[1,j] <- exp(org.phi[1,j]) * org.exist[j]
    }

    # set hatchery as baseline
    for(i in 1:TotLadderWeeks) {
      org.phi[i,3] <- 0
      exp.org.phi[i,3] <- exp(org.phi[i,3]) * org.exist[3]
      # get sum of all phis
      sum.exp.phi[i] <- sum(exp.org.phi[i,1:3])
    }

    # extract initial movement probabilities for week 1
    for(j in 1:3) {
      # org.prop[1,j] <- ifelse(org.exist[j] == 0, 0, exp.org.phi[1,j] / sum.exp.phi[1])
      org.prop[1,j] <- exp.org.phi[1,j] / sum.exp.phi[1]
    }

    # variation in time-varying random walk movement probabilities
    org.sigma ~ T(dt(0, 0.001, 1), 0,) # half-Cauchy

    for(i in 2:TotLadderWeeks) {
      for(j in 1:2) {
        epsilon[i,j] ~ dnorm(0, org.sigma)
        # set phi for any type of fish that was never caught to 0
        # org.phi[i,j] <- ifelse(org.exist[j] == 0, 0, org.phi[i - 1, j] + epsilon[i,j])
        org.phi[i,j] <- org.phi[i - 1, j] + epsilon[i,j]
        exp.org.phi[i,j] <- exp(org.phi[i,j]) * org.exist[j]
      }

      for (j in 1:3) {
        org.prop[i,j] <- exp.org.phi[i,j] / sum.exp.phi[i]
      }
    }

    # parameter clean-up
    for(i in 1:TotLadderWeeks) {
      day.true[i] <- ilogit(day.true.logit[i]) * ladder[i]
      reasc.true[i] <- ilogit(reasc.true.logit[i]) * ladder[i]
    }

    ####################################
    ## True state of nature
    ####################################

    for(i in 2:TotLadderWeeks) {
      # random walks
      X.log.all[i] ~ dnorm(X.log.all[i-1], X.sigma)
      day.true.logit[i] ~ dnorm(day.true.logit[i-1], day.sigma)
      reasc.true.logit[i] ~ dnorm(reasc.true.logit[i-1], reasc.sigma)
    }

    # derived parameters
    for(i in 1:TotLadderWeeks) {

      X.all[i] <- round(exp(X.log.all[i]) * ladder[i])
      X.day[i] <- round(X.all[i] * day.true[i])
      X.night[i] <- X.all[i] - X.day[i]
      X.reasc[i] <- round(X.all[i] * reasc.true[i])

      X.all.wild[i] <- round(X.all[i] * org.prop[i,1])
      X.all.hnc[i] <- round(X.all[i] * org.prop[i,2])
      X.all.hatch[i] <- round(X.all[i] * org.prop[i,3])

      X.new.tot[i] <- round(X.all[i] * (1 - reasc.true[i]))
      X.new.wild[i] <- round(X.new.tot[i] * org.prop[i,1])
      X.new.hnc[i] <- round(X.new.tot[i] * org.prop[i,2])
      X.new.hatch[i] <- round(X.new.tot[i] * org.prop[i,3])

      X.reasc.wild[i] <- X.all.wild[i] - X.new.wild[i]
      X.night.wild[i] <- X.new.wild[i] * (1 - day.true[i])
    }

    ####################################
    ## What we observe
    ####################################

    for(i in 1:TotLadderWeeks) {
      # at window
      # Poisson
      Y.window[i] ~ dpois(X.all[i])

      # fish in trap by origin
      trap.fish.matrix[i,1:3] ~ dmulti(org.prop[i,1:3], trap.fish[i])

      # day-time tags
      DC.tags[i] ~ dbin(day.true[i], Tot.tags[i])

      # re-ascension tags
      ReAsc.tags[i] ~ dbin(reasc.true[i], Tot.tags[i])
    }

    ####################################
    ## Summary statistics
    ####################################

    X.tot.all <- sum(X.all[1:TotLadderWeeks])
    X.tot.day <- sum(X.day[1:TotLadderWeeks])
    X.tot.night <- sum(X.night[1:TotLadderWeeks])
    X.tot.reasc <- sum(X.reasc[1:TotLadderWeeks])

    X.tot.all.wild <- sum(X.all.wild[1:TotLadderWeeks])
    X.tot.all.hatch <- sum(X.all.hatch[1:TotLadderWeeks])
    X.tot.all.hnc <- sum(X.all.hnc[1:TotLadderWeeks])

    X.tot.new.all <- sum(X.new.tot[1:TotLadderWeeks])
    X.tot.new.wild <- sum(X.new.wild[1:TotLadderWeeks])
    X.tot.new.hatch <- sum(X.new.hatch[1:TotLadderWeeks])
    X.tot.new.hnc <- sum(X.new.hnc[1:TotLadderWeeks])

    X.tot.night.wild <- sum(X.night.wild[1:TotLadderWeeks])
    X.tot.reasc.wild <- sum(X.reasc.wild[1:TotLadderWeeks])

    # combine all hatchery fish
    X.tot.new.hatch.all = X.tot.new.hatch + X.tot.new.hnc

    # proportion of wild fish tagged
    prop.tagged <- sum(trap.fish.matrix[,1]) / X.tot.new.wild
  })
