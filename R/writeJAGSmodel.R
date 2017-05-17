#' @title Write JAGS model
#'
#' @description Writes a text file version of the model to be used by JAGS
#'
#' @author Kevin See
#'
#' @param file_name name (with file path) to save the model as
#' @param win_model what type of distribution should be used when modeling the window counts. \code{neg_bin} is a standard negative binomial distribution. \code{neg_bin2} is a more flexible version of a negative binomial, allowing the mean-variance relationship to take different forms. \code{pois} is a Poisson distribution.
#'
#' @export
#' @return NULL
#' @examples writeJAGSmodel()
#'
writeJAGSmodel = function(file_name = NULL,
                          win_model = c('neg_bin', 'neg_bin2', 'pois')) {

  if(is.null(file_name)) file_name = 'LGR_escapement_JAGS.txt'

  win_model = match.arg(win_model)

  if(win_model == 'neg_bin') {
    model_code =
      'model {

  ####################################
  # Set up parameters and initial states...
  ####################################
  X.sigma ~ dunif(0, 20) # process error in log space
  X.tau <- pow(X.sigma, -2)
  X.log.all[1] ~ dunif(0,10) # initial state in log space

  # for over-dispersed negative binomial
  r ~ dgamma(0.01, 0.01)
  k <- 1/r

  # modeling proportion of fish available for window counts
  win.prop.true.logit[1] ~ dnorm(0, 0.001)	# daytime ascension rate for week 1
  win.prop.sigma ~ dunif(0, 10) # process error on window proportion correction
  win.prop.tau <- pow(win.prop.sigma, -2)

  # modeling proportion of fish re-ascending the dam
  reasc.true.logit[1] ~ dnorm(0, 0.001) # re-ascension rate for week 1
  reasc.sigma ~ dunif(0, 10) # process error on re-ascension proportion correction
  reasc.tau <- pow(reasc.sigma, -2)

  # modeling proportion of fish that are wild, hatchery no-clip and hatchery
  # set probability for any type of fish that was never caught to 0
  # prior on log odds ratio for initial week
  for(j in 1:2) {
    org.phi[1,j] ~ dnorm(0, 0.01)
    exp.org.phi[1,j] <- exp(org.phi[1,j]) * org.exist[j]
  }

  # set hatchery as baseline
  for(i in 1:TotLadderWeeks) {
    org.phi[i,3] <- 0
    exp.org.phi[i,3] <- exp(org.phi[i,3]) * org.exist[3]
    # get sum of all phis
    sum.exp.phi[i] <- sum(exp.org.phi[i,])
  }

  # extract initial movement probabilities for week 1
  for(j in 1:3) {
    org.prop[1,j] <- ifelse(org.exist[j] == 0, 0, exp.org.phi[1,j] / sum.exp.phi[1])
  }

  # variation in time-varying random walk movement probabilities
  org.sigma ~ dunif(0, 10)
  org.tau <- pow(org.sigma, -2)

  for(i in 2:TotLadderWeeks) {
    for(j in 1:2) {
      epsilon[i,j] ~ dnorm(0, org.tau)
      # set phi for any type of fish that was never caught to 0
      org.phi[i,j] <- ifelse(org.exist[j] == 0, 0, org.phi[i - 1, j] + epsilon[i,j])
      exp.org.phi[i,j] <- exp(org.phi[i,j]) * org.exist[j]
    }

    for (j in 1:3) {
      org.prop[i,j] <- exp.org.phi[i,j] / sum.exp.phi[i]
    }
  }

  # parameter clean-up
  for(i in 1 :TotLadderWeeks) {
    win.prop.true[i] <- ilogit(win.prop.true.logit[i])
    reasc.true[i] <- ilogit(reasc.true.logit[i])
  }

  ####################################
  ## True state of nature
  ####################################

  for(i in 2:TotLadderWeeks) {
    # random walks
    X.log.all[i] ~ dnorm(X.log.all[i-1], X.tau)
    win.prop.true.logit[i] ~ dnorm(win.prop.true.logit[i-1], win.prop.tau)
    reasc.true.logit[i] ~ dnorm(reasc.true.logit[i-1], reasc.tau)
  }

  # derived parameters
  for(i in 1:TotLadderWeeks) {
    true.prop[i] <- win.prop.true[i]

    X.all[i] <- round(exp(X.log.all[i]))
    X.day[i] <- round(X.all[i] * true.prop[i])
    X.night[i] <- X.all[i] - X.day[i]
    X.reasc[i] <- round(X.all[i] * reasc.true[i])

    X.all.wild[i] <- round(X.all[i] * org.prop[i,1])
    X.all.hnc[i] <- round(X.all[i] * org.prop[i,2])
    X.all.hatch[i] <- round(X.all[i] * org.prop[i,3])

    X.new.wild[i] <- round(X.all[i] * org.prop[i,1] * (1-reasc.true[i]))
    X.new.hnc[i] <- round(X.all[i] * org.prop[i,2] * (1-reasc.true[i]))
    X.new.hatch[i] <- round(X.all[i] * org.prop[i,3] * (1-reasc.true[i]))

    X.reasc.wild[i] <- X.all.wild[i] - X.new.wild[i]
    X.night.wild[i] <- X.new.wild[i] * (1-true.prop[i])

  }

  ####################################
  ## What we observe
  ####################################

  for(i in 1:TotLadderWeeks) {
    # at window: over-dispersed negative binomial
    # overdispersed if r is small, approximately Poisson if r is very large
    p[i] <- r / (r + X.day[i])
    Y.window[i] ~ dnegbin(p[i], r)

    # in trap
    # uncertainty in trap rate
    trap.rate.true[i] ~ dbeta(trap.alpha[i], trap.beta[i])
    Y.trap[i] ~ dbin(trap.rate.true[i], X.all[i])

    # fish in trap by origin
    trap.fish.matrix[i,1:3] ~ dmulti(org.prop[i,], trap.fish[i])

    # day-time tags
    DC.tags[i] ~ dbin(win.prop.true[i], Tot.tags[i])

    # re-ascension tags
    ReAsc.tags[i] ~ dbin(reasc.true[i], Tot.tags[i])
  }

  ####################################
  ## Summary statistics
  ####################################

  X.tot.all <- sum(X.all)
  X.tot.day <- sum(X.day)
  X.tot.night <- sum(X.night)
  X.tot.reasc <- sum(X.reasc)

  X.tot.all.wild <- sum(X.all.wild)
  X.tot.all.hatch <- sum(X.all.hatch)
  X.tot.all.hnc <- sum(X.all.hnc)

  X.tot.new.wild <- sum(X.new.wild)
  X.tot.new.hatch <- sum(X.new.hatch)
  X.tot.new.hnc <- sum(X.new.hnc)

  X.tot.night.wild <- sum(X.night.wild)
  X.tot.reasc.wild <- sum(X.reasc.wild)

  prop.tagged <- sum(trap.fish.matrix[,1]) / X.tot.new.wild

}
' }

if(win_model == 'neg_bin2') {
  model_code =
    'model {

  ####################################
  # Set up parameters and initial states...
  ####################################
  X.sigma ~ dunif(0, 20) # process error in log space
  X.tau <- pow(X.sigma, -2)
  X.log.all[1] ~ dunif(0,10) # initial state in log space

  # for over-dispersed negative binomial
  omega ~ dgamma(0.01, 0.01)
  theta ~ dgamma(0.01, 0.01)

  # modeling proportion of fish available for window counts
  win.prop.true.logit[1] ~ dnorm(0, 0.001)	# daytime ascension rate for week 1
  win.prop.sigma ~ dunif(0, 10) # process error on window proportion correction
  win.prop.tau <- pow(win.prop.sigma, -2)

  # modeling proportion of fish re-ascending the dam
  reasc.true.logit[1] ~ dnorm(0, 0.001) # re-ascension rate for week 1
  reasc.sigma ~ dunif(0, 10) # process error on re-ascension proportion correction
  reasc.tau <- pow(reasc.sigma, -2)

  # modeling proportion of fish that are wild, hatchery no-clip and hatchery
  # set probability for any type of fish that was never caught to 0
  # prior on log odds ratio for initial week
  for(j in 1:2) {
  org.phi[1,j] ~ dnorm(0, 0.01)
  exp.org.phi[1,j] <- exp(org.phi[1,j]) * org.exist[j]
  }

  # set hatchery as baseline
  for(i in 1:TotLadderWeeks) {
  org.phi[i,3] <- 0
  exp.org.phi[i,3] <- exp(org.phi[i,3]) * org.exist[3]
  # get sum of all phis
  sum.exp.phi[i] <- sum(exp.org.phi[i,])
  }

  # extract initial movement probabilities for week 1
  for(j in 1:3) {
  org.prop[1,j] <- ifelse(org.exist[j] == 0, 0, exp.org.phi[1,j] / sum.exp.phi[1])
  }

  # variation in time-varying random walk movement probabilities
  org.sigma ~ dunif(0, 10)
  org.tau <- pow(org.sigma, -2)

  for(i in 2:TotLadderWeeks) {
  for(j in 1:2) {
  epsilon[i,j] ~ dnorm(0, org.tau)
  # set phi for any type of fish that was never caught to 0
  org.phi[i,j] <- ifelse(org.exist[j] == 0, 0, org.phi[i - 1, j] + epsilon[i,j])
  exp.org.phi[i,j] <- exp(org.phi[i,j]) * org.exist[j]
  }

  for (j in 1:3) {
  org.prop[i,j] <- exp.org.phi[i,j] / sum.exp.phi[i]
  }
  }

  # parameter clean-up
  for(i in 1 :TotLadderWeeks) {
  win.prop.true[i] <- ilogit(win.prop.true.logit[i])
  reasc.true[i] <- ilogit(reasc.true.logit[i])
  }

  ####################################
  ## True state of nature
  ####################################

  for(i in 2:TotLadderWeeks) {
  # random walks
  X.log.all[i] ~ dnorm(X.log.all[i-1], X.tau)
  win.prop.true.logit[i] ~ dnorm(win.prop.true.logit[i-1], win.prop.tau)
  reasc.true.logit[i] ~ dnorm(reasc.true.logit[i-1], reasc.tau)
  }

  # derived parameters
  for(i in 1:TotLadderWeeks) {
  true.prop[i] <- win.prop.true[i]

  X.all[i] <- round(exp(X.log.all[i]))
  X.day[i] <- round(X.all[i] * true.prop[i])
  X.night[i] <- X.all[i] - X.day[i]
  X.reasc[i] <- round(X.all[i] * reasc.true[i])

  X.all.wild[i] <- round(X.all[i] * org.prop[i,1])
  X.all.hnc[i] <- round(X.all[i] * org.prop[i,2])
  X.all.hatch[i] <- round(X.all[i] * org.prop[i,3])

  X.new.wild[i] <- round(X.all[i] * org.prop[i,1] * (1-reasc.true[i]))
  X.new.hnc[i] <- round(X.all[i] * org.prop[i,2] * (1-reasc.true[i]))
  X.new.hatch[i] <- round(X.all[i] * org.prop[i,3] * (1-reasc.true[i]))

  X.reasc.wild[i] <- X.all.wild[i] - X.new.wild[i]
  X.night.wild[i] <- X.new.wild[i] * (1-true.prop[i])

  }

  ####################################
  ## What we observe
  ####################################

  for(i in 1:TotLadderWeeks) {
  # at window: over-dispersed negative binomial
  # like quasi-Poisson if theta = 0, like neg. binomial if omega = 1, like Poisson if theta = 0 & omega = 1
  r[i] <- X.all[i] / (omega - 1 + (theta * X.all[i]))
  p[i] <- 1 / (omega + theta * X.all[i])
  Y.window[i] ~ dnegbin(p[i], r[i])

  # in trap
  # uncertainty in trap rate
  trap.rate.true[i] ~ dbeta(trap.alpha[i], trap.beta[i])
  Y.trap[i] ~ dbin(trap.rate.true[i], X.all[i])

  # fish in trap by origin
  trap.fish.matrix[i,1:3] ~ dmulti(org.prop[i,], trap.fish[i])

  # day-time tags
  DC.tags[i] ~ dbin(win.prop.true[i], Tot.tags[i])

  # re-ascension tags
  ReAsc.tags[i] ~ dbin(reasc.true[i], Tot.tags[i])
  }

  ####################################
  ## Summary statistics
  ####################################

  X.tot.all <- sum(X.all)
  X.tot.day <- sum(X.day)
  X.tot.night <- sum(X.night)
  X.tot.reasc <- sum(X.reasc)

  X.tot.all.wild <- sum(X.all.wild)
  X.tot.all.hatch <- sum(X.all.hatch)
  X.tot.all.hnc <- sum(X.all.hnc)

  X.tot.new.wild <- sum(X.new.wild)
  X.tot.new.hatch <- sum(X.new.hatch)
  X.tot.new.hnc <- sum(X.new.hnc)

  X.tot.night.wild <- sum(X.night.wild)
  X.tot.reasc.wild <- sum(X.reasc.wild)

  prop.tagged <- sum(trap.fish.matrix[,1]) / X.tot.new.wild

}
' }

if(win_model == 'pois') {
  model_code =
    'model {

  ####################################
  # Set up parameters and initial states...
  ####################################
  X.sigma ~ dunif(0, 20) # process error in log space
  X.tau <- pow(X.sigma, -2)
  X.log.all[1] ~ dunif(0,10) # initial state in log space

  # modeling proportion of fish available for window counts
  win.prop.true.logit[1] ~ dnorm(0, 0.001)	# daytime ascension rate for week 1
  win.prop.sigma ~ dunif(0, 10) # process error on window proportion correction
  win.prop.tau <- pow(win.prop.sigma, -2)

  # modeling proportion of fish re-ascending the dam
  reasc.true.logit[1] ~ dnorm(0, 0.001) # re-ascension rate for week 1
  reasc.sigma ~ dunif(0, 10) # process error on re-ascension proportion correction
  reasc.tau <- pow(reasc.sigma, -2)

  # modeling proportion of fish that are wild, hatchery no-clip and hatchery
  # set probability for any type of fish that was never caught to 0
  # prior on log odds ratio for initial week
  for(j in 1:2) {
  org.phi[1,j] ~ dnorm(0, 0.01)
  exp.org.phi[1,j] <- exp(org.phi[1,j]) * org.exist[j]
  }

  # set hatchery as baseline
  for(i in 1:TotLadderWeeks) {
  org.phi[i,3] <- 0
  exp.org.phi[i,3] <- exp(org.phi[i,3]) * org.exist[3]
  # get sum of all phis
  sum.exp.phi[i] <- sum(exp.org.phi[i,])
  }

  # extract initial movement probabilities for week 1
  for(j in 1:3) {
  org.prop[1,j] <- ifelse(org.exist[j] == 0, 0, exp.org.phi[1,j] / sum.exp.phi[1])
  }

  # variation in time-varying random walk movement probabilities
  org.sigma ~ dunif(0, 10)
  org.tau <- pow(org.sigma, -2)

  for(i in 2:TotLadderWeeks) {
  for(j in 1:2) {
  epsilon[i,j] ~ dnorm(0, org.tau)
  # set phi for any type of fish that was never caught to 0
  org.phi[i,j] <- ifelse(org.exist[j] == 0, 0, org.phi[i - 1, j] + epsilon[i,j])
  exp.org.phi[i,j] <- exp(org.phi[i,j]) * org.exist[j]
  }

  for (j in 1:3) {
  org.prop[i,j] <- exp.org.phi[i,j] / sum.exp.phi[i]
  }
  }

  # parameter clean-up
  for(i in 1 :TotLadderWeeks) {
  win.prop.true[i] <- ilogit(win.prop.true.logit[i])
  reasc.true[i] <- ilogit(reasc.true.logit[i])
  }

  ####################################
  ## True state of nature
  ####################################

  for(i in 2:TotLadderWeeks) {
  # random walks
  X.log.all[i] ~ dnorm(X.log.all[i-1], X.tau)
  win.prop.true.logit[i] ~ dnorm(win.prop.true.logit[i-1], win.prop.tau)
  reasc.true.logit[i] ~ dnorm(reasc.true.logit[i-1], reasc.tau)
  }

  # derived parameters
  for(i in 1:TotLadderWeeks) {
  true.prop[i] <- win.prop.true[i]

  X.all[i] <- round(exp(X.log.all[i]))
  X.day[i] <- round(X.all[i] * true.prop[i])
  X.night[i] <- X.all[i] - X.day[i]
  X.reasc[i] <- round(X.all[i] * reasc.true[i])

  X.all.wild[i] <- round(X.all[i] * org.prop[i,1])
  X.all.hnc[i] <- round(X.all[i] * org.prop[i,2])
  X.all.hatch[i] <- round(X.all[i] * org.prop[i,3])

  X.new.wild[i] <- round(X.all[i] * org.prop[i,1] * (1-reasc.true[i]))
  X.new.hnc[i] <- round(X.all[i] * org.prop[i,2] * (1-reasc.true[i]))
  X.new.hatch[i] <- round(X.all[i] * org.prop[i,3] * (1-reasc.true[i]))

  X.reasc.wild[i] <- X.all.wild[i] - X.new.wild[i]
  X.night.wild[i] <- X.new.wild[i] * (1-true.prop[i])

  }

  ####################################
  ## What we observe
  ####################################

  for(i in 1:TotLadderWeeks) {
  # at window: Poisson
  Y.window[i] ~ dpois(X.day[i])

  # in trap
  # uncertainty in trap rate
  trap.rate.true[i] ~ dbeta(trap.alpha[i], trap.beta[i])
  Y.trap[i] ~ dbin(trap.rate.true[i], X.all[i])

  # fish in trap by origin
  trap.fish.matrix[i,1:3] ~ dmulti(org.prop[i,], trap.fish[i])

  # day-time tags
  DC.tags[i] ~ dbin(win.prop.true[i], Tot.tags[i])

  # re-ascension tags
  ReAsc.tags[i] ~ dbin(reasc.true[i], Tot.tags[i])
  }

  ####################################
  ## Summary statistics
  ####################################

  X.tot.all <- sum(X.all)
  X.tot.day <- sum(X.day)
  X.tot.night <- sum(X.night)
  X.tot.reasc <- sum(X.reasc)

  X.tot.all.wild <- sum(X.all.wild)
  X.tot.all.hatch <- sum(X.all.hatch)
  X.tot.all.hnc <- sum(X.all.hnc)

  X.tot.new.wild <- sum(X.new.wild)
  X.tot.new.hatch <- sum(X.new.hatch)
  X.tot.new.hnc <- sum(X.new.hnc)

  X.tot.night.wild <- sum(X.night.wild)
  X.tot.reasc.wild <- sum(X.reasc.wild)

  prop.tagged <- sum(trap.fish.matrix[,1]) / X.tot.new.wild

}
' }


  cat(model_code,
      file = file_name)

  model_file = readLines(file_name)

  writeLines(model_file, file_name)

}
