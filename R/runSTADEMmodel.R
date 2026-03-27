#' @title Run JAGS model
#'
#' @description Run the JAGS model
#'
#' @author Kevin See
#'
#' @inheritParams writeJAGSmodel
#' @param mcmc_chainlength number of total samples in each MCMC chain
#' @param mcmc_burn number of burn-in samples in each MCMC chain
#' @param mcmc_thin thinning interval for MCMC samples to save
#' @param mcmc_chains number of MCMC chains
#' @param jags_data list of data being passed to \code{JAGS} model, created by \code{prepJAGS}
#' @param weekly_params should weekly estimates be saved? Default is \code{FALSE}
#' @param seed starting seed value for RNG, to make results reproducible
#' @param verbose passed to the \code{jags} function
#' @param parallel passed to the \code{jags} function
#' @param DIC passed to the \code{jags} function
#' @param use_jagsUI should the `jagsUI` package be used to fit the model? Default is `FALSE`, meaning the `rjags` package is used instead.
#'
#' @import rjags
#' @export
#' @return mcmc.list
#' @examples #runSTADEMmodel()

runSTADEMmodel = function(file_name = NULL,
                          mcmc_chainLength = 100,
                          mcmc_burn = 10,
                          mcmc_thin = 2,
                          mcmc_chains = 1,
                          jags_data = NULL,
                          weekly_params = FALSE,
                          seed = NULL,
                          verbose = FALSE,
                          parallel = TRUE,
                          DIC = FALSE,
                          win_model = c('neg_bin', 'neg_bin2', 'pois', 'quasi_pois', 'log_space'),
                          trap_est = TRUE,
                          use_jagsUI = FALSE) {

  # check if jagsUI is installed
  if( !requireNamespace('jagsUI', quietly = T)) {
    stop("Package \"jagsUI\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  cat('Running JAGS model\n')

  # which distribution is used to model window counts?
  win_model = match.arg(win_model)

  # write the JAGS model
  STADEM::writeJAGSmodel(file_name,
                         win_model,
                         trap_est)


  # add log of window counts, if needed, to jags_data
  if(win_model == 'log_space') jags_data$Y.window.log = log(jags_data$Y.window)

  # which parameters to save
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
                  'X.sigma',
                  'day.sigma',
                  'reasc.sigma',
                  'org.sigma',
                  'prop.tagged')

  if(win_model == 'neg_bin')  jags_params = c(jags_params, 'r', 'k')
  if(win_model == 'neg_bin2')  jags_params = c(jags_params, 'theta', 'omega')
  if(win_model == 'quasi_pois')  jags_params = c(jags_params, 'qp.sigma')
  if(win_model == 'log_space')  jags_params = c(jags_params, 'win.sigma')

  # add some weekly parameters if desired
  if(weekly_params) jags_params = c(jags_params,
                                    'X.all',
                                    'X.day',
                                    'X.night',
                                    'X.reasc',
                                    'X.all.wild',
                                    'X.all.hatch',
                                    'X.all.hnc',
                                    'X.new.tot',
                                    'X.new.wild',
                                    'X.new.hatch',
                                    'X.new.hnc',
                                    'X.night.wild',
                                    'X.reasc.wild',
                                    'day.true',
                                    'reasc.true',
                                    'org.prop',
                                    'trap.rate.true')

  # set initial values
  if(!is.null(seed)) set.seed(seed)
  jags_inits = vector('list', mcmc_chains)
  for(i in 1:mcmc_chains) jags_inits[[i]] = jagsInits(jags_data)

  # ptm = proc.time()
  if(use_jagsUI) {
    jags_model = try(jagsUI::jags(data = jags_data,
                                  inits = jags_inits,
                                  parameters.to.save = jags_params,
                                  model.file = file_name,
                                  n.chains = mcmc_chains,
                                  n.burnin = mcmc_burn,
                                  n.thin = mcmc_thin,
                                  n.iter = mcmc_chainLength,
                                  parallel = parallel,
                                  DIC = DIC,
                                  verbose = verbose))
  } else {

    adapt_mod <-
      rjags::jags.model(file = file_name,
                        data = jags_data,
                        inits = jags_inits,
                        n.chains = mcmc_chains,
                        n.adapt = mcmc_burn)

    jags_model <-
      rjags::coda.samples(model = adapt_mod,
                          variable.names = jags_params,
                          n.iter = mcmc_chainLength,
                          thin = mcmc_thin)
  }
  # proc.time() - ptm # returns the CPU time used

  return(jags_model)

}
