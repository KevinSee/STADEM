#' @title Initial values
#'
#' @description Function to set appropriate initial values
#'
#' @author Kevin See
#'
#' @param jags_data list of data being passed to \code{JAGS} model
#' @param params vector describing which parameters should have an initial value function applied to them. Currently accepts one or more of \code{X.log.all}, \code{trap.rate.true}. Default is all of them.
#'
#' @export
#' @return NULL
#'
jagsInits = function(jags_data,
                     params = c('X.log.all',
                                'trap.rate.true')) {
  init_list = NULL
  if('X.log.all' %in% params) {
    x_log_all = log(jags_data$Y.window + rpois(length(jags_data$Y.window), 10))
    x_log_all[1] = NA

    init_list = c(init_list,
                  list('X.log.all' = x_log_all,
                       'X.all.prior1' = jags_data$Y.window[1] + rpois(1, 10)))
  }

  if('trap.rate.true' %in% params) {
    init_list = c(init_list,
                  list('trap.rate.true' = with(jags_data, n.trap.tags / n.poss.tags + 0.0001)))
  }

  return(init_list)
}
