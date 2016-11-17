#' @title Initial values
#'
#' @description Function to set appropriate initial values
#'
#' @author Kevin See
#'
#' @param jags_data list of data being passed to \code{JAGS} model
#'
#' @export
#' @return NULL
#'
jagsInits = function(jags_data) {
  return(list('X.log.all' = log(jags_data$Y.window + 1),
              'trap.rate.true' = with(jags_data, trap.alpha / (trap.alpha + trap.beta) + 0.0001)))
}
