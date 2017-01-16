#' @title Modified Lincoln-Petersen (Chapman) estimator
#'
#' @description Estimate total abundance from a 2 pass mark - recapture event
#'
#' @author Kevin See
#'
#' @param data data.frame containing columns labeled \code{M}, \code{C} and \code{R} for marks, captures and recaptures
#' @param rmInvalid if estimates are deemed invalid by Robson & Regier criteria, make them NA
#'
#' @export
#' @return NULL

chapmanMR = function(data,
                     rmInvalid = TRUE){

  N.hat = with(data, ((M + 1) * (C + 1)) / (R + 1) -1)
  N.hat.SE = with(data, sqrt(((M + 1) * (C + 1) * (M - R) * (C - R)) / ((R + 1)^2 * (R + 2))))
  p.hat = data$M / N.hat
  p.hat.se = sqrt(N.hat.SE^2 * (data$M / N.hat^2)^2)
  # using Robson & Regier criteria for valid abundance estimates
  Valid = with(data, (M * C)) > (N.hat * 4)
  # Valid = (data$M * data$C) > (data$N.hat * 4) | data$R >= 7
  if(rmInvalid==T){
    N.hat[Valid==F] = NA
    N.hat.SE[Valid==F] = NA
    p.hat[Valid==F]= NA
    p.hat.se[Valid==F]= NA
  }
  return(data.frame(N = N.hat,
                    N_se = N.hat.SE,
                    p = p.hat,
                    p_se = p.hat.se,
                    Valid = Valid))
}
