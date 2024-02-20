#' @title Extract STADEM Posteriors
#'
#' @description Extracts the MCMC posteriors of selected parameters from a STADEM model, and formats them in a long format.
#'
#' @author Kevin See
#'
#' @param stadem_mod The result of `runSTADEMmodel()`. An MCMC.list, or \code{jagsUI} object that can be coerced to an MCMC.list.
#' @param param_nms Character vector with the name of the parameter corresponding to total escapement. Default is focused on wild fish, `X.new.wild`.
#'
#' @import dplyr tidyr stringr
#' @export
#' @return NULL
#' @examples extractPost()

extractPost = function(stadem_mod = NULL,
                       param_nms = 'X.new.wild') {

  stopifnot(!is.null(stadem_mod))

  # make sure dabom_mod is mcmc.list
  if(inherits(stadem_mod, "jagsUI")) {
    stadem_mod = stadem_mod$samples
  }

  stopifnot(!is.null(stadem_mod),
            inherits(stadem_mod, c('mcmc', 'mcmc.list')))


  stadem_post <-
    stadem_mod |>
    as.matrix(iters = T,
              chains = T) |>
    dplyr::as_tibble() |>
    dplyr::select(chain = CHAIN,
                  iter = ITER,
                  contains(param_nms)) |>
    dplyr::group_by(chain) |>
    dplyr::mutate(iter = 1:n()) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(cols = -c(chain, iter),
                        names_to = "param",
                        values_to = "value") |>
    dplyr::mutate(strata_num = stringr::str_extract(param, '[:digit:]+'),
                  dplyr::across(
                    strata_num,
                    as.integer
                  ),
                  dplyr::across(param,
                                ~ str_split(., "\\[", simplify = T)[,1])) |>
    # dplyr::group_by(param) |>
    # dplyr::mutate(iter = 1:n()) |>
    # dplyr::ungroup() |>
    dplyr::select(chain,
                  iter,
                  param,
                  strata_num,
                  tot_abund = value)

  return(stadem_post)
}
