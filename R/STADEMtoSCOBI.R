#' @title Summarise STADEM output for SCOBI
#'
#' @description Format STADEM output into a form that can be fed into the SCOBI package for further run decomposition.
#'
#' @author Kevin See
#'
#' @param stadem_mod results of STADEM model, of class \code{mcmc.list} or `jagsUI`.
#' @param lgr_weekly weekly data from Lower Granite dam, compiled by \code{compileGRAdata}.
#' @param saveCSV should results be saved as a .csv file? Default is \code{FALSE}.
#' @param fileName if \code{saveCSV} is \code{TRUE}, what file name, including the path and extension, should the results be written to?
#'
#' @import stringr dplyr readr
#' @export
#' @return NULL

STADEMtoSCBOI = function(stadem_mod = NULL,
                         lgr_weekly = NULL,
                         saveCSV = F,
                         fileName = 'SCOBI_input.csv') {

  stopifnot(!is.null(stadem_mod) | !is.null(lgr_weekly))

  # make sure stadem_mod is mcmc.list
  if(inherits(stadem_mod, "jagsUI")) {
    stadem_mod = stadem_mod$samples
  }

  stopifnot(!is.null(stadem_mod),
            inherits(stadem_mod, c('mcmc', 'mcmc.list')))

  week_est = summary(stadem_mod)$statistics |>
    tibble::as_tibble(rownames = "var") |>
    dplyr::mutate(week = as.integer(stringr::str_extract(var, "[0-9]+")),
                  param = stringr::str_extract_all(var, "[:alpha:]+", simplify = T)[,3]) %>%
    dplyr::select(var, param, week, everything()) %>%
    dplyr::left_join(stadem_list[['weeklyData']],
                     by = c('week' = 'week_num'))

  scobi_input = week_est %>%
    dplyr::mutate(Strata = lubridate::week(Start_Date)) %>%
    dplyr::select(model_week = week,
                  StartDate = Start_Date,
                  Strata,
                  Estimate = Mean,
                  SE = SD) %>%
    dplyr::mutate(
      dplyr::across(Estimate,
                    ~ round(.,,
                            digits = 0))) %>%
    dplyr::mutate(Collapse = NA)

  if(saveCSV) {
    readr::write_csv(scobi_input,
                     fileName)
  }

  return(scobi_input)
}
