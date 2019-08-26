#' @title Summarise STADEM output for SCOBI
#'
#' @description Format STADEM output into a form that can be fed into the SCOBI package for further run decomposition.
#'
#' @author Kevin See
#'
#' @param stadem_mod results of STADEM model, of class \code{jagsUI}.
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

  week_est = stadem_mod$summary[grep('^X.new.tot', rownames(stadem_mod$summary)),] %>%
    as.data.frame() %>%
    mutate(var = rownames(.),
           week = as.integer(stringr::str_extract(var, "[0-9]+")),
           param = stringr::str_extract_all(var, "[:alpha:]+", simplify = T)[,3]) %>%
    tbl_df() %>%
    select(var, param, week, everything()) %>%
    left_join(stadem_list[['weeklyData']],
              by = c('week' = 'week_num'))

  scobi_input = week_est %>%
    mutate(Strata = lubridate::week(Start_Date)) %>%
    select(model_week = week,
           StartDate = Start_Date,
           Strata,
           Estimate = mean,
           SE = sd) %>%
    mutate_at(vars(Estimate),
              list(round),
              digits = 0) %>%
    mutate(Collapse = NA)

  if(saveCSV) {
    readr::write_csv(scobi_input,
                     fileName)
  }

  return(scobi_input)
}
