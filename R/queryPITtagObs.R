#' @title PIT tag data
#'
#' @description Query and download data about PIT tags observed crossing a particular site, using DART.
#'
#' @author Kevin See
#'
#' @param site The site to query observations for. Query only set up for Lower Granite Dam Adult Fishway (GRA) and Priest Rapids fishway (PRA) at this time.
#' @param spp species to query PIT tag observations for. Possible choices are: `Chinook`, `Coho`, `Steelhead`, `Sockeye`
#' @param start_date character vector of date (\code{YYYYMMDD}) when query should start
#' @param end_date character vector of date (\code{YYYYMMDD}) when query should end
#'
#' @source \url{https://www.cbr.washington.edu/dart}
#'
#' @import lubridate httr2 dplyr vroom janitor
#' @export
#' @return NULL
#' @examples queryPITtagObs(spp = "Steelhead", start_date = "20180701")

queryPITtagObs = function(site = c('GRA', 'PRA'),
                          spp = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                          start_date = NULL,
                          end_date = NULL) {

  site <- match.arg(site)
  if(!site %in% c('GRA', 'PRA')) stop('Query only set up for GRA and PRA at this time')

  # default values
  spp = match.arg(spp)

  # match up species code with species name
  spp_code <- dplyr::case_when(spp == "Chinook" ~ 1,
                               spp == "Coho" ~ 2,
                               spp == "Steelhead" ~ 3,
                               spp == "Sockeye" ~ 4,
                               .default = NA_real_)
  if(is.na(spp_code)) {
    stop("Species name not part of this query")
  }

  # need a start date
  stopifnot(!is.null(start_date))

  # set default end date (1 year after start date)
  if(is.null(end_date)) {
    end_date = format(lubridate::ymd(start_date) + years(1) - days(1), '%Y%m%d')
  }

  # turn start / end date character vectors into actual date objects
  startDate = lubridate::ymd(start_date)
  endDate = lubridate::ymd(end_date)

  if(endDate < startDate) stop('start_date comes after end_date')

  # get character vectors of start day and end day for query
  start_day = paste(lubridate::month(startDate), lubridate::day(startDate), sep = '/')
  end_day = paste(lubridate::month(endDate), lubridate::day(endDate), sep = '/')

  span_yrs = dplyr::if_else(lubridate::year(startDate) != lubridate::year(endDate),
                            'yes', 'no')

  # set year to match end_date
  yr = lubridate::year(lubridate::ymd(end_date))

  # assign user agent to the GitHub repo for this package
  # ua = httr::user_agent('https://github.com/KevinSee/STADEM')
  ua = 'https://github.com/KevinSee/STADEM'

  # compose url with query
  url_req = 'https://www.cbr.washington.edu/dart/cs/php/rpt/pitall_obs_de.php?'

  # build query for DART
  queryList = list(sc = 1,
                   queryName = 'pit_obs_de',
                   stage = 'A',
                   outputFormat = 'csv',
                   year = lubridate::year(startDate),
                   proj = site,
                   species = spp_code,
                   run = NULL,
                   rear_type = NULL,
                   span = 'no',
                   startdate = start_day,
                   enddate = end_day,
                   reltype = 'alpha',
                   summary = 'yes')


  if(lubridate::year(endDate) > lubridate::year(startDate)) {
    queryList[['span']] = 'yes'
    queryList = c(queryList,
                  list(syear = lubridate::year(startDate),
                       eyear = lubridate::year(endDate)))
  }

  # construct a request
  req <-
    httr2::request(url_req) |>
    httr2::req_user_agent(ua) |>
    httr2::req_url_query(!!!queryList) |>
    httr2::req_retry(max_tries = 3,
              retry_on_failure = T)

  # req |>
  #   req_dry_run()

  # send query to DART
  resp <-
    req |>
    httr2::req_perform()

  # resp |>
  #   resp_status_desc()

  if(httr2::resp_status(resp) != 200) {
    message(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>",
        httr2::resp_status(resp),
        httr2::resp_status_desc(resp),
        resp$url
      ))
    stop()
  }

  if(httr2::resp_content_type(resp) != "text/csv") {
    warning(paste('DART returned no PIT tag observations for', spp, 'in', yr, ". Returning an empty data frame."))  # Optional: show a warning instead of stopping
    parsed = data.frame()  # Assign an empty data frame
    return(parsed)
  }

  # parse the response
  parsed <-
    resp |>
    httr2::resp_body_string(encoding = 'UTF-8') |>
    vroom::vroom(delim = ',',
                 col_names = T,
                 show_col_types = FALSE) |>
    clean_names("upper_camel") |>
    filter(!is.na(TagFile)) |>
    suppressWarnings() |>
    mutate(across(Year,
                  as.integer)) |>
    dplyr::rename(TagCode = TagId) |>
    arrange(ObsTime)

  return(parsed)
}


