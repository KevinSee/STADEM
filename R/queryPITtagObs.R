#' @title PIT tag data
#'
#' @description Query and download data about PIT tags observed crossing a particular site, using DART.
#'
#' @author Kevin See
#'
#' @param site The site to query observations for.
#' @param spp species to query PIT tag observations for. Possible choices are: Chinook, Coho, Steelhead, Sockeye
#' @param start_date character vector of date (\code{YYYYMMDD}) when query should start
#' @param end_date character vector of date (\code{YYYYMMDD}) when query should end
#'
#' @source \url{http://www.cbr.washington.edu/dart}
#'
#' @import lubridate readr httr dplyr
#' @export
#' @return NULL
#' @examples queryPITtagObs(yr = 2013)

queryPITtagObs = function(site = 'GRA',
                          spp = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                          start_date = NULL,
                          end_date = NULL) {

  # need a start date
  stopifnot(!is.null(start_date))

  # set default end date (1 year after start date)
  if(is.null(end_date)) end_date = format(lubridate::ymd(start_date) + years(1) - days(1), '%Y%m%d')

  # set year to match end_date
  yr = lubridate::year(lubridate::ymd(end_date))

  if(site != 'GRA') stop('Query only set up for GRA at this time')
  if(site == 'GRA') proj = 'GRA:Lower Granite Dam Adult Fishway (GRA) rkm 522.173'

  # default values
  spp = match.arg(spp)

  # match up species code with species name
  spp_code_df = data.frame(Species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                           code = 1:4)

  spp_code = spp_code_df$code[match(spp, spp_code_df$Species)]

  # turn start / end date character vectors into actual date objects
  startDate = lubridate::ymd(start_date)
  endDate = lubridate::ymd(end_date)

  if(endDate < startDate) stop('start_date comes after end_date')

  # get character vectors of start day and end day for query
  start_day = paste(lubridate::month(startDate), lubridate::day(startDate), sep = '/')
  end_day = paste(lubridate::month(endDate), lubridate::day(endDate), sep = '/')

  span_yrs = if_else(lubridate::year(startDate) != lubridate::year(endDate),
                     'yes', 'no')

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/KevinSee/STADEM')

  # compose url with query
  url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/pitall_obs_de.php?'

  # build query for DART
  queryList = list(sc = 1,
                   queryName = 'pit_obs_de',
                   stage = 'A',
                   outputFormat = 'csv',
                   year = yr,
                   # proj = proj,
                   proj = site,
                   species = spp_code,
                   run = NULL,
                   rear_type = NULL,
                   span = span_yrs,
                   startdate = start_day,
                   enddate = end_day,
                   reltype = 'alpha',
                   summary = 'yes')


  if(span_yrs == 'yes') {
    queryList[['span']] = 'yes'
    queryList = c(queryList,
                  list(syear = yr - 1,
                       eyear = yr))
    queryList[['year']] = yr - 1
  }

  # send query to DART
  web_req = httr::GET(url_req,
                      ua,
                      query = queryList)

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from DART')

  # parse the response
  parsed = try(suppressWarnings(
    httr::content(web_req,
                  'text',
                  encoding = 'ISO-8859-1') %>%
      readr::read_delim(delim = ',',
                        col_names = T) %>%
      dplyr::filter(!is.na(`Tag ID`)) %>%
      dplyr::mutate(Year = as.integer(Year))
  ))

  if(class(parsed)[1] == 'try-error') stop(paste('DART returned no PIT tag observations for', spp, 'in', yr))

  names(parsed) = gsub(' ', '', names(parsed))

  return(parsed)
}


