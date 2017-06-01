#' @title PIT tag data
#'
#' @description Query and download data about PIT tags observed crossing a particular site, using DART.
#'
#' @author Kevin See
#'
#' @param site The site to query observations for.
#' @param spp species to query window counts for. Possible choices are: Chinook, Coho, Steelhead, Sockeye
#' @param yr year to query for window counts.
#' @param start_day date (\code{month / day}) when query should start
#' @param end_day date (\code{month / day}) when query should end
#' @param span_yrs Should the query span mulitple years? If \code{NULL}, default value is chosen based on species.
#'
#' @source \url{http://www.cbr.washington.edu/dart}
#'
#' @import lubridate readr httr dplyr
#' @export
#' @return NULL
#' @examples queryPITtagObs(yr = 2013)

queryPITtagObs = function(site = 'GRA',
                          spp = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                          yr = NULL,
                          start_day = NULL,
                          end_day = NULL,
                          span_yrs = NULL) {

  # need a year
  stopifnot(!is.null(yr))

  if(site == 'GRA') proj = 'GRA:Lower Granite Dam Adult Fishway (GRA) rkm 522.173'

  # default values
  spp = match.arg(spp)

  # match up species code with species name
  spp_code_df = data.frame(Species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                           code = 1:4)

  spp_code = spp_code_df$code[match(spp, spp_code_df$Species)]

  # set up default start and end days
  if(site == 'GRA' & spp == 'Chinook' & is.null(start_day)) start_day = '03/01'
  if(site == 'GRA' & spp == 'Chinook' & is.null(end_day)) end_day = '08/17'

  if(site == 'GRA' & grepl('Steelhead', spp) & is.null(start_day)) start_day = '07/01'
  if(site == 'GRA' & grepl('Steelhead', spp) & is.null(end_day)) end_day = '06/30'

  if(is.null(span_yrs)) {
    if(spp == 'Steelhead') span_yrs = T
    if(spp != 'Steelhead') span_yrs = F
  }


  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/KevinSee/damEscapement')

  # compose url with query
  url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/pitall_obs_de.php?'

  # build query for DART
  queryList = list(sc = 1,
                   queryName = 'pit_obs_de',
                   stage = 'A',
                   outputFormat = 'csv',
                   year = yr,
                   proj = proj,
                   species = spp_code,
                   run = NULL,
                   rear_type = NULL,
                   span = ifelse(span_yrs, 'yes', 'no'),
                   startdate = start_day,
                   enddate = end_day,
                   reltype = 'alpha',
                   summary = 'yes')


  if(span_yrs) {
    queryList[['span']] = 'yes'
    queryList = c(queryList,
                  list(syear = yr - 1,
                       eyear = yr))
  }

  # send query to DART
  web_req = httr::GET(url_req,
                      ua,
                      query = queryList)

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from DART')

  # parse the response
  parsed = httr::content(web_req,
                         'text',
                         encoding = 'ISO-8859-1') %>%
    read_delim(delim = ',',
               col_names = T) %>%
    dplyr::filter(!is.na(`Tag ID`)) %>%
    dplyr::mutate(Year = as.integer(Year))

  names(parsed) = gsub(' ', '', names(parsed))

  return(parsed)
}


