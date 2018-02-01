#' @title PIT tag data
#'
#' @description Query and download PIT tag data about night passage and re-ascension at particular dams for specific species and year, using DART.
#'
#' @author Kevin See
#'
#' @param damPIT the dam code for the dam you wish to query for PIT tag data. Currently only available for Lower Granite Dam (GRA).
#' @param spp species to query window counts for. Possible choices are: Chinook, Coho, Steelhead, Sockeye
#' @param spawn_yr spawn year to query for window counts.
#' @param start_day date (\code{month / day}) when query should start
#' @param end_day date (\code{month / day}) when query should end
#'
#' @source \url{http://www.cbr.washington.edu/dart}
#'
#' @import lubridate httr dplyr readr
#' @export
#' @return NULL
#' @examples queryPITtagData(spawn_yr = 2015)

queryPITtagData = function(damPIT = 'GRA',
                           spp = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                           spawn_yr = NULL,
                           start_day = NULL,
                           end_day = NULL) {

  # need a year, and only dam allowed in Lower Granite (GRA)
  stopifnot(damPIT == 'GRA', !is.null(spawn_yr))

  # pull out default dam and species code
  damPIT = match.arg(damPIT)
  spp = match.arg(spp, several.ok = F)

  # set up default start and end days
  if(damPIT == 'GRA' & spp == 'Chinook' & is.null(start_day)) start_day = '03/01'
  if(damPIT == 'GRA' & spp == 'Chinook' & is.null(end_day)) end_day = '08/17'

  if(damPIT == 'GRA' & grepl('Steelhead', spp) & is.null(start_day)) start_day = '07/01'
  if(damPIT == 'GRA' & grepl('Steelhead', spp) & is.null(end_day)) end_day = '06/30'

  # match up species code with species name
  spp_code_df = tibble(Species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                       code = 1:4)

  spp_code = spp_code_df$code[match(spp, spp_code_df$Species)]

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/KevinSee/damEscapement')

  # compose url with query
  url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/pit_adult_window_new.php'

  # build query for DART
  queryList = list(type = 'tagid',
                   outputFormat = 'csv',
                   year = spawn_yr,
                   site = damPIT,
                   species = spp_code,
                   span = 'no',
                   startdate = start_day,
                   enddate = end_day)

  if(grepl('Steelhead', spp)) {
    queryList[['span']] = 'yes'
    queryList = c(queryList,
                  list(syear = spawn_yr - 1,
                       eyear = spawn_yr))
  }


  # send query to DART
  web_req = httr::GET(url_req, ua,
                      query = queryList)

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from DART')

  # what encoding to use?
  # stringi::stri_enc_detect(content(web_req, "raw"))

  # parse the response
  parsed = httr::content(web_req,
                         'text') %>%
    readr::read_delim(delim = ',',
                      col_names = T)

  if(is.null(parsed)) {
    message(paste('DART returned no data for', spp, 'in', spawn_yr, '\n'))
    stop
  }

  if(class(parsed)[1] == 'xml_document') {
    message(paste('For', spp, 'in', spawn_yr, 'XML document returned by DART instead of data\n'))
    stop
  }

  if (httr::status_code(web_req) != 200) {
    message(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>",
        status_code(web_req),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
    stop
  }

  pit_df = parsed %>%
    mutate(Date = lubridate::ymd(Date),
           `Detection DateTime` = lubridate::ymd_hms(`Detection DateTime`)) %>%
    filter(Ladder == damPIT) %>%
    rename(SpCode = Species) %>%
    mutate(Species = spp,
           Year = spawn_yr) %>%
    arrange(Date, TagID, `Detection DateTime`) %>%
    select(Ladder, Year, Species, SpCode, TagID, everything())

  names(pit_df) = gsub(' ', '', names(pit_df))

  return(pit_df)
}
