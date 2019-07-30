#' @title PIT tag data
#'
#' @description Query and download PIT tag data about night passage and re-ascension at particular dams for specific species and year, using DART.
#'
#' @author Kevin See
#'
#' @param damPIT the dam code for the dam you wish to query for PIT tag data. Currently only available for Lower Granite Dam (GRA) and Priest Rapids (PRA).
#' @param spp species to query PIT tag data for. Possible choices are: Chinook, Coho, Steelhead, Sockeye
#' @param start_date character vector of date (\code{YYYYMMDD}) when query should start
#' @param end_date character vector of date (\code{YYYYMMDD}) when query should end
#'
#' @source \url{http://www.cbr.washington.edu/dart}
#'
#' @import lubridate httr dplyr readr
#' @export
#' @return NULL
#' @examples queryPITtagData(start_date = '20150701')

queryPITtagData = function(damPIT = c('GRA', 'PRA'),
                           spp = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                           start_date = NULL,
                           end_date = NULL) {

  # need a start date
  stopifnot(!is.null(start_date))

  # pull out default dam and species code
  damPIT = match.arg(damPIT)
  spp = match.arg(spp, several.ok = F)

  # set default end date (1 year after start date)
  if(is.null(end_date)) end_date = format(lubridate::ymd(start_date) + years(1) - days(1), '%Y%m%d')

  # turn start / end date character vectors into actual date objects
  startDate = lubridate::ymd(start_date)
  endDate = lubridate::ymd(end_date)

  if(endDate < startDate) stop('start_date comes after end_date')

  # get character vectors of start day and end day for query
  start_day = paste(lubridate::month(startDate), lubridate::day(startDate), sep = '/')
  end_day = paste(lubridate::month(endDate), lubridate::day(endDate), sep = '/')

  # match up species code with species name
  spp_code_df = tibble(Species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                       code = 1:4)

  spp_code = spp_code_df$code[match(spp, spp_code_df$Species)]

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/KevinSee/STADEM')

  # compose url with query
  if(damPIT == 'GRA') url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/pit_adult_window_new.php'

  if(damPIT == 'PRA') url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/pit_adult_ladder_pra.php'

  # build query for DART
  queryList = list(type = 'tagid',
                   outputFormat = 'csv',
                   year = lubridate::year(startDate),
                   site = damPIT,
                   species = spp_code,
                   span = 'no',
                   startdate = start_day,
                   enddate = end_day)

  # if(grepl('Steelhead', spp)) {
  if(lubridate::year(endDate) > lubridate::year(startDate)) {
    queryList[['span']] = 'yes'
    queryList = c(queryList,
                  list(syear = lubridate::year(startDate),
                       eyear = lubridate::year(endDate)))
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
  parsed = suppressWarnings(
    httr::content(web_req,
                         'text',
                  encoding = 'UTF-8') %>%
    readr::read_delim(delim = ',',
                      col_names = T)
  )

  #if(is.null(parsed) | grepl(paste('No', spp, 'data found'), parsed)) {
    if(is.null(parsed) | ncol(parsed) == 1) {
    stop(paste('DART returned no PIT tag data for', spp, 'in', lubridate::year(startDate), '\n'))
  }

  if(class(parsed)[1] == 'xml_document') {
    stop(paste('For', spp, 'in', lubridate::year(startDate), 'XML document returned by DART instead of data\n'))
  }

  if (httr::status_code(web_req) != 200) {
    stop(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>",
        httr::status_code(web_req),
        parsed$message,
        parsed$documentation_url
      ))
  }

  pit_df = parsed %>%
    filter(!is.na(Date)) %>%
    filter(Date != 'Date') %>%
    mutate(Date = lubridate::ymd(Date),
           `Detection DateTime` = lubridate::ymd_hms(`Detection DateTime`)) %>%
    # filter(Ladder == damPIT) %>%
    rename(SpCode = Species) %>%
    mutate(Species = spp,
           Year = year(endDate)) %>%
    arrange(Date, TagID, `Detection DateTime`) %>%
    select(Ladder, Year, Species, SpCode, TagID, everything()) %>%
    filter(Date >= startDate,
           Date <= endDate)

  names(pit_df) = gsub(' ', '', names(pit_df))

  if(damPIT == 'PRA') {
    pit_df = pit_df %>%
      rename(PreviousDate = PreviousDetectDate,
             PreviousHours = DiffPreviousHours,
             PreviousDays = DiffPreviousDays,
             LadderSide = Ladder_1)

  }


  return(pit_df)
}
