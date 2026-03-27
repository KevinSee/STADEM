#' @title PIT tag data
#'
#' @description Query and download PIT tag data about night passage and re-ascension at particular dams for specific species and year, using DART.
#'
#' @author Kevin See
#'
#' @param damPIT the dam code for the dam you wish to query for PIT tag data. Currently only available for Lower Granite Dam (GRA), Priest Rapids (PRA), Rock Island (RIA) and Rocky Reach (RRF).
#' @inheritParams queryPITtagObs
#'
#' @source \url{https://www.cbr.washington.edu/dart}
#'
#' @import lubridate httr2 dplyr vroom janitor
#' @export
#' @return NULL
#' @examples queryPITtagData(spp = "Steelhead", start_date = "20180701")

queryPITtagData = function(damPIT = c('GRA', 'PRA', 'RIA', 'RRF'),
                           spp = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                           start_date = NULL,
                           end_date = NULL) {

  damPIT <- match.arg(damPIT)

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
    end_date = format(min(lubridate::today(),
                          lubridate::ymd(start_date) + years(1) - days(1)),
                      '%Y%m%d')
  }

  # turn start / end date character vectors into actual date objects
  startDate = lubridate::ymd(start_date)
  endDate = lubridate::ymd(end_date)

  if(endDate < startDate) stop('start_date comes after end_date')

  # get character vectors of start day and end day for query
  start_day = paste(lubridate::month(startDate), lubridate::day(startDate), sep = '/')
  end_day = paste(lubridate::month(endDate), lubridate::day(endDate), sep = '/')

  # assign user agent to the GitHub repo for this package
  # ua = httr::user_agent('https://github.com/KevinSee/STADEM')
  ua = 'https://github.com/KevinSee/STADEM'

  # compose url with query
  if(damPIT == 'GRA') {
    url_req = 'https://www.cbr.washington.edu/dart/cs/php/rpt/pit_adult_window_new.php'
  } else if(damPIT %in% c('PRA', "RIA", "RRF")){
    url_req = 'https://www.cbr.washington.edu/dart/cs/php/rpt/pit_adult_ladder_upcol.php'
  }

  # build query for DART
  queryList = list(type = 'tagid',
                   outputFormat = 'csv',
                   year = lubridate::year(startDate),
                   site = damPIT,
                   species = spp_code,
                   span = 'no',
                   startdate = start_day,
                   enddate = end_day)

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
    janitor::clean_names("upper_camel") |>
    dplyr::filter(!is.na(Species)) |>
    suppressWarnings()

  if("ClockDate" %in% names(parsed)) {
    parsed <-
      parsed |>
      dplyr::rename(Date = ClockDate)
  }
  if("ReleaseRkm" %in% names(parsed)) {
    parsed <-
      parsed %>%
      dplyr::rename(ReleaseRKM = ReleaseRkm)
  }

  pit_df <-
    parsed |>
    # dplyr::rename(TagID = TagId) |>
    dplyr::mutate(
      dplyr::across(Date,
                    lubridate::ymd),
      dplyr::across(`DetectionDateTime`,
                    lubridate::ymd_hms)) %>%
    dplyr::rename(SpCode = Species) %>%
    dplyr::mutate(Species = spp,
                  Year = lubridate::year(endDate)) %>%
    dplyr::arrange(Date,
                   TagId,
                   DetectionDateTime) %>%
    dplyr::select(dplyr::matches("Ladder"),
                  Year, Species, SpCode,
                  TagId,
                  everything()) %>%
    dplyr::filter(Date >= startDate,
                  Date <= endDate)



  # names(pit_df) = gsub(' ', '', names(pit_df))

  if(damPIT  %in% c('PRA', "RIA", "RRF")) {
    pit_df <-
      pit_df %>%
      dplyr::rename(PreviousDate = PreviousDetectDate,
             PreviousHours = DiffPreviousHours,
             PreviousDays = DiffPreviousDays,
             Ladder = Ladder2,
             LadderSide = Ladder9)

  }


  return(pit_df)
}

