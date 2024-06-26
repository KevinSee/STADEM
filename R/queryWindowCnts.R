#' @title DART Window Counts
#'
#' @description Query and download window counts at a dam by species and year using DART
#'
#' @author Kevin See
#'
#' @param dam the dam code for the dam you wish to query for window counts. Possible codes are: WFF (Willamette Falls), BON (Bonneville), TDA (The Dalles), JDA (John Day), MCN (McNary), IHR (Ice Harbor), LMN (Lower Monumental), LGS (Little Goose), LWG (Lower Granite), PRO (Prosser), ROZ (Roza), PRD (Priest Rapids), WAN (Wanapum), RIS (Rock Island), TUM (Tumwater), RRH (Rocky Reach), WEL (Wells), ZOS (Zosel)
#' @param spp_code species code(s) to query window counts for. Possible codes are: fc (Chinook), fk (Coho), fb (Sockeye), fs (Steelhead), fsw (Wild Steelhead), fa (Shad), fcj (Jack Chinook), fkj (Jack Coho), fbj (Jack Sockeye), fsj (Jack Steelhead), fl (Lamprey), ft (Bull Trout
#' @param start_date character vector of date (\code{YYYYMMDD}) when query should start
#' @param end_date character vector of date (\code{YYYYMMDD}) when query should end
#'
#' @source \url{https://www.cbr.washington.edu/dart}
#'
#' @import lubridate readr httr dplyr tidyr
#' @export
#' @return NULL
#' @examples queryWindowCnts(spawn_yr = 2015)

queryWindowCnts = function(dam = c('LWG', 'WFF', 'BON', 'TDA', 'JDA', 'MCN', 'IHR', 'LMN', 'LGS', 'PRO', 'ROZ', 'PRD', 'WAN', 'RIS', 'TUM', 'RRH', 'WEL', 'ZOS'),
                           spp_code = c('fc', 'fk', 'fb', 'fs', 'fsw', 'fa', 'fcj', 'fkj', 'fbj', 'fsj', 'fl', 'ft'),
                           start_date = NULL,
                           end_date = NULL) {

  # need a start date
  stopifnot(!is.null(start_date))

  # set default end date (1 year after start date)
  if(is.null(end_date)) end_date = format(lubridate::ymd(start_date) + years(1) - days(1), '%Y%m%d')

  # turn start / end date character vectors into actual date objects
  startDate = lubridate::ymd(start_date)
  endDate = lubridate::ymd(end_date)

  if(endDate < startDate) stop('start_date comes after end_date')

  if(as.integer(year(endDate)) - as.integer(year(startDate)) > 1) stop('Years must be consecutive')

  # get character vectors of start day and end day for query
  start_day = paste(lubridate::month(startDate), lubridate::day(startDate), sep = '/')
  end_day = paste(lubridate::month(endDate), lubridate::day(endDate), sep = '/')

  # pull out default dam
  dam = match.arg(dam)
  # pull out default spp_code
  spp_code = match.arg(spp_code, several.ok = T)

  # match up species code with species name
  spp_name <-
    dplyr::tibble(Species = c('Chinook', 'Coho', 'Sockeye', 'Steelhead', 'Wild_Steelhead', 'Shad', 'Jack_Chinook', 'Jack_Coho', 'Jack_Sockeye', 'Jack_Steelhead', 'Lamprey', 'Bull_Trout'),
                        code = c('fc', 'fk', 'fb', 'fs', 'fsw', 'fa', 'fcj', 'fkj', 'fbj', 'fsj', 'fl', 'ft')) |>
    dplyr::filter(code %in% spp_code) |>
    dplyr::mutate(code = factor(code, levels = spp_code)) |>
    dplyr::arrange(code) |>
    dplyr::pull(Species)

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/KevinSee/STADEM')

  # compose url with query
  url_req = 'https://www.cbr.washington.edu/dart/cs/php/rpt/mg.php'

  # build query list to send to DART
  queryList = list(sc = 1,
                   mgconfig = 'adult',
                   outputFormat = 'csvSingle',
                   `loc[]` = dam,
                   startdate = start_day,
                   enddate = end_day)

  sppList = NULL
  for(i in 1:length(spp_code)) {
    sppList = c(sppList,
                list(`ftype[]` = spp_code[i]))
  }

  yrList = list(`year[]` = lubridate::year(startDate))

  # if(grepl('Steelhead', spp_name)) {
  if(lubridate::year(endDate) > lubridate::year(startDate)) {
    yrList = list(`year[]` = lubridate::year(startDate),
                  `year[]` = lubridate::year(endDate))

    queryList[['startdate']] = '01/01'
    queryList[['enddate']] = '12/31'

  }

  # send query to DART
  web_req = httr::GET(url_req, ua,
                      query = c(queryList, sppList, yrList))

  # if any problems
  httr::stop_for_status(web_req,
                        task = 'query data from DART')

  # what encoding to use?
  # stringi::stri_enc_detect(content(web_req, "raw"))

  # parse the response
  parsed = suppressWarnings(
    httr::content(web_req,
                         'text',
                         encoding = 'UTF-8') |>
    readr::read_delim(delim = ',',
                      col_names = T,
                      skip = 1,
                      show_col_types = FALSE)
  )

  if(is.null(parsed) | ncol(parsed) == 1) {
    stop(paste('DART returned no window count data for', spp_name, 'in', lubridate::year(startDate), '\n'))
  }


  if (httr::status_code(web_req) != 200) {
    stop(
      sprintf(
        "GitHub API request failed [%s]\n%s\n<%s>",
        httr::status_code(web_req),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }

  # re-format
  win_cnts <-
    parsed |>
    dplyr::mutate(year = as.integer(year)) |>
    suppressWarnings() |>
    dplyr::filter(!is.na(year)) |>
    dplyr::mutate(Date = lubridate::ymd(paste(year, `mm-dd`, sep = '-'))) |>
    suppressWarnings() |>
    dplyr::mutate(Species = dplyr::case_match(parameter,
                                              'Chin' ~ 'Chinook',
                                              'JChin' ~ 'Jack_Chinook',
                                              'Coho' ~ 'Coho',
                                              'JCoho' ~ 'Jack_Coho',
                                              'JStlhd' ~ 'Jack_Steelhead',
                                              'Lmpry' ~ 'Lamprey',
                                              'Sock' ~ 'Sockeye',
                                              'Stlhd' ~ 'Steelhead',
                                              'WStlhd' ~ 'Wild_Steelhead')) |>
    dplyr::select(Species,
                  Year = year,
                  Date,
                  win_cnt = value) |>
    tidyr::pivot_wider(names_from = "Species",
                       values_from = "win_cnt",
                       values_fill = 0) |>
    dplyr::filter(Date >= startDate,
                  Date <= endDate)

  return(win_cnts)
}
