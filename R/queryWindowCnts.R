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
#' @import lubridate vroom httr2 dplyr tidyr purrr
#' @export
#' @return NULL
#' @examples queryWindowCnts(dam = "LWG", spp_code = "fc", start_date = '20150301', end_date = '20150817')

queryWindowCnts = function(dam = c('LWG', 'WFF', 'BON', 'TDA', 'JDA', 'MCN', 'IHR', 'LMN', 'LGS', 'PRO', 'ROZ', 'PRD', 'WAN', 'RIS', 'TUM', 'RRH', 'WEL', 'ZOS'),
                           spp_code = c('fc', 'fk', 'fb',
                                        'fs', 'fsw', 'fa',
                                        'fcj', 'fkj',
                                        # 'fbj',
                                        # 'fsj',
                                        'fl', 'ft'),
                           start_date = NULL,
                           end_date = NULL) {

  # need a start date
  stopifnot(!is.null(start_date))

  # set default end date (1 year after start date)
  if(is.null(end_date)) end_date = format(lubridate::ymd(start_date) + years(1) - days(1), '%Y%m%d')

  # turn start / end date character vectors into actual date objects
  if(lubridate::ymd(end_date) < lubridate::ymd(start_date)) stop('start_date comes after end_date')

  # pull out default dam
  dam = match.arg(dam, several.ok = T)
  # pull out default spp_code
  spp_code = match.arg(spp_code, several.ok = T)

  # match up species code with species name
  spp_name <- dplyr::case_when(spp_code == "fc" ~ "Chinook",
                               spp_code == "fk" ~ "Coho",
                               spp_code == "fb" ~ "Sockeye",
                               spp_code == "fs" ~ "Steelhead",
                               spp_code == "fsw" ~ "Wild_Steelhead",
                               spp_code == "fa" ~ "Shad",
                               spp_code == "fcj" ~ "Jack_Chinook",
                               spp_code == "fkj" ~ "Jack_Coho",
                               spp_code == "fbj" ~ "Jack_Sockeye",
                               spp_code == "fsj" ~ "Jack_Steelhead",
                               spp_code == "fl" ~ "Lamprey",
                               spp_code == "ft" ~ "Bull_Trout",
                               .default = NA_character_)

  # assign user agent to the GitHub repo for this package
  ua <- "https://github.com/KevinSee/STADEM"

  # compose url with query
  url_req = 'https://www.cbr.washington.edu/dart/cs/php/rpt/mg.php'

  # build query for DART
  queryList <- list(sc = 1,
                    mgconfig = 'adult',
                    outputFormat = 'csvSingle',
                    startdate = '01/01',
                    enddate = '12/31')

  # construct a request
  req <-
    httr2::request(url_req) |>
    httr2::req_user_agent(ua) |>
    httr2::req_url_query(!!!queryList) |>
    httr2::req_url_query("loc[]" = dam,
                         .multi = "explode") |>
    httr2::req_url_query("ftype[]" = c(spp_code),
                         .multi = "explode") |>
    httr2::req_url_query("year[]" = lubridate::year(lubridate::ymd(start_date)):lubridate::year(lubridate::ymd(end_date)),
                         .multi = "explode")

  # req |>
  #   req_dry_run()

  # send query to DART
  resp <-
    req |>
    httr2::req_perform()

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

  # parse the response
  if(httr2::resp_content_type(resp) == "text/csv") {
    parsed <-
      resp |>
      httr2::resp_body_string() |>
      vroom::vroom(delim = ',',
                   col_names = T,
                   show_col_types = FALSE) |>
      janitor::clean_names("upper_camel") |>
      dplyr::filter(!is.na(Location)) |>
      suppressWarnings() |>
      dplyr::mutate(
        dplyr::across(Year,
                      as.integer)) |>
      dplyr::mutate(Date = lubridate::ymd(paste(Year, MmDd, sep = "-"))) |>
      dplyr::mutate(Species = dplyr::case_match(Parameter,
                                                'Chin' ~ 'Chinook',
                                                'JChin' ~ 'Jack_Chinook',
                                                'Coho' ~ 'Coho',
                                                'JCoho' ~ 'Jack_Coho',
                                                'JStlhd' ~ 'Jack_Steelhead',
                                                'Lmpry' ~ 'Lamprey',
                                                'Shad' ~ "Shad",
                                                'Sock' ~ 'Sockeye',
                                                'Stlhd' ~ 'Steelhead',
                                                'WStlhd' ~ 'Wild_Steelhead',
                                                "BTrout" ~ "Bull_Trout",
                                                .default = Parameter)) |>
      dplyr::select(Location,
                    Year,
                    Date,
                    Species,
                    win_cnt = Value) |>
      tidyr::pivot_wider(names_from = "Species",
                         values_from = "win_cnt",
                         values_fill = 0) |>
      dplyr::filter(Date >= lubridate::ymd(start_date),
                    Date <= lubridate::ymd(end_date))
  } else if(httr2::resp_content_type(resp) != "text/csv") {
    if(length(dam) == 1) {
      stop(paste('DART returned no window count data for', paste(spp_name, collapse = ", "), 'in', lubridate::year(lubridate::ymd(start_date)), "at", dam, '\n'))
    }
      #
      # resp_n |>
      # httr2::resp_body_string() |>
      # vroom::vroom(delim = ',',
      #              col_names = T,
      #              show_col_types = FALSE) |>
      # names()
      #


    # try each dam combination separately
    dam_spp_df <-
      dplyr::tibble(dam = dam) |>
      dplyr::mutate(win_cnt_query = purrr::map(dam,
                                               .f = function(x) {
                                                 # cat(paste("Trying", y, "at", x, "\n"))
                                                 try(queryWindowCnts(dam = x,
                                                                     spp_code = spp_code,
                                                                     start_date = start_date,
                                                                     end_date = end_date))
                                               })) |>
      dplyr::mutate(query_fail = purrr::map_lgl(win_cnt_query,
                                                .f = function(x) inherits(x, "try-error")))

    parsed <-
      dam_spp_df |>
      filter(!query_fail) |>
      select(win_cnt_query) |>
      unnest(win_cnt_query) |>
      pivot_longer(-(Location:Date)) |>
      filter(!is.na(value)) |>
      pivot_wider(names_sort = T) |>
      arrange(Location,
              Date)

  }

  return(parsed)
}
