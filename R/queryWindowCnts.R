#' @title Window Counts
#'
#' @description Query and download window counts from a particular by species and year using DART
#'
#' @author Kevin See
#'
#' @param dam the dam code for the dam you wish to query for window counts. Possible codes are: WFF (Willamette Falls), BON (Bonneville), TDA (The Dalles), JDA (John Day), MCN (McNary), IHR (Ice Harbor), LMN (Lower Monumental), LGS (Little Goose), LWG (Lower Granite), PRO (Prosser), ROZ (Roza), PRD (Priest Rapids), WAN (Wanapum), RIS (Rock Island), TUM (Tumwater), RRH (Rocky Reach), WEL (Wells), ZOS (Zosel)
#' @param spp_code species code(s) to query window counts for. Possible codes are: fc (Chinook), fk (Coho), fb (Sockeye), fs (Steelhead), fsw (Wild Steelhead), fa (Shad), fcj (Jack Chinook), fkj (Jack Coho), fbj (Jack Sockeye), fsj (Jack Steelhead), fl (Lamprey), ft (Bull Trout
#' @param yr calendar year(s) to query for window counts.
#'
#' @source \url{http://www.cbr.washington.edu/dart}
#'
#' @import lubridate readr httr dplyr
#' @export
#' @return NULL
#' @examples queryWindowCnts(yr = 2015)

queryWindowCnts = function(dam = c('LWG', 'WFF', 'BON', 'TDA', 'JDA', 'MCN', 'IHR', 'LMN', 'LGS', 'PRO', 'ROZ', 'PRD', 'WAN', 'RIS', 'TUM', 'RRH', 'WEL', 'ZOS'),
                       spp_code = c('fc', 'fk', 'fb', 'fs', 'fsw', 'fa', 'fcj', 'fkj', 'fbj', 'fsj', 'fl', 'ft'),
                       yr = NULL) {
  # need a year and species code
  stopifnot(!is.null(yr))
  stopifnot(!is.null(spp_code))

  # pull out default dam
  dam = match.arg(dam)
  spp_code = match.arg(spp_code, several.ok = T)

  # match up species code with species name
  spp_code_df = data.frame(Species = c('Chinook', 'Coho', 'Sockeye', 'Steelhead', 'Wild_Steelhead', 'Shad', 'Jack_Chinook', 'Jack_Coho', 'Jack_Sockeye', 'Jack_Steelhead', 'Lamprey', 'Bull_Trout'),
                           code = c('fc', 'fk', 'fb', 'fs', 'fsw', 'fa', 'fcj', 'fkj', 'fbj', 'fsj', 'fl', 'ft'))

  spp_name = spp_code_df %>%
    filter(code %in% spp_code) %>%
    select(Species) %>%
    as.matrix() %>%
    as.character()

  # assign user agent to the GitHub repo for this package
  ua = user_agent('https://github.com/KevinSee/damEscapement')

  # compose url with query
  url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/mg.php'

  # send query to DART
  win_cnts = NULL
  for(i in 1:length(yr)) {
    yr_cnts = NULL
    for(j in 1:length(spp_code)) {
      web_req = GET(url_req, ua,
                    query = list(sc = 1,
                                 mgconfig = 'adult',
                                 outputFormat = 'csv',
                                 `year[]` = yr[i],
                                 `loc[]` = dam,
                                 `ftype[]` = spp_code[j],
                                 startdate = '1/1',
                                 enddate = '12/31'))

      # if any problems
      httr::stop_for_status(web_req,
                            task = 'query data from DART')

      # what encoding to use?
      # stringi::stri_enc_detect(content(web_req, "raw"))

      # parse the response
      parsed = httr::content(web_req,
                             'parsed',
                             encoding = 'ISO-8859-2',
                             skip = 2,
                             col_names = c('Day', spp_name[j]))

      if (status_code(web_req) != 200) {
        stop(
          sprintf(
            "GitHub API request failed [%s]\n%s\n<%s>",
            status_code(web_req),
            parsed$message,
            parsed$documentation_url
          ),
          call. = FALSE
        )
      }

      # re-format slightly
      if(is.null(yr_cnts)) {
        yr_cnts = parsed %>%
          dplyr::mutate(Date = lubridate::ymd(paste(yr[i], Day))) %>%
          dplyr::filter(!is.na(Date)) %>%
          dplyr::mutate(Year = yr[i]) %>%
          dplyr::select(Year, Date, matches(spp_name[j]))
      }
      else yr_cnts = yr_cnts %>%
        dplyr::left_join(parsed %>%
                           dplyr::mutate(Date = lubridate::ymd(paste(yr[i], Day))) %>%
                           dplyr::filter(!is.na(Date)) %>%
                           dplyr::mutate(Year = yr[i]) %>%
                           dplyr::select(Year, Date, matches(spp_name[j])))
    }
    if(is.null(win_cnts)) win_cnts = yr_cnts
    else win_cnts = win_cnts %>%
        bind_rows(yr_cnts)
  }

  return(win_cnts)
}

