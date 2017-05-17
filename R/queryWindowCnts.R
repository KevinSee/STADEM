#' @title DART Window Counts
#'
#' @description Query and download window counts at a dam by species and year using DART
#'
#' @author Kevin See
#'
#' @param dam the dam code for the dam you wish to query for window counts. Possible codes are: WFF (Willamette Falls), BON (Bonneville), TDA (The Dalles), JDA (John Day), MCN (McNary), IHR (Ice Harbor), LMN (Lower Monumental), LGS (Little Goose), LWG (Lower Granite), PRO (Prosser), ROZ (Roza), PRD (Priest Rapids), WAN (Wanapum), RIS (Rock Island), TUM (Tumwater), RRH (Rocky Reach), WEL (Wells), ZOS (Zosel)
#' @param spp_code species code(s) to query window counts for. Possible codes are: fc (Chinook), fk (Coho), fb (Sockeye), fs (Steelhead), fsw (Wild Steelhead), fa (Shad), fcj (Jack Chinook), fkj (Jack Coho), fbj (Jack Sockeye), fsj (Jack Steelhead), fl (Lamprey), ft (Bull Trout
#' @param spawn_yr spawn year to query for window counts.
#' @param start_day date (\code{month / day}) when query should start
#' @param end_day date (\code{month / day}) when query should end
#'
#' @source \url{http://www.cbr.washington.edu/dart}
#'
#' @import lubridate readr httr dplyr
#' @return NULL
#' @examples queryWindowCnts(spawn_yr = 2015)

queryWindowCnts = function(dam = c('LWG', 'WFF', 'BON', 'TDA', 'JDA', 'MCN', 'IHR', 'LMN', 'LGS', 'PRO', 'ROZ', 'PRD', 'WAN', 'RIS', 'TUM', 'RRH', 'WEL', 'ZOS'),
                           spp_code = c('fc', 'fk', 'fb', 'fs', 'fsw', 'fa', 'fcj', 'fkj', 'fbj', 'fsj', 'fl', 'ft'),
                           spawn_yr = NULL,
                           start_day = c('03/01', '07/01'),
                           end_day = c('08/17', '06/30')) {

  # need a year
  stopifnot(!is.null(spawn_yr))

  # pull out default start and end dates
  start_day = match.arg(start_day)
  end_day = match.arg(end_day)

  # pull out default dam
  dam = match.arg(dam)
  # pull out default spp_code
  spp_code = match.arg(spp_code, several.ok = T)

  # match up species code with species name
  spp_name = tibble(Species = c('Chinook', 'Coho', 'Sockeye', 'Steelhead', 'Wild_Steelhead', 'Shad', 'Jack_Chinook', 'Jack_Coho', 'Jack_Sockeye', 'Jack_Steelhead', 'Lamprey', 'Bull_Trout'),
                    code = c('fc', 'fk', 'fb', 'fs', 'fsw', 'fa', 'fcj', 'fkj', 'fbj', 'fsj', 'fl', 'ft')) %>%
    filter(code %in% spp_code) %>%
    mutate(code = factor(code, levels = spp_code)) %>%
    arrange(code) %>%
    select(Species) %>%
    as.matrix() %>%
    as.character()

  # assign user agent to the GitHub repo for this package
  ua = httr::user_agent('https://github.com/KevinSee/damEscapement')

  # compose url with query
  url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/mg.php'

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

  yrList = list(`year[]` = spawn_yr)

  if(grepl('Steelhead', spp_name)) {
    yrList = c(list(`year[]` = spawn_yr - 1),
               yrList)

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
  parsed = httr::content(web_req,
                         'text') %>%
    read_delim(delim = ',',
               col_names = T,
               skip = 1)

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
  win_cnts = parsed %>%
    mutate(year = as.integer(year)) %>%
    filter(!is.na(year)) %>%
    mutate(Date = ymd(paste(year, `mm-dd`, sep = '-'))) %>%
    mutate(Species = recode(parameter,
                            'Chin' = 'Chinook',
                            'JChin' = 'Jack_Chinook',
                            'JCoho' = 'Jack_Coho',
                            'JStlhd' = 'Jack_Steelhead',
                            'Lmpry' = 'Lamprey',
                            'Sock' = 'Sockeye',
                            'Stlhd' = 'Steelhead',
                            'WStlhd' = 'Wild_Steelhead')) %>%
    select(Species,
           Year = year,
           Date,
           win_cnt = value) %>%
    spread(Species, win_cnt, fill = 0)


  if(grepl('Steelhead', spp_name)) {
    win_cnts = win_cnts %>%
      filter(Date >= ymd(paste(spawn_yr - 1, start_day)),
             Date <= ymd(paste(spawn_yr, end_day)))
  }

  return(win_cnts)
}
