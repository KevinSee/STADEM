#' @title PIT tag data
#'
#' @description Query and download PIT tag data about night passage and re-ascension at particular dams for specific species and year, using DART.
#'
#' @author Kevin See
#'
#' @param dam the dam code for the dam you wish to query for PIT tag data. Currently only available for Lower Granite Dam (GRA).
#' @param spp_code species code to query window counts for. Possible codes are: 1 (Chinook), 2 (Coho), 3 (Steelhead), 4 (Sockeye)
#' @param yr calendar year to query for PIT tag data.
#'
#' @source \url{http://www.cbr.washington.edu/dart}
#'
#' @import lubridate readr httr dplyr
#' @export
#' @return NULL
#' @examples queryPITtagData(yr = 2015)

queryPITtagData = function(dam = 'GRA',
                           spp_code = c('1', '2', '3', '4'),
                           yr = NULL) {

  # need a year, and only dam allowed in Lower Granite (GRA)
  stopifnot(dam == 'GRA', !is.null(yr))

  # pull out default dam and species code
  dam = match.arg(dam)
  spp_code = match.arg(spp_code, several.ok = T)
  spp_code = as.integer(spp_code)

  # match up species code with species name
  spp_code_df = data.frame(Species = c('Chinook', 'Coho', 'Steelhead', 'Sockeye'),
                           code = 1:4)

  spp_name = spp_code_df %>%
    filter(code %in% spp_code) %>%
    select(Species) %>%
    as.matrix() %>%
    as.character()

  # assign user agent to the GitHub repo for this package
  ua = user_agent('https://github.com/KevinSee/damEscapement')

  # compose url with query
  url_req = 'http://www.cbr.washington.edu/dart/cs/php/rpt/pit_adult_window_new.php'

  # send query to DART
  pit_df = NULL
  for(i in 1:length(yr)) {
    for(j in 1:length(spp_code)) {
      web_req = GET(url_req, ua,
                    query = list(type = 'tagid',
                                 outputFormat = 'csv',
                                 year = yr[i],
                                 site = 'GRA',
                                 species = spp_code[j],
                                 span = 'no',
                                 startdate = '1/1',
                                 enddate = '12/31',
                                 syear = yr[i],
                                 eyear = yr[i]))
      # if any problems
      httr::stop_for_status(web_req,
                            task = 'query data from DART')

      # what encoding to use?
      # stringi::stri_enc_detect(content(web_req, "raw"))

      # parse the response
      parsed = httr::content(web_req,
                             'parsed',
                             encoding = 'ISO-8859-1')

      if(is.null(parsed)) {
        # return(NULL)
        message(paste('DART returned no data for', spp_name[j], 'in', yr[i], '\n'))
        next
      }

      if(class(parsed)[1] == 'xml_document') {
        # return(NULL)
        message(paste('For', spp_name[j], 'in', yr[i], 'XML document returned by DART instead of data\n'))
        next
      }

      if (status_code(web_req) != 200) {
        message(
          sprintf(
            "GitHub API request failed [%s]\n%s\n<%s>",
            status_code(web_req),
            parsed$message,
            parsed$documentation_url
          ),
          call. = FALSE
        )
        next
      }

      # re-format slightly
      if(is.null(pit_df)) {
        pit_df = parsed %>%
          mutate(Date = lubridate::ymd(Date)) %>%
          filter(!is.na(Date)) %>%
          dplyr::rename(SpCode = Species) %>%
          mutate(Species = spp_name[j],
                 Year = yr[i]) %>%
          select(Ladder, Year, Species, SpCode, TagID, everything())
      }
      else {
        pit_df = pit_df %>%
          bind_rows(parsed %>%
                      mutate(Date = lubridate::ymd(Date)) %>%
                      filter(!is.na(Date)) %>%
                      dplyr::rename(SpCode = Species) %>%
                      mutate(Species = spp_name[j],
                             Year = yr[i]) %>%
                      select(Ladder, Year, Species, SpCode, TagID, everything()))
      }

    }

  }

  pit_df = pit_df %>%
    tbl_df()
  names(pit_df) = gsub(' ', '', names(pit_df))

  return(pit_df)
}
