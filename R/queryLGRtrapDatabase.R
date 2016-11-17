#' @title Access Lower Granite Dam trap database
#'
#' @description Download copy of the Lower Granite Dam trap database. ** This currently doesn't work **
#'
#' @author Kevin See
#'
#' @param username A username to access the SharePoint site where the trap database resides.
#' @param password A password to access the SharePoint site where the trap database resides.
#'
#' @import lubridate readr httr dplyr
#' @export
#' @return NULL
#' @examples # Don't run; # queryLGRtrapDatabase(username = 'ksee', password = 'IDFGuser')

queryLGRtrapDatabase = function(username = NULL,
                                password = NULL) {

  url_req = 'https://collaboration.idfg.idaho.gov/qci/default.aspx'

  # assign user agent to the GitHub repo for this package
  ua = user_agent('https://github.com/KevinSee/damEscapement')

  # where zip file of trap database lives
  url_req = 'https://collaboration.idfg.idaho.gov/qci/Shared%20Documents/LGTrappingExportJodyW.zip'

  url_req = 'https://collaboration.idfg.idaho.gov/qci/Shared%20Documents'

  web_req = GET(url_req, ua,
                authenticate(username, password),
                progress(),
                write_disk('data/LGTrappingExportJodyW.zip',
                           overwrite = T))

  unzip('data/LGTrappingExportJodyW.zip')

  lgr_trap_df = unz('data/LGTrappingExportJodyW.zip',
                    'data/LGTrappingExportJodyW.csv') %>%
    read.csv() %>%
    tbl_df()

  # what encoding to use?
  # stringi::stri_enc_detect(content(web_req, "raw"))

  url_req = paste0('https://', username, ':', password, 'collaboration.idfg.idaho.gov/qci/Shared%20Documents/LGTrappingExportJodyW.zip')

  temp = tempfile()
  download.file(url_req,
                temp)
  lgr_trap_df = unz(temp,
                    'data/LGTrappingExportJodyW.csv') %>%
    read.csv() %>%
    tbl_df()
  unlink(temp)

  lgr_trap_zip = content(web_req,
                         'parsed',
                         encoding = 'ISO-8859-1')

}
