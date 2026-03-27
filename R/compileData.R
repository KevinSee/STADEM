#' @title STADEM Data Summary
#'
#' @description Query and summarise data for STADEM
#'
#' @author Kevin See
#'
#' @inheritParams getWindowCounts
#' @param spawn_yr spawn year.
#' @param strata_beg 3 letter code for the day of the week each weekly strata
#'   should begin on. Default value is \code{'Mon'}.
#' @param last_strata_min minimum length (in days) for the final strata. Default
#'   value is 3.
#' @param sthd_B_run should numbers of B run steelhead be reported? These are
#'   defined as wild steelhead greater than 780mm in length. Default is
#'   \code{FALSE}.
#' @param trap_dbase data frame object containing the LWG trapping data with
#'   identical data types as in tblLGDMasterCombineExport; required fields
#'   include MasterID, LGDNumPIT, CollectionDate, SRR, LGDSpecies, LGDRear,
#'   LGDLifeStage, LGDMarkAD, LGDValid, LGDFLmm, PTAgisSxCGRAObs.
#' @param useDARTrate should the DART query for the trap rate be used? Default
#'   is \code{FALSE}, which implies the trap rate is estimated by PIT tags.
#' @param trap_rate_cv constant coefficient of variation (CV) that should be
#'   applied to estimates of trap rate queried by DART. Default value is
#'   \code{0}.
#' @param trap_rate_dist distributional form for trap rate prior. \code{beta}
#'   returns alpha and beta parameters for beta distribution. \code{logit}
#'   returns mean and standard deviation in logit space.
#'
#' @import lubridate dplyr boot janitor
#' @export
#' @return NULL
#' @examples compileData(2012)

compileData = function(spp = c('Chinook', 'Steelhead', 'Coho'),
                       spawn_yr = NULL,
                       dam = c('LWG',
                               'WFF', 'BON', 'TDA', 'JDA', 'MCN',
                               'IHR', 'LMN', 'LGS',
                               'PRO', 'ROZ',
                               'PRD', 'WAN', 'RIS', 'TUM', 'RRH', 'WEL', 'ZOS'),
                       start_date = NULL,
                       end_date = NULL,
                       incl_jacks = NULL,
                       sthd_type = c('all', 'unclipped'),
                       strata_beg = 'Mon',
                       last_strata_min = 3,
                       sthd_B_run = FALSE,
                       trap_dbase = NULL,
                       incl_trapRate = T,
                       useDARTrate = F,
                       trap_rate_cv = 0,
                       trap_rate_dist = c('beta', 'logit')) {

  # need a start date
  stopifnot(!is.null(start_date))

  # set default end date (1 year after start date)
  if(is.null(end_date)) end_date = format(lubridate::ymd(start_date) + years(1) - days(1), '%Y%m%d')

  if(is.null(spawn_yr)) {
    spawn_yr <-
      lubridate::year(lubridate::ymd(end_date))
  }

  # if not provided, set a few defaults
  spp = match.arg(spp)
  dam = match.arg(dam)
  sthd_type = match.arg(sthd_type)
  trap_rate_dist = match.arg(trap_rate_dist)

  # set the PTAGIS code based on the dam
  dam_pit_code = case_when(dam == "PRD" ~ "PRA",
                           dam == "LWG" ~ "GRA",
                           dam == "RIS" ~ "RIA",
                           dam == "RRH" ~ "RRF",
                           .default = NA_character_)


  if(is.null(trap_dbase)) {
    warning('Trapping data is not supplied.')
  }

  if(is.null(incl_jacks)) {
    incl_jacks = dplyr::if_else(spp %in% c('Chinook', 'Coho'), T, F)
  }

  # currently pit tag query only works for Lower Granite and Priest Rapids
  if(is.na(dam_pit_code)) {
    warning('PIT tag queries currently only work for Lower Granite (code LWG), Priest Rapids (PRD), Rock Island (RIS) and Rocky Reach (RRH)')
  }


  message(paste0('Compiling data for ', spp,
             ' at ', dam,
             ' in spawn year ', spawn_yr,'\n'))

  #---------------------------------------------
  # query window counts
  message('Querying window counts\n')
  win_cnts = getWindowCounts(dam = dam,
                             spp = spp,
                             start_date = start_date,
                             end_date = end_date,
                             incl_jacks = incl_jacks,
                             sthd_type = sthd_type) |>
    janitor::clean_names() |>
    dplyr::mutate(window_open = dplyr::if_else(!is.na(win_cnt), T, F))

  #--------------------------------------------------------
  # query PIT tag data from previously tagged fish
  if(!is.na(dam_pit_code)) {
    message('Querying night passage & reascension PIT tags\n')
    pit_df = queryPITtagData(damPIT = dam_pit_code,
                             spp = spp,
                             start_date = start_date,
                             end_date = end_date)

    # for Upper Columbia dams, no day/night counts, so consider all periods day
    if(dam %in% c("PRD", "RIS", "RRH")) {
      pit_df <-
        pit_df |>
        mutate(Period = "D")
    }

    # summarize on daily scale
    pit_daily <-
      summarisePITdataDaily(pit_df) |>
      select(-SpawnYear) |>
      janitor::clean_names()

  } else {
    pit_daily <- NULL
  }

  #--------------------------------------------------------
  # determine weekly strata
  message('Dividing into strata\n')
  week_strata = weeklyStrata(start_date = start_date,
                             end_date = end_date,
                             strata_beg = strata_beg,
                             last_strata_min = last_strata_min)


  # compile data from fish trap, if provided
  if(!is.null(trap_dbase)) {

    if(dam == "LWG") {
      # read in data for Chinook, steelhead, and coho
      message('Getting LGR trap data\n')
      trap_yr = trap_dbase |>
        rename(tag_code = LGDNumPIT) |>
        mutate(Date = lubridate::floor_date(CollectionDate, unit = 'day'),
               SppCode = as.numeric(substr(SRR, 1, 1)),
               across(tag_code,
                      as.character),
               across(tag_code,
                      ~ dplyr::if_else(nchar(.) < 3,
                                       NA_character_, .)),
               Species = ifelse(SppCode == 1, 'Chinook', ifelse(SppCode == 3, 'Steelhead', ifelse(SppCode == 2, 'Coho', NA)))) |>
        # drop data from other species, and other runs of Chinook
        filter(Species %in% c('Chinook', 'Steelhead', 'Coho'),
               # filter out records outside dates of interest
               Date >= lubridate::ymd(lubridate::int_start(week_strata[1])),
               Date < lubridate::ymd(lubridate::int_end(week_strata[length(week_strata)]) + lubridate::dseconds(1)),
               # filter out juveniles, keep only adults
               LGDLifeStage == 'RF',
               # drop sort-by-code fish
               (PTAgisSxCGRAObs != 'Yes' | is.na(PTAgisSxCGRAObs) ) ) |>
        janitor::clean_names() |>
        dplyr::rename(species_run_rear_type = srr) |>
        # assign week numbers to each day
        rowwise() |>
        mutate(week_num = which(date %within% week_strata)) |>
        ungroup() |>
        relocate(week_num,
                 .after = "date")

      if(sum(c("collection_date", "date") %in% names(trap_yr)) == 2) {
        if(identical(trap_yr$collection_date,
                     trap_yr$date)) {
          trap_yr <-
            trap_yr |>
            select(-collection_date) |>
            rename(event_date = date)
        }
      }


    } else if(dam == "PRD" & spp == "Steelhead") {

      # trap_yr <-
      #   read_rds("O:Documents/Git/MyProjects/DabomPriestRapidsSthd/analysis/data/derived_data/Bio_Data_2011_2024.rds") |>
      #   filter(spawn_year == spawn_yr)

      trap_yr <- trap_dbase |>
        rename(tag_code = pit_tag) |>
        # assign week numbers to each day
        rowwise() |>
        mutate(week_num = which(event_date %within% week_strata)) |>
        ungroup() |>
        relocate(week_num,
                 .after = "event_date")
    }

    # summarise by date for particular species
    # trap_daily = summariseLGRtrapDaily(trap_df = trap_yr,
    #                                    spp = spp,
    #                                    sthd_B_run = sthd_B_run)

    trap_daily <-
      summarizeTrapDaily(trap_yr,
                         spp = spp,
                         incl_clip_sthd = if_else(sthd_type == "all",
                                                  TRUE, FALSE),
                         sthd_B_run = sthd_B_run) |>
      arrange(date) |>
      filter(date >= int_start(week_strata[1]),
             date <= int_end(week_strata[length(week_strata)])) |>
      # assign week numbers to each day
      rowwise() |>
      mutate(week_num = which(date %within% week_strata)) |>
      ungroup() |>
      relocate(week_num,
               .after = "date")

  } else {
    trap_yr = NULL
    trap_daily = NULL
  }

  #--------------------------------------------------------
  if(incl_trapRate) {
    # Query trap rate from DART
    message('Estimating trap rate\n')

    # estimate trap rate from PIT tags
    if(!useDARTrate) {
      if(dam == "PRD") {
        pit_obs <-
          queryPITtagObs(site = dam_pit_code,
                         spp = species,
                         start_date = start_date,
                         end_date = end_date) |>
          janitor::clean_names()

      } else if(dam == "LWG") {
        trap_species <-
          trap_yr |>
          mutate(species_code = stringr::str_extract(species_run_rear_type, "[:digit:]+")) |>
          # mutate(species_code = stringr::str_extract(srr, "[:digit:]+")) |>
          pull(species_code) |>
          unique()

        pit_obs <-
          tibble(spp_run = trap_species) |>
          mutate(spp_code = stringr::str_sub(spp_run, 1, 1),
                 spp_name = case_when(spp_code == 1 ~ "Chinook",
                                      spp_code == 2 ~ "Coho",
                                      spp_code == 3 ~ "Steelhead",
                                      .default = NA_character_)) |>
          select(spp_name) |>
          distinct() |>
          dplyr::mutate(pit_dam = purrr::map(spp_name,
                                             .f = function(spp) {
                                               queryPITtagObs(site = dam_pit_code,
                                                              spp = spp,
                                                              start_date = start_date,
                                                              end_date = end_date) |>
                                                 janitor::clean_names()
                                             },
                                             .progress = F)) |>
          tidyr::unnest(pit_dam) |>
          dplyr::mutate(species_code = stringr::str_extract(sp_rrt, "[:digit:]+")) |>
          dplyr::filter(species_code %in% trap_species)

        # filter out Fall Chinook detections, since they're weird (full of sort by code fish)
        pit_obs <-
          pit_obs |>
          dplyr::mutate(keep_record = dplyr::case_when(spp_name == "Chinook" &
                                                         (lubridate::month(obs_time) <= 8 |
                                                            (lubridate::month(obs_time) == 8 &
                                                               lubridate::day(obs_time) <= 17)) ~ T,
                                                       spp_name %in% c("Steelhead", "Coho") ~ T,
                                                       .default = F)) |>
          dplyr::filter(keep_record) |>
          dplyr::select(-keep_record)
      }

      dam_mark_site <- case_when(dam == "PRD" ~ "PRDLD1",
                                 dam == "LWG" ~ c('LGRLDR', 'LGR'),
                                 .default = NA_character_)

      if(sum(is.na(dam_mark_site)) > 0) {
        stop("No trap data exists.")
      }


      trap_rate <-
        pit_obs |>
        filter(!mark_site %in% dam_mark_site) |>
        mutate(obs_date = floor_date(obs_time, unit = "days")) |>
        group_by(tag_code,
                 sp_rrt) |>
        summarize(across(obs_date,
                         min),
                  .groups = "drop") |>
        left_join(trap_yr |>
                    select(tag_code,
                           srr = species_run_rear_type,
                           trap_date = event_date) |>
                    mutate(in_trap = T),
                  by = join_by(tag_code)) |>
        mutate(mod_date = case_when(!is.na(trap_date) ~ trap_date,
                                    is.na(trap_date) ~ obs_date,
                                    .default = NA_Date_),
               across(in_trap,
                      ~ replace_na(., F))) |>
        rowwise() |>
        mutate(week_num = which(mod_date %within% week_strata)) |>
        ungroup() |>
        arrange(mod_date,
                tag_code) |>
        group_by(week_num) |>
        summarise(n_trap_tags = sum(in_trap),
                  n_poss_tags = n_distinct(tag_code),
                  trap_rate = n_trap_tags / n_poss_tags,
                  trap_rate_se = sqrt((trap_rate * (1 - trap_rate)) / n_poss_tags),
                  trap_rate_cv = trap_rate_se / trap_rate,
                  .groups = "drop") |>
        left_join(tibble(week_num = 1:length(week_strata),
                         start_date = int_start(week_strata)),
                  by = join_by(week_num)) |>
        relocate(start_date,
                 .after = "week_num") |>
        mutate(trap_open = if_else(n_trap_tags > 0, T, F)) %>%
        mutate(trap_valid = case_when(n_poss_tags < 20 ~ F,
                                      # n_trap_tags < 7 ~ F,
                                      ! trap_open ~ F,
                                      .default = T)) |>
        select(start_date,
               week_num,
               trap_open,
               trap_valid,
               everything())

    }

    # to use DART trap rate instead (only works for Lower Granite)
    # impose constant CV on trap rate estimates
    if(useDARTrate & dam == "LWG") {
      trap_rate = queryTrapRate(week_strata,
                                spp = spp,
                                return_weekly = T) |>
        janitor::clean_names() |>
        arrange(start_date) |>
        mutate(trap_rate = actual_rate_inclusive_time,
               # add some error
               trap_rate_se = trap_rate * trap_rate_cv) |>
        mutate(trap_open = if_else(trap_rate > 0, T, F)) |>
        select(start_date,
               week_num,
               trap_open,
               trap_rate,
               trap_rate_se,
               everything())
    }

    # mark trap rat valid if there were fish in the trap
    trap_rate <-
      trap_rate |>
      dplyr::full_join(trap_yr |>
                         dplyr::group_by(week_num) |>
                         dplyr::summarise(trap_fish = n()),
                       by = dplyr::join_by(week_num)) |>
      arrange(week_num) |>
      dplyr::mutate(
        dplyr::across(trap_open,
                      ~ dplyr::case_when(trap_fish > 0 ~ TRUE,
                                         is.na(.) ~ FALSE,
                                         .default = .))) |>
      dplyr::select(-trap_fish)

  }

  #--------------------------------------------------------
  # comnbine window counts, PIT tag data and trap summary on daily time-step
  message('Combining daily data\n')
  dam_daily <-
    win_cnts |>
    select(-year) |>
    left_join(pit_daily,
              by = join_by(species, date)) |>
    # assign week numbers to each day
    filter(date >= int_start(week_strata[1]),
           date <= int_end(week_strata[length(week_strata)])) |>
    rowwise() |>
    mutate(week_num_org = which(date %within% week_strata)) |>
    ungroup() |>
    relocate(week_num_org,
             .after = "date")

  if(!is.null(trap_daily)) {
    dam_daily <-
      dam_daily |>
      left_join(trap_daily,
                by = join_by(date))

  }


  #------------------------------------------
  # summarize by week and add trap rate
  dam_weekly <-
    dam_daily |>
    filter(!is.na(week_num_org)) |>
    group_by(species,
             week_num_org) |>
    summarise(
      across(date,
             min),
      across(c(win_cnt,
               where(is.numeric)),
             ~ sum(., na.rm = T)),
      across(where(is.logical),
             ~ if_else(sum(.) > 0, T, F)),
      .groups = "drop") |>
    rename(start_date = date) |>
    group_by(species) |>
    mutate(week_num = week_num_org - min(week_num_org) + 1) |>
    ungroup() |>
    select(species,
           starts_with("week_num"),
           start_date,
           window_open,
           everything())


  if(incl_trapRate) {
    dam_weekly <-
      addTrapRate(dam_weekly,
                  trap_rate,
                  trap_rate_dist)
  }

  return_list <-
    list('weekStrata' = week_strata,
         'dailyData' = dam_daily,
         'weeklyData' = dam_weekly)

  if(!is.null(trap_yr)) {
    return_list <-
      c(return_list,
        list(trapData = trap_yr))
  }

  return(return_list)

}

