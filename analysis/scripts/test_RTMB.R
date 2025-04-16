# Author: Kevin See
# Purpose: test STADEM using NIMBLE
# Created: 10/30/24
# Last Modified: 11/1/24
# Notes: NIMBLE help pages: https://r-nimble.org/html_manual/cha-welcome-nimble.html

#-----------------------------------------------------------------
# load needed libraries
library(tidyverse)
library(STADEM)
library(RTMB)
library(janitor)

#-----------------------------------------------------------------
# set species and spawn year
species = c("Steelhead",
            "Chinook")[2]
yr = 2019
dam_code = c("PRD",
             "LWG")[2]

# what dates should window counts cover?
window_dates <-
  case_when(species == "Steelhead" &
              dam_code == "LWG" ~ c(paste0(yr-1, "0701"),
                                    paste0(yr, "0630")),
            species == "Steelhead" &
              dam_code == "PRD" ~ c(paste0(yr-1, "0601"),
                                    paste0(yr, "0531")),
            species == "Chinook" &
              dam_code == "LWG" ~ c(paste0(yr, "0301"),
                                    paste0(yr, "0817")),
            species == "Chinook" &
              dam_code == "PRD" ~ c(paste0(yr, "0301"),
                                    paste0(yr, "0615")))
# trap data
if(dam_code == "PRD") {
  trap_dbase <- read_rds("O:Documents/Git/MyProjects/DabomPriestRapidsSthd/analysis/data/derived_data/Bio_Data_2011_2024.rds") |>
    filter(spawn_year == yr)

  if(n_distinct(trap_dbase$event_date) == 1) {
    trap_dbase <-
      trap_dbase |>
      rename(event_date_org = event_date,
             event_date = release_date)
  }

} else if(dam_code == "LWG" & species == "Chinook" & yr == 2019) {
  trap_dbase = readLGRtrapDB(system.file("extdata",
                                         "Chnk2019_TrapDatabase.csv",
                                         package = "STADEM",
                                         mustWork = TRUE))
} else {
  message("Trap data not available")
}

data_list <-
  compileData(spp = species,
              spawn_yr = yr,
              start_date = window_dates[1],
              end_date = window_dates[2],
              dam = dam_code,
              trap_dbase = trap_dbase,
              incl_trapRate = T)

#-------------------------------------------------------
# what kind of hatchery / wild calls should be used?
hw_type = c("PBT", "Morph")[1]

# pull out certain pieces of data to feed into model
jags_data_list <- prepJAGS(data_list$weeklyData,
                           hw_type = hw_type)

