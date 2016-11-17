#' @title Mark-recapture trap rate
#'
#' @description Estimate the trap rate at Lower Granite by using a mark-recapture model. The trap is the marking event, and detections at the weir at the top of the fish ladder is the 2nd capture event.
#'
#' @author Kevin See
#'
#' @param filepath file path to the excel file containing data
#' @param week_strata vector of intervals delimiting the weekly strata to summarize mark-recapture data over. Returned as one part of the \code{summariseLGRweekly} function.
#'
#' @import lubridate readxl dplyr Rcapture boot msm
#' @importFrom plyr ddply
#' @export
#' @return NULL

mrTrapRate = function(filepath = NULL,
                      week_strata = NULL) {

  trap_df = read_excel(filepath,
                       1,
                       skip = 5) %>%
    mutate(Species = revalue(Species, c('steelhead' = 'Steelhead'))) %>%
    dplyr::rename(Date = `Trap date`,
           M = `TRAP adj dn time`,
           C = `LAD adj dn time`,
           SpawnYear = `Spawn yr`,
           Year = `Obs Year YYYY`,
           Tag_Code = `Tag Code`,
           tag_yr = `unique tag-year`,
           rear_type = `rear type`,
           SRR = `SRR Code`,
           StartDate = `sample week min date`,
           week_num_org2 = `both spec. samp Week`) %>%
    dplyr::mutate(R = ifelse(M == 1 & C == 1, 1, 0)) %>%
    dplyr::select(SpawnYear, Year, Tag_Code, rear_type, SRR, Date, M, C, R) %>%
    dplyr::filter(!is.na(Date),
           !is.na(M),
           !(M == 0 & C == 0)) %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(week_num_org = NA)

  # assign week number
  for(i in 1:length(week_strata)) {
    trap_df$week_num_org[with(trap_df, which(Date %within% week_strata[i]))] = i
  }

  # summarise by week
  ch_freq = trap_df %>%
    group_by(Year, week_num_org, M, C, R) %>%
    dplyr::summarise(freq = n()) %>%
    ungroup()

  trap_rate_mr = ddply(ch_freq,
        .(Year, week_num_org),
        function(x) {
          mod = try(closedp(x[,c('M', 'C', 'R', 'freq')], dfreq=T), silent = T)
          if(class(mod)[1] == 'try-error') return(data.frame(N = NA, N_se = NA, p = NA, p_se = NA))
          mod_N = mod$results['Mt', c('abundance', 'stderr')]
          p_mean = inv.logit(coef(mod$glm[['Mt']])[2:3])
          p_se = deltamethod(list(~ 1 / (1 + exp(-x1)), ~ 1 / (1 + exp(-x2))), mean = coef(mod$glm[['Mt']])[2:3], cov = vcov(mod$glm[['Mt']])[2:3,2:3])
          return(data.frame(trap_fish = sum(x[x$M==1,'freq']), N = mod_N[1], N_se = mod_N[2], p = p_mean[1], p_se = p_se[1]))
        },
        .id = 'week_num_org',
        .progress = 'text') %>%
    tbl_df()

  return(trap_rate_mr)

}
