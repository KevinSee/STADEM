#' @title Mark-recapture trap rate
#'
#' @description Estimate the trap rate at Lower Granite by using a mark-recapture model. The trap is the marking event, and detections at the weir at the top of the fish ladder is the 2nd capture event.
#'
#' @author Kevin See
#'
#' @param filepath file path to the excel file containing data
#' @param week_strata vector of intervals delimiting the weekly strata to summarize mark-recapture data over. Returned as one part of the \code{summariseLGRweekly} function.
#' @param m model type to use to estimate parameters. Standard \code{Mt} and \code{Mt.bc} use \code{closedp} and \code{closedp.bc} functions from \code{Rcapture} package. \code{Chap} used the Chapman, or modified Lincoln-Peterson estimator.
#'
#' @import lubridate readxl dplyr Rcapture FSA boot msm
#' @importFrom plyr ddply
#' @export
#' @return NULL

mrTrapRate = function(filepath = NULL,
                      week_strata = NULL,
                      m = c('Mt.bc', 'Mt', 'Chap')) {
  m = match.arg(m)

  if (!requireNamespace("FSA", quietly = TRUE) & m == 'Chap') {
    stop("FSA package needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("Rcapture", quietly = TRUE) & m %in% c('Mt.bc', 'Mt')) {
    stop("Rcapture package needed for this function to work. Please install it.",
         call. = FALSE)
  }


  trap_df = readxl::read_excel(filepath,
                       1,
                       skip = 5) %>%
    mutate(Species = recode(Species,
                            'steelhead' = 'Steelhead')) %>%
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

  if(m == 'Mt') trap_rate_mr = plyr::ddply(ch_freq,
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

  if(m == 'Mt.bc') trap_rate_mr = plyr::ddply(ch_freq,
                       .(Year, week_num_org),
                       function(x) {
                         mod = try(closedp.bc(x[,c('M', 'C', 'R', 'freq')], dfreq=T, m = 'Mt'), silent = T)
                         if(class(mod)[1] == 'try-error') return(data.frame(N = NA, N_se = NA, p = NA, p_se = NA))
                         mod_N = mod$results['Mt', c('abundance', 'stderr')]
                         p_mean = sum(x$freq[x$M==1]) / mod_N[1]
                         p_se = sqrt(mod_N[2]^2 * (sum(x$freq[x$M==1]) / mod_N[1]^2)^2)
                         return(data.frame(trap_fish = sum(x[x$M==1,'freq']), N = mod_N[1], N_se = mod_N[2], p = p_mean[1], p_se = p_se[1]))
                       },
                       .id = 'week_num_org',
                       .progress = 'text') %>%
    tbl_df()

  if(m == 'Chap') {
    trap_rate_mr = trap_df %>%
      group_by(Year, week_num_org) %>%
      summarise_at(vars(M, C, R), funs(sum)) %>%
      bind_cols(with(., FSA::mrClosed(M = M,
                                      n = C,
                                      m = R,
                                      method = 'Chapman')) %>%
                  summary(incl.SE = T) %>%
                  as.data.frame() %>%
                  tbl_df() %>%
                  slice(-n())) %>%
      mutate(p = M / N,
             p_se = SE * (M / N^2)) %>%
      # using Robson & Regier criteria for valid abundance estimates
      mutate(Valid = if_else((M * C) > (N * 4), T, F)) %>%
      # mutate(Valid = if_else((M * C) > (N * 4) | data$R >= 7, T, F)) %>%
      select(Year, week_num_org, trap_fish = M, N, SE, p, p_se, Valid)
  }


  return(trap_rate_mr)

}
