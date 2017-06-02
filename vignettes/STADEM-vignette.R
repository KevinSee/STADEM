## ----setUp_options, echo = F, message=FALSE, warning=FALSE---------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)

## ----load_packages-------------------------------------------------------
library(lubridate)
library(tidyverse)
library(STADEM)

## ----prep_JAGS-----------------------------------------------------------
# compile everything into a list to pass to JAGS
jags_data_list = prepJAGS(stadem_list[['weeklyData']])

## ----summary_total_escapement--------------------------------------------
mod$summary[grep('X.tot', rownames(mod$summary)),] %>%
  pander::pander()

## ----compile_time-series_data--------------------------------------------
library(stringr)
week_est = mod$summary[grep('^X.all', rownames(mod$summary)),] %>%
  as.data.frame() %>%
  mutate(var = rownames(.),
         week = as.integer(str_extract(var, "[0-9]+")),
         param = str_extract_all(var, "[:alpha:]+", simplify = T)[,3],
         param = ifelse(param == '', 'all', param)) %>%
  tbl_df() %>%
  select(var, param, week, everything()) %>%
  left_join(stadem_list[['weeklyData']] %>%
              filter(window_open | trap_open) %>%
              mutate(week = 1:n()))

## ----time-series_plot, fig.height = 5, fig.width = 7---------------------
# plot time-series of model estimates, window counts and trap estimates
week_est %>%
  filter(param == 'all') %>%
  filter(tot_tags > 0) %>%
  ggplot(aes(x = Start_Date,
             y = `50%`)) +
  geom_ribbon(aes(ymin = `2.5%`,
                  ymax = `97.5%`),
              alpha = 0.2) +
  geom_line(aes(y = win_cnt / (day_tags / tot_tags),
                color = 'Window (adj)')) +
  geom_point(aes(y = win_cnt / (day_tags / tot_tags),
                 color = 'Window (adj)')) +
  geom_line(aes(y = win_cnt,
                color = 'Window')) +
  geom_point(aes(y = win_cnt,
                 color = 'Window')) +
  geom_line(aes(y = trap_est,
                color = 'Trap')) +
  geom_point(aes(y = trap_est,
                 color = 'Trap')) +
  geom_line(aes(color = 'Model')) +
  geom_point(aes(color = 'Model')) +
  scale_color_manual(values = c('Model' = 'black',
                                'Window' = 'lightblue',
                                'Window (adj)' = 'blue',
                                'Trap' = 'red')) +
  theme(legend.position = 'bottom') +
  theme_bw() +
  labs(x = 'Date',
       y = 'Estimate',
       color = 'Source',
       title = paste('All', spp, 'in', yr))


