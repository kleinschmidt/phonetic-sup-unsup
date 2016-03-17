## This is data from 

library(dplyr)
library(ggplot2)

d <- read.table('voicedinitial.txt', header=TRUE) %>%
  tbl_df() %>%
  mutate(Subject = factor(Subject)) %>%
  mutate(VOT = VOT * 1000)              # convert VOT to ms

ggplot(d, aes(x=VOT, fill=factor(Prevoiced))) +
  geom_histogram() +
  facet_wrap(~Subject)

d %>%
  group_by(Subject, Prevoiced) %>%
  summarise(VOT_mean = mean(VOT),
            VOT_sd = sd(VOT),
            VOT_var = var(VOT)) %>%
  ggplot(aes(x=VOT_mean, VOT_sd, color=factor(Prevoiced))) +
  geom_point()

d %>% group_by(Subject) %>% summarise(mean(Prevoiced))

d %>%
  group_by(Subject) %>%
  summarise(VOT_mean = mean(VOT),
            VOT_sd = sd(VOT),
            VOT_var = var(VOT))




prior_stats <- d %>%
  filter(Consonant == 'B') %>%
  group_by(Prevoiced, Subject) %>%
  summarise(mean_VOT = mean(VOT),
            sd_VOT = sd(VOT)) %>%
  summarise(mean = mean(mean_VOT),
            sd = mean(sd_VOT, na.rm=TRUE)) %>%
  transmute(prevoiced = as.logical(Prevoiced),
            mean, sd,
            category = factor('b', levels=c('b', 'p')),
            source = 'goldrick2013') %>%
  bind_rows(data.frame(category=factor(c('b', 'p')),
                       mean = c(0, 60),
                       sd = sqrt(c(14, 254)),
                       source='kronrod2012'))

devtools::use_data(prior_stats, overwrite=TRUE)
