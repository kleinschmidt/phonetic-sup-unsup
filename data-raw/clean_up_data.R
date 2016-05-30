#' Generate the parsed data files included with the package.
#'
#' Assumes wd is root directory of package (parent of this directory)

devtools::load_all()

## Supervised/unsupervised experiment

supunsup <- load_and_parse('data-raw/supunsup-ALL-visworld-anonymized.csv')

excludes <- exclusions(supunsup)

supunsup_excluded <- semi_join(supunsup, excludes,
                               by=c('subject', 'assignmentid'))

supunsup_clean <- anti_join(supunsup, excludes,
                            by=c('subject', 'assignmentid'))

devtools::use_data(supunsup, excludes, supunsup_excluded, supunsup_clean,
                   overwrite=TRUE)

## Separate mean shifts

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

# convert csv string with ';' as line separator into data frame.
parse_vwb <- function(data) {
  data %>%
    str_replace_all(';', '\n') %>%
    read_csv(col_names=c('blockname','trial','stim','stimfn','listId','targetId',
                         'targetPos','clickx','clicky','tstart','tend','rt'))
}

process_vwb <- function(data) {
  data  %>%
    mutate(blockname = str_replace(blockname, 'Resp', '')) %>%
    separate(blockname, into = c('block', 'supCond', 'bvotCond', 'pvotCond'),
             sep='_', convert=TRUE) %>%
    separate(listId, c("wordClass", "trueCat", "trialSupCond"), 
             convert = TRUE) %>%
    mutate(respCat = as.factor(substr(targetId, 1, 1))) %>%
    mutate(respP = as.numeric(respCat == "p")) %>% 
    mutate(vot = as.numeric(str_extract(stimfn, "[-0-9]+"))) %>%
    rename(labeled = trialSupCond)
}

results <- 
  read_tsv('data-raw/separatemeans.all.anon.results') %>%
  gather('cond', 'visworld_unsupervised',
         contains('visworld_unsupervised'), na.rm=TRUE) %>%
  mutate(visworld_unsupervised = map(visworld_unsupervised, parse_vwb))

# assignment metadata
separatemeans_assignments <-
  results %>%
  select(subject=workerid_anon, assignmentid, hitid,
         accepttime=assignmentaccepttime, submittime=assignmentsubmittime,
         starts_with('Answer')) %>%
  gather('field', 'value', starts_with('Answer')) %>%
  mutate(field = str_replace(field, 'Answer\\.', '')) %>%
  spread(field, value)

separatemeans <-
  results %>%
  select(subject=workerid_anon, assignmentid, hitid,
         accepttime=assignmentaccepttime, submittime=assignmentsubmittime,
         visworld_unsupervised) %>%
  unnest(visworld_unsupervised) %>%
  mutate(is_test = str_detect(listId, 'test')) %>%
  process_vwb()

# exclusions
bad_class_test <- 
  separatemeans %>%
  filter(is_test) %>% 
  supunsup::bad_classification(vots=c(-10, 80)) %>%
  filter(exclude80PercentAcc)

separatemeans_clean <-
  separatemeans %>%
  anti_join(bad_class_test)

# check list balancing
separatemeans_clean %>%
  group_by(bvotCond, pvotCond, subject) %>%
  summarise() %>%
  tally()

separatemeans_excluded <-
  separatemeans %>%
  right_join(bad_class_test)

# check excluded data
separatemeans_excluded %>%
  ggplot(aes(x=vot, y=respP, linetype=is_test, color=paste(bvotCond, pvotCond))) +
  geom_line(stat='summary', fun.y=mean) +
  geom_point(stat='summary', fun.y=mean) +
  geom_smooth(method=glm, method.args=list(family='binomial'), alpha=0) + 
  facet_wrap(~ subject)

# save everything
devtools::use_data(separatemeans, separatemeans_clean, separatemeans_excluded,
                   separatemeans_assignments, overwrite=TRUE)
                   
