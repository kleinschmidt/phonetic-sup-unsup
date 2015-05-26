#' Generate the parsed data files included with the package.
#' 

library(devtools)
devtools::load_all()

supunsup <- load_and_parse('data-raw/supunsup-ALL-visworld-anonymized.csv')

excludes <- exclusions(supunsup)

supunsup_excluded <- semi_join(supunsup, excludes,
                               by=c('subject', 'assignmentid'))

supunsup_clean <- anti_join(supunsup, excludes,
                            by=c('subject', 'assignmentid'))

use_data(supunsup, excludes, supunsup_excluded, supunsup_clean, overwrite=TRUE)
