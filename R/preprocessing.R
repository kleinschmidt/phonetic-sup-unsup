library(dplyr)
library(tidyr)
library(stringr)
library(data.table)

#' Load raw data from .csv file
#'
#' A couple of steps are requied for reading and cleaning the raw data:
#'
#' 1. Split the block condition information, of the form `visworld_supervised_30`,
#'    into the useful parts (the overall supervision condition and /b/ VOT)
#' 2. Split the list ID (of the form `BEACH_p_supervised`) into its useful parts,
#'    which are the word class, actual category, and whether this _trial_ is
#'    supervised or unsupervised (only matters in the supervised condition)
#' 3. Pull out the category of the image clicked on each trial (and convert to a
#'    0/1 b/p response variable for later analysis)
#' 4. Convert the /b/ vot condition variable to a factor to thwart ggplot defaults
#' 5. Extract the numeric VOT of the actual stimulus.
#'
#' The `tidyr::separate` function really comes in handy with the first couple,
#' and automagically cleans up the data frame by removing the original variabls.
#' 
#' @param filename - Filename of the data file to load.
#' @examples
#' data <- load_and_parse('data/supunsup-ALL-visworld.csv')
load_and_parse <- function(filename) {
  tbl_df(read.csv(filename, header=TRUE)) %>%
    separate(blockname, c('block', 'supCond', 'bvotCond'), sep='_', convert=TRUE) %>%
    separate(listId, c('wordClass', 'respCategory', 'trialSupCond'), convert=TRUE) %>%
    mutate(respCat = as.factor(substr(targetId, 1, 1))) %>%
    mutate(respP = as.numeric(respCat=='p')) %>%
    mutate(bvotCond = as.factor(bvotCond)) %>%
    mutate(vot = as.numeric(str_extract(stimfn, '[-0-9]+'))) %>%
    mutate(labeled = ifelse(supCond == 'unsupervised', 'unlabeled',
             ifelse(trialSupCond == 'unsupervised', 'unlabeled', 'labeled')))
}

#' Detect repeat subjects
#'
#' @param data - Parsed data frame
#' @return A data.frame with one row per assignment, and the rank of the
#' submittime of each.  Rank > 1 means it's a repeat for that subject
repeat_subjects <- function(data_) {
  data_ %>% 
    group_by(assignmentid,submittime,subject) %>%
    summarise() %>%
    group_by(subject) %>%
    mutate(rank=row_number(as.POSIXct(strptime(submittime, "%a %b %d %X EDT %Y")))) %>%
    arrange(subject,submittime)
}

#' Detect poor performers
#'
#' Basic check: find people who are classifying the endpoints inconsistently or
#' inaccurately.  One way to measure quality is by the fitted log-odds of
#' /p/ response at 0 and 70 ms (continuum endpoints, for all intents
#' and purposes).  This measures how consistent people are (in a
#' smoothed way) at the ends of the continuum. Intuitively, we want to
#' throw out anyone who doesn't ever consistently get both /b/ and
#' /p/.  So we'll set a cutoff of 80% "correct" at either end (meaning
#' logodds of a /p/ response greater than about 1.4 (`qlogis(0.8)`) at
#' the /p/ end of 70ms and less than -1.4 at the /b/ end.
#' 
bad_classification <- function(data_) {
  contrasts(data_$trialSupCond) <-
    matrix(c(1, -1), nrow=2,
           dimnames = list(c('sup', 'unsup'), 'sup'))

  ## Fit GLM to each subject/assignment
  bysub_withsup_glms <- data_ %>%
    group_by(subject, assignmentid) %>%
    do(fit = glm(respP ~ vot * trialSupCond, data=., family='binomial'))

  ## Get fitted log-odds for 0ms and 70ms stimuli, find minimum correct log-odds,
  ## and mark people for exclusion if the minimum correct less than logit(80%)
  bysub_withsup_end_logodds <- bysub_withsup_glms %>%
    mutate(lo0ms = coef(fit)[1], lo70ms = coef(fit)[1] + 70*coef(fit)[2]) %>%
    select(subject, assignmentid, lo0ms, lo70ms) %>%
    mutate(loMinCorrect = min(lo0ms * -1, lo70ms)) %>%
    mutate(exclude80PercentAcc = loMinCorrect < qlogis(0.8))
}

#' List assignments to exclude from analysis for poor performance or repeats
#'
#' @param data - parsed data frame.
#' @return A data.frame with one row per excluded assignment.  Column `rank` has
#' n>1 for repeat takers, and NA otherwise.  Column loMinCorrect has the minimum
#' log-odds correct for either endpoint of the continuum (0ms or 70ms VOT).
exclusions <- function(data_) {
  repeats <- repeat_subjects(data_)
  bad_subjects <- bad_classification(data_)

  excludes <- full_join(filter(bad_subjects, exclude80PercentAcc),
                        filter(repeats, rank>1))
  return(excludes)
}
