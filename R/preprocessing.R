#' These are functions for pre-processing raw data, including calculating
#' exclusions based on repeat HITs and poor performance.

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
#'
#' @export
load_and_parse <- function(filename) {
  tbl_df(read.csv(filename, header=TRUE)) %>%
    separate(blockname, c('block', 'supCond', 'bvotCond'), sep='_', convert=TRUE) %>%
    separate(listId, c('wordClass', 'respCategory', 'trialSupCond'), convert=TRUE) %>%
    mutate(respCat = as.factor(substr(targetId, 1, 1))) %>%
    mutate(respP = as.numeric(respCat=='p')) %>%
    mutate(trueCat = respCategory) %>%
    mutate(bvotCond = as.factor(bvotCond)) %>%
    mutate(vot = as.numeric(str_extract(stimfn, '[-0-9]+'))) %>%
    mutate(labeled = ifelse(supCond == 'unsupervised', 'unlabeled',
             ifelse(trialSupCond == 'unsupervised', 'unlabeled', 'labeled'))) %>%
    select(-condition, -errors)
}

#' Detect repeat subjects
#'
#' @param data - Parsed data frame
#' @return A data.frame with one row per assignment, and the rank of the
#' submittime of each.  Rank > 1 means it's a repeat for that subject
#'
#' @export
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
#' @param data_ Parsed data frame
#' @param vots = c(0,70) where to check predicted classification.
#' @return A data.frame of subjects/assignments with column
#' `exclude80PercentAcc` (whether this assignment is excluded by the criterion
#' of at least 80% accuracy on 0ms and 70ms VOT). also includes min_correct_logodds.
#'
#' @export
bad_classification <- function(data_, vots=c(0, 70)) {
  ## Fit GLM to each subject/assignment
  if (length(unique(data_$trialSupCond)) > 1) {
    sup_contrasts <-
      matrix(c(1, -1), nrow=2,
             dimnames = list(c('sup', 'unsup'), 'sup'))
    fit_glm <- function(d) glm(respP ~ vot * trialSupCond, data=d, family='binomial',
                               contrasts = list(trialSupCond = sup_contrasts))
  } else {
    fit_glm <- function(d) glm(respP ~ vot, data=d, family='binomial')
  }

  d <- data_frame(vot=vots, correct=c(-1,1))

  ## Get fitted log-odds for 0ms and 70ms stimuli, find minimum correct log-odds,
  ## and mark people for exclusion if the minimum correct less than logit(80%)
  data_ %>%
    group_by(subject, assignmentid) %>%
    tidyr::nest() %>%
    mutate(fit = purrr::map(data, fit_glm),
           logodds = purrr::map(fit, ~ d %>% mutate(pred=predict(.x, d)))) %>%
    tidyr::unnest(logodds) %>%
    mutate(correct_logodds = correct * pred) %>%
    group_by(subject, assignmentid) %>%
    summarise(min_correct_logodds = min(correct_logodds)) %>%
    mutate(exclude80PercentAcc = min_correct_logodds < qlogis(0.8))

}

#' Apply exclusion criteria
#'
#' List assignments to exclude from analysis for poor performance or repeats
#'
#' @param data - parsed data frame.
#' @return A data.frame with one row per excluded assignment.  Column `rank` has
#' n>1 for repeat takers, and NA otherwise.  Column loMinCorrect has the minimum
#' log-odds correct for either endpoint of the continuum (0ms or 70ms VOT).
#'
#' @export
exclusions <- function(data_) {
  repeats <- repeat_subjects(data_)
  bad_subjects <- bad_classification(data_)

  excludes <- full_join(filter(bad_subjects, exclude80PercentAcc),
                        filter(repeats, rank>1))
  return(excludes)
}
