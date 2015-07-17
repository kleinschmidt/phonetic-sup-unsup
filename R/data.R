
#' Excludes
#'
#' Subjects excluded from analysis because they previously participated, or
#' because they didn't consistently classify the 0ms or 70ms VOT stimuli
#' sufficiently consistently (as extrapolated by a logistic GLM). Generated
#' automatically by \code{\link{exclusions}}.
#'
#' @format A data frame with 8 variables:
#' \describe{
#' \item{\code{subject}}{Anonymized MTurk worker identifier}
#' \item{\code{assignmentid}}{Unique ID for the excluded assignment}
#' \item{\code{lo0ms}}{GLM-extrapolated log-odds of /p/ response at 0ms VOT}
#' \item{\code{lo70ms}}{GLM-extrapolated log-odds of /p/ response at 70ms VOT}
#' \item{\code{loMinCorrect}}{The minimum log-odds of a correct response for
#'   either 0ms or 70ms}
#' \item{\code{exclude80PercentAcc}}{Logical: whether assignment is excluded
#'   based on endpoint accuracy}
#' \item{\code{submittime}}{Time assignment was submitted (from determining
#'   which assignment was submitted first for subjects with repeats)}
#' \item{\code{rank}}{Rank of this assignment based on \code{submittime}.
#'   Greater than 1 means a repeat to be excluded}
#' }
"excludes"

#' Supunsup
#'
#' The whole dataset, parsed by \code{\link{load_and_parse}}.
#'
#' This data comes from a phonetic adaptation experiment, where each listener
#' hears a different distribution of VOTs and classifies each one as a /b/ or a
#' /p/.  The VOTs are presented in the form of /b/-/p/ minimal pair words
#' (beach/peach, bees/peas, and beak/peak).  On each trial, the subject hears a
#' word from one of these pairs with a VOT drawn from their specific
#' distribution, and click on a picture to indicate the word they heard.
#'
#' There are two, crossed conditions:
#'
#' \describe{
#' \item{Supervision}{For subjects in the 'unsupervised' condition, all trials
#'   were unlabeled, and either the /b/ or /p/ response was appropriate.  In
#'   the 'supervised' and 'mixed' conditions, some trials were labeled.  On
#'   these trials, only one of the response pictures matched the end of the
#'   word, effectively labeleing the VOT.  For instance, on an unlabeled
#'   beach/peach trial, the subject could click on a picture of a beach or a
#'   peach.  On a labeled beach/peach trial, there might be a picture of a
#'   beach and a picture of a peak, which labeled the VOT of the word as a /b/.
#'   The 'supervised' and 'mixed' conditions differed only in how the labeled
#'   trials were distributed over the different VOT values a subject heard.}
#' \item{VOT distribution}{Each subject heard one of five distributions of VOTs.
#'   All the distributions are bimodal, with the means separated by 40ms VOT,
#'   and differ only in the location: -10/30ms, 0/40ms, 10/50ms, 20/60ms, or
#'   30/70ms.}
#' }
#'
#' There are two different outcome variables:
#'
#' \describe{
#' \item{Classification}{Listeners' percepts measured by which of the two
#'   possible pictures they clicked on.  Variable \code{respCat} is a two-level
#'   factor (b or p), and \code{respP} codes this as a binary (0 or 1) value
#'   suitable for logistic regression.}
#' \item{Reaction time}{Listeners initiated each trial by clicking on a "light"
#'   in the center of the screen.  The sound file played immediately after this
#'   click.  Reaction time is coded in \code{rt} as the number of milliseconds
#'   between the start of the sound file and the response (click on picture).}
#' }
#'
#' @format A data frame with 89,244 observations of 28 variables:
#' \describe{
#' \item{\code{subject}}{Anonymized MTurk worker identifier}
#' \item{\code{assignmentid}}{Unique ID for the excluded assignment}
#' \item{\code{hitid}}{The (non-unique) ID of the HIT for this assignment}
#' \item{\code{experiment}}{Name of the experiment:
#'   'supervised-unsupervised-vot'}
#' \item{\code{accepttime}}{Time assignment was initially accepted by
#'   subject}
#' \item{\code{submittime}}{Time assignment was finished and submitted.}
#' \item{\code{block}}{The type of block for this dataset ('visworld')}
#' \item{\code{supCond}}{Factor: The supervision condition: one of 'supervised',
#'   'unsupervised', or 'mixed'}.
#' \item{\code{bvotCond}}{Factor: the location of the lower cluster of the
#'   bimodal input in ms VOT, either '0', '10', '20', '30', or '40'.}
#' \item{\code{trial}}{Trial number, starting at 0.}
#' \item{\code{stim}}{Numeric: Unique ID of the stimulus file played on that
#'   trial.}
#' \item{\code{stimfn}}{Factor: Filename of the stimulus.}
#' \item{\code{wordclass}}{Factor: Minimal pair for this stimulus, one of
#'   'BEACH', 'BEAK', or 'BEES'.}
#' \item{\code{respCategory}}{The \strong{intended} (correct) category of the
#'   response, \strong{not the listener's actual choice} (that's
#'   \code{respCat}).}
#' \item{\code{trialSupCond}}{Factor: Whether this \emph{trial} was 'supervised'
#'   (labeled) or 'unsupervised' (unlabeled).  Use \code{labeled} instead.}
#' \item{\code{targetId}}{The name of the image that the listener clicked on.}
#' \item{\code{targetPos}}{The location of the clicked image.}
#' \item{\code{clickx}}{Relative to left of experiment window.}
#' \item{\code{clicky}}{Relative to top of experiment window.}
#' \item{\code{tstart}}{System time (in ms) of trial-start click.}
#' \item{\code{tend}}{System time (in ms) of response click.}
#' \item{\code{rt}}{Response time, in ms (difference between \code{tstart} and
#'   \code{tend}.}
#' \item{\code{respCat}}{Factor: The category ('b' or 'p') of the listner's
#'   response (based on \code{targetId}).}
#' \item{\code{trueCat}}{Factor: The intended ('correct') category ('b' or 'p').
#'   Depends on the input distribution (\code{bvotCond}).}
#' \item{\code{vot}}{Numeric: The VOT of the stimulus for this trial (ms).}
#' \item{\code{labeled}}{Factor: Whether this trial was 'labeled' or
#'   'unlabeled'.}
#' }
#' 
"supunsup"

#' Supunsup_clean
#'
#' The clean data set, with excluded assignments removed.
"supunsup_clean"

#' Supunsup_excluded
#'
#' Data from excluded assignments.
"supunsup_excluded"
