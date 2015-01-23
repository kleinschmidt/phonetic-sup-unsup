library(lme4)


#' Set center, scale, and set contrasts for lmer modeling
#'
#' @param .data - Parsed data frame, with at least columns for vot, trial,
#' bvotCond, and supCond
mutate_for_lmer <- function(.data) {
  .data %>%
    ungroup %>%
    mutate(vot_rel.s = scale((vot - as.numeric(as.character(bvotCond))) / 10,
                             scale=FALSE),
           trial.s = scale(trial / max(trial), scale=FALSE),
           bvotCond.s = scale(as.numeric(as.character(bvotCond)) / 10, scale=FALSE),
           supervised = scale(as.numeric(supCond) * -1, scale=FALSE))
}


#' Helper function that rescales one variable based on another (previously
#' scaled), handling presence or absence of centering or scaling appropriately
#'
#' @param x - vector that needs to be scaled.
#' @param x.scaled - vector that was already scaled.
#' 
rescale <- function(x, x.scaled) {
  x.rescaled <- as.matrix(x)
  if (! is.null(attr(x.scaled, 'scaled:center'))) {
    center <- attr(x.scaled, 'scaled:center')
    x.rescaled <- sweep(x.rescaled, 2L, center, check.margin=FALSE)
    attr(x.rescaled, 'scaled:center') <- center
  }

  if (! is.null(attr(x.scaled, 'scaled:scale'))) {
    scale <- attr(x.scaled, 'scaled:scale')
    x.rescaled <- sweep(x.rescaled, 2L, scale, "/", check.margin=FALSE)
    attr(x.rescaled, 'scaled:scale') <- scale
  }

  return(x.rescaled)
}

#' Split data_$trial into n bins.
#'
#' @param data_ - data.frame with at least a `trial` column.
#' @param n_bins - How many bins to split the trials into (defaults to 3)
#' @return - A data.frame with columns trial (mean trial in bin) and trial_range
#' (factor with range of trials as strings, ordered correctly).
bin_trials <- function(data_, n_bins=3) {
  data_ %>%
    mutate(thirds = ntile(trial, n_bins)) %>%
    group_by(thirds) %>%
    summarise(trial_range = paste(min(trial), '-', max(trial)), 
              trial = round(mean(trial))) %>%
    ungroup() %>%
    mutate(trial_range = factor(trial_range, levels=trial_range))
}

#' Generate a data.frame suitable for passing to lme4::predict.merMod.
#'
#' @param data_inc - Raw data, what was used to generate the model input
#' @param data_mod - Model input (raw data at least passed through mutate_for_lmer)
#' 
make_prediction_data <- function(data_inc, data_mod) {
  trial_thirds <- bin_trials(data_inc, 3)

  data_pred <- expand.grid(vot = seq(-10, 80, by=1),
                           trial = trial_thirds$trial,
                           bvotCond = factor(levels(data_inc$bvotCond)),
                           supCond = factor(levels(data_inc$supCond)),
                           KEEP.OUT.ATTRS = FALSE) %>%
                             tbl_df %>%
                             inner_join(y=trial_thirds, copy=TRUE) %>%
                             mutate_for_lmer

  ## rescale variables that were scaled in model fit: vot, trial, bvotCond
  data_pred <- data_pred %>%
    mutate(vot_rel.s = rescale((vot - as.numeric(as.character(bvotCond))) / 10,
             data_mod$vot_rel.s),
           trial.s = rescale(trial / max(data_mod$trial), data_mod$trial.s),
           bvotCond.s = rescale(as.numeric(as.character(bvotCond)) / 10,
             data_mod$bvotCond.s),
           supervised = rescale(as.numeric(supCond) * -1, data_mod$supervised),
           type = factor('predicted', levels=c('actual', 'predicted')))

  return(data_pred)
}

#' predict log-odds of /p/ responses based on just the fixed effects of the
#' fitted model, and plot.
#'
#' @param dat - data suitable for calling lme4::predict.merMod (e.g. output of
#' make_prediction_data).
#' @param fit - glmer fitted model.
#' @param show_se=FALSE - whether or not to draw the confidence intervals
#' (based on the standard error of the fixed effects).
#' @return The ggplot2 plot object.
#' @examples
#' \dontrun{
#' data_mod <- mutate_for_lmer(data_inc)
#' fit <- glmer(respP ~ vot_rel.s * bvotCond.s * supervised * trial.s +
#'              (vot_rel.s * trial.s | subject),
#'              data=data_mod, family='binomial',
#'              control= glmerControl(optimizer='bobyqa'))
#' data_pred <- make_prediction(data_inc, data_mod)
#' predict_and_plot(data_pred, fit, show_se=TRUE)
#' }
predict_and_plot <- function(dat, fit, show_se=FALSE, ...) {
  dat$pred.logodds <- predict(fit, dat, re.form=NA)
  ## convert log odds to probability /p/ response
  dat <- dat %>% mutate(pred.resp = plogis(pred.logodds))
  p <- ggplot(dat,
              aes(x=vot, color=bvotCond,
                  linetype=supCond,
                  group=interaction(bvotCond, supCond, trial_range))) +
    geom_line(aes(y=pred.resp)) +
    facet_grid(type~trial_range)
  if (show_se) {
    form <- as.formula(lme4:::nobars(formula(fit))[-2])
    mm <- model.matrix(form, data=dat)
    ses <- sqrt(diag(mm %*% tcrossprod(vcov(fit_m), mm)))
    dat <- dat %>% mutate(pred.logodds.se=ses,
                          ci.logodds.low=pred.logodds - 1.96*pred.logodds.se,
                          ci.logodds.high=pred.logodds + 1.96*pred.logodds.se,
                          ci.low=plogis(ci.logodds.low),
                          ci.high=plogis(ci.logodds.high))
    return(p + geom_ribbon(aes(ymin=ci.low, ymax=ci.high, fill=bvotCond),
                           data=dat,
                           alpha=0.5,
                           size=0))
  } else {
    return(p)
  }
}
