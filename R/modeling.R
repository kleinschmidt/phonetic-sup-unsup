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
    group_by(trial) %>%
    summarise() %>%
    mutate(thirds = ntile(trial, n_bins)) %>%
    group_by(thirds) %>%
    summarise(trial_range = paste('Trials ', min(trial), '-', max(trial)), 
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
           type = factor('fixed effects', levels=c('actual', 'fixed effects')))

  return(data_pred)
}

#' Actually create a model matrix from a merMod object and new data (only
#' fixed effects, and leaves out the y value).
#' 
#' @param fit - Fitted merMode object
#' @param dat - New data to generate a model.matrix from. See model.matrix.
mer_model_matrix <- function(fit, dat, ...) {
  form <- as.formula(lme4:::nobars(formula(fit))[-2])
  model.matrix(form, data=dat, ...)
}

#' Generate standard errors for predictions of a mixed effects
#' model, based on the variance-covariance matrices of the fixed effects.
#' Returns the standard error, (square root of variance), of the predictions
#' by default, but can return the full variance-covariance matrix
#'
#' @param fit - Fitted merMod object
#' @param dat - Data to generate fixed effects standard errors for
#' @param full_covar=FALSE - Whether to return the full covariance matrix
#' (instead of just the square root of the diagonal elements)
predict_se <- function(fit, mm=mer_model_matrix(fit, dat),
                       dat=NA, full_covar=FALSE) {
  ## mm <- mer_model_matrix(fit, dat)
  if (full_covar) {
    return(mm %*% tcrossprod(as.matrix(vcov(fit)), mm))
  } else {
    return(sqrt(diag(mm %*% tcrossprod(as.matrix(vcov(fit)), mm))))
  }
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
    ses <- predict_se(fit, dat)
    dat <- dat %>% mutate(pred.logodds.se=ses,
                          ci.logodds.low=pred.logodds - 1.96*pred.logodds.se,
                          ci.logodds.high=pred.logodds + 1.96*pred.logodds.se,
                          ci.low=plogis(ci.logodds.low),
                          ci.high=plogis(ci.logodds.high))
    return(p + geom_ribbon(aes(ymin=ci.low, ymax=ci.high, fill=bvotCond),
                           data=dat,
                           alpha=0.5,
                           color=NA))
  } else {
    return(p)
  }
}

#' Get the category boundary based on an glmer fit and model data
#'
#' Works by computing predictions at each level of bvotCond and supCond, for
#' vot predictor of 0 (to get y intercept) and 1 (to get VOT slope).  Then finds
#' the x intercept, and adjusts for how VOT is scaled and shifted to return the
#' actual VOT value where the boundary is.
#'
#' @param dat_mod - Model input data (data.frame with at least columns for
#' bvotCond, bvotCond.s, supervised (numeric supCond predictor), and supCond).
#' @param fit - fitted glmer model object
#' @param trials_prop - (default: 5/6) which point in the experiment to estimate
#' the category boundary at.
#' @return data.frame with columns bvotCond, supCond, and boundary_vot
category_boundaries <- function(dat_mod, fit,
                                trials_prop = 5/6) {
  
  boundary_pred_dat <- dat_mod %>%
    group_by(bvotCond, bvotCond.s, supervised, supCond) %>%
    summarise(trial.s = min(trial.s) + diff(range(trial.s))*trials_prop)
  
  ## predicted y intercept (vot_rel.s=0)
  ## predicted slope is (vot_rel.s = 1) - (vot_rel.s = 0)  

  bounds <- boundary_pred_dat %>% 
    group_by(bvotCond, supCond) %>%
    do({
      ## model matrix: intercept and slope
      mm <- matrix(c(1, 0, -1, 1), nrow=2, byrow=TRUE) %*%
        rbind(mer_model_matrix(fit, mutate(., vot_rel.s=0)),
              mer_model_matrix(fit, mutate(., vot_rel.s=1)))
      ## predicted intercept and slope
      pred <- mm %*% fixef(fit)
      pred_covar <- predict_se(fit, mm=mm, full_covar=TRUE)
      x_int <- -pred[1] / pred[2]
      ## shamelessly copied from http://www.stat.cmu.edu/~hseltman/files/ratio.pdf
      x_int_var <- x_int^2 * 
        (pred_covar[1,1]/pred[1]^2 - 
           2*pred_covar[1,2]/(pred[1]*pred[2]) + 
           pred_covar[2,2]/pred[2]^2)

      ## just to satisfy myself that this is a reasonable approximation
      ## samps <- rmvnorm(n=1000000, mean=pred, sigma=pred_covar)
      ## samps_ratio = - samps[, 1] / samps[, 2]
      ## print(c(mean(samps_ratio), x_int, sd(samps_ratio), sqrt(x_int_var)))
      
      data.frame(y_int=pred[1],
                 slope=pred[2],
                 x_int=x_int,
                 x_int_var=x_int_var)
    }) %>%
    mutate(shift = as.numeric(as.character(bvotCond))) %>% 
    mutate(boundary_vot = x_int*10 + shift + 20,
              boundary_vot_se = sqrt(x_int_var) * 10,
              boundary_vot_true = shift+20)

  return(bounds)
}
