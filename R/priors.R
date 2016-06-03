sd_noise = sqrt(82)

#' Convert data frame of category statistics into likelihoods
#'
#' @param stats data frame with columns category, mean, and sd
#' @param noise_sd standard deviation of sensory noise (added, in variance
#'   space, to all category sds)
#' @param xlims where to compute liklelihood.
#' @return data frame with columns vot, category, and lhood
#' @export
stats_to_lhood <- function(stats, noise_sd=sd_noise, xlim=c(-30, 90)) {
  stats %>%
    group_by(category, mean, sd) %>%
    do(data.frame(vot=seq(xlim[1], xlim[2], 0.5))) %>%
    ungroup() %>%
    mutate(lhood = dnorm(vot, mean, sqrt(sd^2 + noise_sd^2))) %>%
    select(-mean, -sd)
}

#' Convert category likelihoods into categorization function
#'
#' @param lhood data frame with likelihoods (see \link{\code{stats_to_lhood}}).
#' @return data frame with columns category and prob_p
#' @export
lhood_to_classification <- function(lhood) {
  lhood %>%
    spread(category, lhood) %>%
    mutate(prob_p = p / (p+b))
}
