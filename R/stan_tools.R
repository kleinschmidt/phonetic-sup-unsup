require(parallel)
require(rstan)

#' Run stan chains in parallel
#'
#' Uses `mclapply` as backend, and `sflist2stanfit` to convert to a single,
#' multi-chain `stanfit` object.  If you don't supply the `fit` parameter
#' (previously compiled Stan model object) then it will compile one and re-use
#' it for all the chains.  Returns a single
#'
#' @return A single `stanfit` object with `chains` chains.
#'
#' @param data same as `stan`
#' @param file filename for a stan model file.  ignored if `fit` also
#' specified.
#' @param fit fitted `stan` object.  Overrides any provided `file` argument.
#' @param chains number of chains to run (default: 4).
#' @param cores number of cores to use, which controls how many chains can be
#' run simultaneously. (passed to `mclapply`, so see there for more information)
#' (default: `chains`)
#' @param rng_seed optional, passed to each chain.  (default: 1).
#' @param ... additional arguments passed to `stan` for each chain.
#'
#' @export
par_stan <- function(data,
                     file,
                     fit = stan(file, data=data, chains=0),
                     chains = 4,
                     cores = chains,
                     rng_seed = 1,
                     ...) {
  sflist <- mclapply(1:chains, mc.cores=cores,
                     function(n) stan(fit = fit, data = data,
                                      seed = rng_seed,
                                      chains = 1, chain_id = n,
                                      refresh = -1, ...))
  return(sflist2stanfit(sflist))
}


#' convert data to stan input format for the mog_*.stan models
#'
#' @export
supunsup_to_stan_mog <- function(dat) {
  within(list(), {
    y <- dat$vot
    
    z <- dat$respP + 1

    subject <- dat$subject %>%
      factor %>%                        # drop missing subject levels
      as.numeric

    N <- length(y)
    M <- max(subject)
    K <- max(z)

    ## prior hyperparameters
    mu_00 <- 30
    mu_0_sd <- 50
    sigma_00 <- 10
    sigma_0_scale <- 10
  })
}

#' convert data to stan input format for the conj_*.stan models
#'
#' @export
supunsup_to_stan_conj <- function(dat) {

  dat %>%
    filter(supCond == 'unsupervised') %>%
    mutate(trueCat = respCategory,
           subjNum = as.numeric(factor(subject)),
           trueCatNum = as.numeric(trueCat),
           respCatNum = as.numeric(respCat))

  test_responses <- dat %>%
    group_by(subjNum, vot, respCat) %>%
    tally %>%
    spread(key=respCat, value=n, fill=0)

  within(list(), {
    x <- dat$vot
    y <- dat$subjNum
    z <- dat$trueCatNum

    n <- length(x)                        # num training observations
    m <- length(unique(z))                # num categories
    l <- length(unique(y))                # num subjects
    
    x_test <- test_responses$vot
    y_test <- test_responses$subjNum
    z_test_counts <- test_responses %>% select(b:p) %>% as.matrix

    n_test <- length(x_test)
  })
  
}


#' create initializer function from data with sensible parameter values
#' @export
mod_param_init <- function(dat) {
  dat_temp <- with(dat, data.frame(vot=y, resp=z, subject=subject))
  K <- dat$K
  M <- dat$M
  N <- dat$N
  function() {
    within(list(), {
      logit_w_prior <- 0
      theta <- rep(1/K, K)
      ## category-by-subject mean VOTs
      mu <- dat_temp %>%
        group_by(subject, resp) %>%
        summarise(vot = mean(vot)) %>%
        spread(key = subject, value = vot) %>%
        select(-resp) %>%
        as.matrix
      ## category-by-subject VOT sds
      sigma <- dat_temp %>%
        group_by(subject, resp) %>%
        summarise(sd = sd(vot)) %>%
        spread(key = subject, value = sd) %>%
        select(-resp) %>%
        as.matrix
      ## grand mean of means and sds
      mu_0 <- apply(mu, 1, mean)
      mu_sigma <- apply(mu, 1, sd)
      sigma_0 <- apply(sigma, 1, mean)
      ## prior pseudocounts
      kappa_0 <- nu_0 <- N / M
    })
  }
}
