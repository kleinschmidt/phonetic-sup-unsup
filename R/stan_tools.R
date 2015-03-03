require(parallel)
require(rstan)

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


# convert data to stan input format
supunsup_to_stan <- function(dat) {
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
    mu00 <- 30
    mu0_sd <- 50
    sigma00 <- 10
    sigma0_scale <- 10
  })
}

# create initializer function from data with sensible parameter values
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
      mu0 <- apply(mu, 1, mean)
      sigma0 <- apply(sigma, 1, mean)
      ## prior pseudocounts
      log_kappa <- log_nu <- log(rep(N / M, K))
    })
  }
}
