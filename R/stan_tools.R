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
