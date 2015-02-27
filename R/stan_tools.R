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
