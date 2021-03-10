#### Internal functions

.MLE_BB = function(data) {
  s_n = data[, 1]
  t_n = data[, 2]

  # require(VGAM)

  # MLE BetaBin
  fit = Coef(VGAM::vglm(cbind(s_n, t_n - s_n) ~ 1, betabinomial, trace = FALSE))

  return(fit)
}

.NLLBetaBinMix = function(k, x, z_nk, pi)
{
  f = function(mu, rho) # NLL
  {
    if(rho < 0 | mu > 0.99 | mu < 0.00001) v = .Machine$integer.max
    else
    # z[x,k] * { log pi[k] + log betabin(x | mu, rho) }
    v = -1 * z_nk[, k] %*% (log(pi[k]) + dbetabinom(x[, 1], size = x[, 2], prob = mu, rho = rho, log = TRUE))

     # print(paste(mu, rho, v))

    v
  }

  return(f)
}

#### Aux function to do some classical tricks for numerical stability
.log_sum_exp = function(x) {
  # Computes log(sum(exp(x))
  offset <- max(x)
  s <- log(sum(exp(x - offset))) + offset
  i <- which(!is.finite(s))
  if (length(i) > 0) { s[i] <- offset }
  return(s)
}

.betaMV2AB = function(mu, var)
{
  if(var == 0) var = 1e-9
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(a = alpha, b = beta))
}

