
bmixfit_EM = function(
  data,
  K = c(2, 2),
  epsilon = 1e-8
)
{
  stopifnot(ncol(data) >= 2)

  fit = NULL
  fit$status.MLE.error = FALSE

  # Fitting
  # cat("Fitting with", K[1], "Binomials and ", K[2], "Beta-Binomials\n")
  K.total = sum(K)

  # Indexing variables and cluster names
  iB = 1:K[1]
  iB2 = 1:K[2]
  iBB = (K[1] + 1):K.total

  Bin.names = NULL
  if(K[1] > 0) Bin.names = paste("Bin", 1:K[1])

  BBin.names = NULL
  if(K[2] > 0) BBin.names = paste("BBin", 1:K[2])

  cluster.names = c(Bin.names, BBin.names)


  # Initial conditions by kmeans clustering of "p" for Binomial/ Beta-Binomial
  frequencies = data[, 1]/data[, 2]
  km = kmeans(frequencies, centers = K.total, nstart = 100)

  centres = sample(km$centers[, 1], replace = FALSE)

  # Initial conditions are taken as follows:
  # - Binomials: cluster centres
  # - BetaBinomials: MLE fit from clustering assignement
  # In all cases some randomness is added -- just to avoid deterministic fits

  # Binomial distribution centred at the value -- easy one
  B.params = NULL
  if(K[1] > 0)
  {
    # randomization is repeated untill parameters are valid means in (0, 1)
    repeat {
      B.params = centres[iB] + runif(K[1], min = -0.025, max = 0.025) # U[-0.025, 0.025]
      if(all(B.params > 0 & B.params < 1)) break;
    }
    names(B.params) = cluster.names[iB]
  }

  # Beta-Binomial components are fit from the clustering's assignment of the data
  BB.params = NULL
  if(K[2] > 0) {
    BB.params = sapply(
      names(centres)[iBB],
      function(b) .MLE_BB(data[km$cluster == b, ])
    )

    original = BB.params

    repeat{
      BB.params['mu', ] = BB.params['mu', ] + runif(K[2], min = -0.025, max = 0.025) # U[-0.025, 0.025]

      if(all(BB.params['mu', ] > 0 & BB.params['mu', ] < 1)) break;
      BB.params = original
    }

    colnames(BB.params) = cluster.names[iBB]
  }


  # Initial mixing proportions
  pi = table(km$cluster)/nrow(data)
  pi = pi[names(centres)]
  names(pi) = cluster.names


  iterate = function()
  {
    ## Latent variables formulation
    z_nk = matrix(NA, nrow = nrow(data), ncol = K.total)
    colnames(z_nk) = cluster.names

    ## Latent variables
    for(i in 1:nrow(data))
    {
      if(K[1] > 0)
        z_nk[i, iB] = sapply(
          iB,
          function(w) { dbinom(data[i, 1], size = data[i, 2], prob = B.params[w], log = TRUE) + log(pi[w]) }
        )

      if(K[2] > 0)
        z_nk[i, K[1] + iB2] = sapply(
          iB2,
          function(w) { dbetabinom(data[i, 1], size = data[i, 2], prob = BB.params['mu', w], rho = BB.params['rho', w], log = TRUE) + log(pi[w + K[1]]) }
        )
    }

    z_nk
  }


  NLL = .Machine$integer.max
  repeat
  {
    ############### E-step
    z_nk = iterate()

    # Normalization constant, exponentiated to get actual probabilities and with NLL
    Z = apply(z_nk, 1, .log_sum_exp)
    z_nk = z_nk - Z
    z_nk  = apply(z_nk, 2, exp)

    epsilon.conv = -sum(Z) - NLL
    NLL   = -sum(Z)

    # Update mixing proportions for each cluster from the Sum of responsibilities
    pi  = colSums(z_nk)/nrow(data)

    ############### M-step

    # Binomials are analytical
    if(K[1] > 0)
    {
      for (k in iB)
        B.params[k] = z_nk[,k] %*% data[, 1] / z_nk[, k] %*% data[, 2]
    }

    # Beta-Binomials are note: we use a functional of the negative logLik, which we minimize
    if(K[2] > 0)
    {
      for (k in iB2){

        # Box constraints are computed ensuring that current estimates are in the range
        lmu = min(BB.params['mu', k] - 1e-6, 1e-6)
        umu = min(BB.params['mu', k] - 1e-6, 1-1e-6)
        lrho = min(BB.params['rho', k], 1e-6)

        # Numerical fit -- it can be instable, we force a stop when we catch an error
        tryCatch({

          MLE.fit = stats4::mle(
            minuslogl = .NLLBetaBinMix(k + K[1], data, z_nk, pi),
            start = list(mu = BB.params['mu', k] + runif(1), rho = BB.params['rho', k] + runif(1)),
            method = "L-BFGS-B",
            lower = c(lmu, lrho),
            upper = c(umu, Inf)
          )

          BB.params['mu', k] = as.numeric(stats4::coef(MLE.fit)['mu'])
          BB.params['rho', k] = as.numeric(stats4::coef(MLE.fit)['rho'])
        },
        error = function(e)
        {
          cat(crayon::red("MLE error, forcing stop."))
          epsilon = epsilon.conv - 0.01

          fit$status.MLE.error = TRUE
        })
      }
    }

    # Stop criterion
    if(epsilon.conv < epsilon) break;
  }

  ## Compute clustering assignment
  labels =  unlist(
    apply(z_nk, 1,
          function(x) {
            names(which.max(x))
          }))

  npar = K.total + K[1] + 2 * K[2]

  N = nrow(data)
  BIC = 2 * NLL + log(N) * npar
  entropy = -sum(z_nk * log(z_nk), na.rm = TRUE) # will be 0 for w=1

  # Score each model via ICL
  ICL = BIC + entropy

  fit$K = K
  fit$B.params = B.params
  fit$BB.params = BB.params
  fit$labels = labels
  fit$z_nk = z_nk
  fit$pi = pi
  fit$ICL = ICL
  fit$BIC = BIC
  fit$NLL = NLL
  fit$entropy = entropy


  class(fit) = "bmix"

  fit
}
