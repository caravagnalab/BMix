
#' Title
#'
#' @param data
#' @param K.Binomials
#' @param K.BetaBinomials
#' @param epsilon
#' @param samples
#'
#' @return
#' @export
#'
#' @examples
bmixfit = function(
  data,
  K.Binomials = 0:2,
  K.BetaBinomials = 0:2,
  epsilon = 1e-8,
  samples = 1
)
{
  grid = expand.grid(Sample = 1:samples, B = K.Binomials, BB = K.BetaBinomials,  stringsAsFactors = FALSE)
  grid = grid[ grid$B + grid$BB > 0,  ]
  grid$ICL = NA

  best.score = .Machine$integer.max

  cat("\n\tTotal number of runs: ", nrow(grid), '\n\n')

  cat(sprintf("%10s  | %7s |  %6s | %11s | %10s | %10s\n",
              "Run #", "Samp. #", "Binom.", "Beta-Binom.", "Conv.", "ICL" ))
  cat("------------------------------------------------------------------------------")

  fits = NULL
  for(i in 1:nrow(grid))
  {
    # Loop untill it computes without errors
    success = FALSE
    repeat {
      tryCatch({
        cat(sprintf("\n%10s  | %7s |  %6s | %11s | ", paste0(i, '/', nrow(grid)), grid$Sample[i], grid$B[i], grid$BB[i]))

        # cat('\n',
        #     paste0('#',i,'.'),
        #     "K =", grid$B[i], "Binomials + K =", grid$BB[i], "Beta-Binomials: ")

        # try the EM
        fit = bmixfit_EM(data, K = c(grid$B[i], grid$BB[i]), epsilon = epsilon)

        # if you get here, it will exit
        success = TRUE
        if(success) break;

      },
      error = function(e)
      {
        # cat("\n\tIntercepted error (retrying)\n")
        # print(e)
        cat(crayon::red("error (forcing restart of this computation)"))
      }
      )
    }

    if(fit$status.MLE.error){
      cat(crayon::red(sprintf("%10s", "MLE error")), '| ')
    }
    else cat(crayon::green(sprintf("%10s", "OK")), '| ')

    grid$ICL[i] = fit$ICL

    if(fit$ICL < best.score)
    {
      cat(crayon::green(fit$ICL), '*')
      best.score = fit$ICL
    }
    else cat(fit$ICL)

    fits = append(fits, list(fit))
  }

  best = which.min(grid$ICL)
  best = fits[[best]]
  best$grid.model.selection = grid

  cat("\n\n *** Best with ICL: ", min(grid$ICL), " \n\n")
  print(best)
  best
}


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

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
print.bmix = function(x)
{
  cat("BMix model with K =", sum(x$K), "components:", x$K[1], "Binomials and", x$K[2], "Beta-Binomials.\n")

  cat("\nBinomials\n")
  print(x$B.params)

  cat("\nBeta-Binomials\n")
  print(x$BB.params)

  cat("\nICL:", x$ICL, '\n')
}


#' Title
#'
#' @param fit
#' @param data
#' @param coverage
#' @param annotation
#' @param cex
#' @param palette
#' @param alpha
#'
#' @return
#' @export
#' @import ggplot2
#'
#' @examples
plot.bmix = function(fit, data, coverage = 100, annotation = NULL, cex = 1, palette = 'Set1', palette.heatmap = 'Spectral', alpha = .3, M.tarone = 1000)
{
  # require(ggplot2)

  p = NULL

  K = fit$K

  # Main histogram
  col = colorRampPalette(RColorBrewer::brewer.pal(n = min(sum(K),8), palette))(sum(K))
  names(col) = c(names(fit$B.params), colnames(fit$BB.params))

  df = data.frame(Frequency = data[, 1]/data[, 2],
                  Cluster = fit$labels,
                  Color = col[fit$labels])

  p = ggplot(df, aes(Frequency, fill = Cluster, y = ..count../sum(..count..))) +
    geom_histogram(alpha = alpha, position = 'identity', binwidth = 0.01) +
    labs(
      title = bquote(bold("BMix fit.")~K[B]~"="~.(fit$K[1])~","~K[BB]~"="~.(fit$K[2])),
      subtitle = annotation,
      x = "Observed Frequency", y = "") +
    theme_classic(base_size = 10 * cex) +
    xlim(0, 1) +
    scale_fill_manual(names(col), values = col)+
    guides(fill = guide_legend(title = "Cluster"))




  ##### Density functions
  domain = seq(0, coverage, 1)

  B.params = fit$B.params
  BB.params = fit$BB.params

  means = data.frame(Cluster = c(names(B.params), colnames(BB.params)), value = NA)

  pi = fit$pi

  df = NULL
  lcoverage = paste0(coverage, 'x')
  d = ggplot(df, aes(x = x, fill = Cluster)) +
    labs(
      title = bquote(bold("Density: ")~.(lcoverage)),
      x = "", y = "") +
    theme_classic(base_size = 10 * cex)

  if(K[1] > 0)
  {
    values = lapply(
      1:K[1],
      function(w) { dbinom(domain, size = coverage, prob = B.params[w]) * pi[w] }
    )

    values = lapply(seq(values), function(w) data.frame(
      Cluster = names(B.params)[w], x = domain,
      density = values[[w]]))

    for(i in seq(values)) d = d + geom_line(data = values[[i]], aes(y = density, x = x), colour = col[i])

    means$value[means$Cluster %in% names(B.params)] =  fit$B.params * coverage

    for(i in seq(values)) d = d + geom_vline(data = means, aes(xintercept = value), colour = 'black', linetype = "longdash")
  }

  if(K[2] > 0)
  {
    values = lapply(
      1:K[2],
      function(w) { dbetabinom(domain, size = coverage, prob = BB.params['mu', w],
                               rho = BB.params['rho', w]) * pi[w] }
    )

    values = lapply(seq(values), function(w) data.frame(
      Cluster = colnames(BB.params)[w], x = domain,
      density = values[[w]]))

    for(i in seq(values)) d = d + geom_line(data = values[[i]], aes(y = density, x = x), colour = col[i + K[1]])

    means$value[means$Cluster %in% colnames(BB.params)] =  fit$BB.params['mu', ] * coverage

    for(i in seq(values)) d = d + geom_vline(data = means, aes(xintercept = value), colour = 'black', linetype = "longdash")

  }



  # Model selection heatmap
  ms = fit$grid.model.selection

  ms$id = apply(ms, 1, function(w) paste0(w['B'], w['BB']))
  ms = split(ms, f = ms$id)
  ms = lapply(ms, function(w) w[which.min(w$ICL), ])
  ms = Reduce(rbind, ms)

  ms = ms[order(ms$ICL, decreasing = FALSE), ]
  ms$id = ms$Sample = NULL
  rownames(ms) = NULL

  # ms$ICL = log(ms$ICL)


  best.ICL = min(ms$ICL)
  worst.ICL = max(ms$ICL)
  range.ICL = worst.ICL - best.ICL

  cat("Model selection matrix\n")
  print(ms)

  qn = quantile(ms$ICL, c(.001, .01, .05, .10, .25, .50, .75, 0.99), na.rm = TRUE)
  ms$fICL = cut(ms$ICL, breaks = c(-Inf, qn, Inf), right = FALSE)

  msh = ggplot(ms, aes(B, BB)) +
    geom_tile(aes(fill = fICL), color = "white") +
    scale_fill_brewer(palette = palette.heatmap, direction = -1) +
    ylab("Beta-Binomials") +
    xlab("Binomials") +
    theme(legend.title = element_text(size = 10 * cex),
          legend.text = element_text(size = 12 * cex),
          plot.title = element_text(size=12* cex),
          axis.title=element_text(size=14 * cex,face="bold"),
          axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(fill = "ICL", title = bquote(bold('Scores'))) +
    theme_classic(base_size = 10 * cex) +
    guides(fill = guide_legend(label = FALSE, keyheight = .3, reverse = TRUE))
  #
  # +
  #   guides(fill = guide_legend(labels = ""))
    # guides(fill = FALSE)


  if(fit$K[2] > 0) {
    tpl = lapply(
      colnames(fit$BB.params),
      taronezstat,
      data = data,
      fit = fit,
      colour = 'black',
      M = M.tarone
    )

  }

  ### Multiplot
  top.layout = matrix(c(1,1,1,2,2,3), nrow = 2, byrow = T)
  if(fit$K[2] == 0)
    multiplot(p, d, msh, layout = top.layout)
  else
  {
    nbb = fit$K[2]
    repeat{
      if(nbb/3 !=1) nbb = nbb+1
      else break;
    }
    bline = 3 + matrix(1:nbb, ncol = 3)

    multiplot(
      plotlist = append(list(p, d, msh), tpl),
      layout = rbind(top.layout, bline))
  }

}


taronezstat = function(cluster, fit, data, colour = 'black', M = 1000, cex =1)
{
  cdata = data[fit$labels == cluster, ]
  s_n = cdata[, 1]
  t_n = cdata[, 2]

  cat("** Analyzing overdispersion for ", cluster, '\n\n')

  cat('- Bootstrapping Tarone\'s Z statistics ...')
  alt_hyp = null_hyp <- vector("numeric", length = M)
  for (i in 1:M) {
    boostrap_samples = sample(length(t_n), replace = TRUE)

    t = t_n[boostrap_samples]
    s = s_n[boostrap_samples]

    p_hat = sum(s) / sum(t)

    S = sum( (s - t * p_hat)^2 / (p_hat * (1 - p_hat)) )
    Z_score = (S - sum(t)) / sqrt(2 * sum(t * (t - 1)))

    null_hyp[i] <- Z_score
  }
  cat(" OK.\n")

  require(dplyr)
  require(data.table)

  dt <- list(Bootstrapped = null_hyp) %>%
    melt %>% as.data.table %>%
    setnames(c("value", "L1"), c("value", "Hypothesis"))

  ######## p-value
  aster = function(p, pmin){
    if(p >= pmin) return('')
    if(p < 0.001) return('***')
    if(p < 0.01) return('**')
    if(p < 0.05) return('*')
    return('')
  }

  p = 1-pnorm(mean(null_hyp), 0, 1)
  cluster.coverage = t_n

  if(length(cluster.coverage) > 5000) {
    message("More than 5000 elements in a cluster --  Shapiro test will be computed with a random subset of just 5000 values")
    cluster.coverage = cluster.coverage[sample(1:length(cluster.coverage), 5000)]
  }

  p = shapiro.test(cluster.coverage)


  cat("- Shapiro test against normality (p > 0.05 implies normal)\n")
  print(p)
  p = p$p.value
  p = paste(format(p, scientific = T, digits = 2), aster(p, 0.05))


  # Plot limits
  H0 = dnorm(seq(-10, 10,0.01), 0, 1)

  boot.hist = hist(null_hyp, breaks = 100, plot = FALSE)
  xlim.hist = c(min(boot.hist$mids) - 1, max(boot.hist$mids) + 1)
  # xlim.H0 = dnorm(xlim.hist, 0, 1)
  if(xlim.hist[1] > -5) xlim.hist[1] = -5

  pl = ggplot(dt, aes(x = value, fill = Hypothesis)) +
    geom_histogram(aes(y = ..density..), alpha = 0.3,
                   position = "dodge", bins = 100) +
    geom_vline(xintercept = mean(null_hyp), colour = colour, linetype = "dashed", size = 0.7) +
    stat_function(fun = dnorm, colour = "red", size = .5,
                  arg = list(mean = 0, sd = 1), linetype = "dashed") +
    scale_x_continuous(limits = xlim.hist) +
    labs(title = bquote(bold(.(cluster))),
         subtitle = bquote("M ="~ .(M)~ ";" ~p[Shap.]~ "=" ~.(p)),
         x = bquote("Tarone's Z: "~H[0] ~ "~ N(0,1)"), y = "", parse = T) +
    scale_fill_manual(values = colour) +
    # scale_color_brewer(palette = "Dark2") +
    guides(fill = FALSE) +
    theme_classic(base_size = 10 * cex)
  pl
}


#### Internal functions

.MLE_BB = function(data) {
  s_n = data[, 1]
  t_n = data[, 2]

  require(VGAM)

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


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
