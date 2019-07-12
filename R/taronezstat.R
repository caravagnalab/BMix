
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
