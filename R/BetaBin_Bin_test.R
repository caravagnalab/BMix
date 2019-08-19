library(tidyverse)

# Sample data from a single Binomial
# - n: number of samples
# - p: Binomial success probability
# - c: mean of the Poisson-distributed number of trials
bsampler = function(n, p, c)
{
  cov = rpois(n, c)
  data.frame(
    m = paste(sample(LETTERS, 5), collapse = ''),
    x = rbinom(n, cov, p),
    y = cov)
}

# Test sampler: sample data from a mixture of 2 Binomials
# - n: number of total samples
# - pi1: proportion of samples from the first mixture component
# - p1,p2: Binomial probabilities
# - c: mean of the Poisson-distributed number of trials
tsampler = function(n, pi1, p1, p2, c)
{
  np1 = round(n * pi1)
  np2 = round(n * (1 - pi1))

  df_b1 = bsampler(np1, p1, c)
  df_b2 = bsampler(np2, p2, c)

  df = bind_rows(df_b1, df_b2)

  plot =
    ggplot(df, aes(x/y, fill = m)) +
    geom_histogram(binwidth = 0.01) +
    xlim(0, 1) +
    labs(
      title = paste0('N = (', np1, ', ', np2, '), C = ', c),
      subtitle = paste0('Sk: ', e1071::skewness(df$x/df$y))
      ) +
    guides(fill = FALSE) +
    geom_vline(xintercept = p1) +
    geom_vline(xintercept = p2)

  return(list(data = df, plot = plot))
}

tfitter = function(x)
{
  require(BMix)

  df = x$data

  fit = bmixfit(df[ , -1], samples = 3)

  pd = plot_density(fit, data = df[, -1])
  pc = plot_clusters(fit, data = df[, -1], alpha = 1)

  return(list(
    K_B = fit$K[1],
    K_BB = fit$K[2],
    fit = fit, pd = pd, pc = pc))
}

# Example single test
test = tsampler(500, runif(1), .5, .2, 120)
fit = tfitter(test)

assemble_plot =
  ggpubr::ggarrange(
    test$plot,
    fit$pc
  )
assemble_plot

# Large scale tests
ranges = seq(0, 0.3, by = 0.02)
pi = seq(0.1, 0.9, by = 0.05)

params = expand.grid(r = ranges, pi = pi, n = 1:10)

tests = lapply(1:nrow(params),
       function(w)
       {
         test = tsampler(500, params$pi[w], .5, .5 - params$r[w], 120)
         fit = tfitter(test)

         assemble_plot =
           ggpubr::ggarrange(
             test$plot,
             fit$pc
           )
         assemble_plot

         fit
       }
       )
