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
