## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(BMix)

## -----------------------------------------------------------------------------
data = data.frame(
  successes = c(
    rbinom(300, 100, .5),  # First component - 300 points, peak at 0.5
    rbinom(700, 100, .25), # Second component - 700 points, peak at 0.25
    rbinom(700, 100, .1)), # Third component - 700 points, peak at 0.1
  trials = 100)

print(head(data))

## ---- warning=FALSE, fig.height=3, fig.width=3--------------------------------
require(ggplot2)

ggplot(data, aes(successes/trials)) + geom_histogram(binwidth = 0.01) + theme_linedraw()

## ---- warning=FALSE-----------------------------------------------------------
# Default parameters
x = bmixfit(data, K.Binomials = 1:3, K.BetaBinomials = 0)

# Maybe one could compare x to this
# y = bmixfit(data, K.Binomials = 0, K.BetaBinomials = 1:2)

## ---- warning=FALSE-----------------------------------------------------------
print(x)

## -----------------------------------------------------------------------------
# Augment data with cluster labels and latent variables
Clusters(x, data)

# Obtain for every fit component the mean and its overdispersion.
# Binomial components have 0 overdispersion by definition.
Parameters(x)

## ---- warning=FALSE, fig.height=3, fig.width=3--------------------------------
plot_clusters(x, data)

## ---- warning=FALSE, fig.height=3, fig.width=3--------------------------------
plot_density(x, data)

## ---- warning=FALSE, fig.height=3, fig.width=3--------------------------------
plot_model_selection(x)

## ---- warning=FALSE, fig.height=3, fig.width=9--------------------------------
BMix::plot.bmix(x, data) 

## ---- warning=FALSE, fig.height=3, fig.width=9--------------------------------
# Custom parameters
x = bmixfit(data, 
            K.Binomials = 0, 
            K.BetaBinomials = 1:3)

# Show outputs
print(x)

BMix::plot.bmix(x, data) 

## ---- warning=FALSE, fig.height=3, fig.width=9--------------------------------
# Custom parameters
x = bmixfit(data, 
            K.Binomials = 0:3, 
            K.BetaBinomials = 0:3)

# Show outputs
print(x)

BMix::plot.bmix(x, data) 

