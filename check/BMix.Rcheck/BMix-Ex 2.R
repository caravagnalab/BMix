pkgname <- "BMix"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('BMix')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Clusters")
### * Clusters

flush(stderr()); flush(stdout())

### Name: Clusters
### Title: Extract clustering assignments.
### Aliases: Clusters

### ** Examples

# The same dataset used in the package vignette
data = data.frame(successes = c(rbinom(30, 100, .4), rbinom(70, 100, .7)), trials = 100)

# BMix fit with default parameters
x = bmixfit(data)

Clusters(x, data)



cleanEx()
nameEx("Parameters")
### * Parameters

flush(stderr()); flush(stdout())

### Name: Parameters
### Title: Extract the fit parameters of the mixture.
### Aliases: Parameters

### ** Examples

# The same dataset used in the package vignette
data = data.frame(successes = c(rbinom(30, 100, .4), rbinom(70, 100, .7)), trials = 100)

# BMix fit with default parameters
x = bmixfit(data)

Parameters(x)



cleanEx()
nameEx("bmixfit")
### * bmixfit

flush(stderr()); flush(stdout())

### Name: bmixfit
### Title: Fit a 'Bmix' mixture
### Aliases: bmixfit

### ** Examples

# The same dataset used in the package vignette
data = data.frame(successes = c(rbinom(30, 100, .4), rbinom(70, 100, .7)), trials = 100)

# BMix fit with default parameters
x = bmixfit(data)

print(x)



cleanEx()
nameEx("plot.bmix")
### * plot.bmix

flush(stderr()); flush(stdout())

### Name: plot.bmix
### Title: Plot a 'bmix' object.
### Aliases: plot.bmix

### ** Examples

# The same dataset used in the package vignette
data = data.frame(successes = c(rbinom(30, 100, .4), rbinom(70, 100, .7)), trials = 100)

# BMix fit with default parameters
x = bmixfit(data)

# Empty plot
plot(x)

# Plot with data
plot(x, data)



cleanEx()
nameEx("plot_clusters")
### * plot_clusters

flush(stderr()); flush(stdout())

### Name: plot_clusters
### Title: Plot the clustering of the data.
### Aliases: plot_clusters

### ** Examples

# The same dataset used in the package vignette
data = data.frame(successes = c(rbinom(30, 100, .4), rbinom(70, 100, .7)), trials = 100)

# BMix fit with default parameters
x = bmixfit(data)

plot_clusters(x, data)



cleanEx()
nameEx("plot_density")
### * plot_density

flush(stderr()); flush(stdout())

### Name: plot_density
### Title: Plot the density of the mixture.
### Aliases: plot_density

### ** Examples

# The same dataset used in the package vignette
data = data.frame(successes = c(rbinom(30, 100, .4), rbinom(70, 100, .7)), trials = 100)

# BMix fit with default parameters
x = bmixfit(data)

plot_density(x, data)



cleanEx()
nameEx("plot_model_selection")
### * plot_model_selection

flush(stderr()); flush(stdout())

### Name: plot_model_selection
### Title: Plot the grid of model selection for the mixture.
### Aliases: plot_model_selection

### ** Examples

# Simple dataset
data = data.frame(successes = c(rbinom(30, 100, .4), rbinom(70, 100, .7)), trials = 100)

# BMix fit
x = bmixfit(data, K.Binomials = 1:3, K.BetaBinomials = 0:3)

plot_model_selection(x)



cleanEx()
nameEx("print.bmix")
### * print.bmix

flush(stderr()); flush(stdout())

### Name: print.bmix
### Title: Print a 'bmix' object.
### Aliases: print.bmix

### ** Examples

# The same dataset used in the package vignette
data = data.frame(successes = c(rbinom(30, 100, .4), rbinom(70, 100, .7)), trials = 100)

# BMix fit with default parameters
x = bmixfit(data)

print(x)



cleanEx()
nameEx("to_string")
### * to_string

flush(stderr()); flush(stdout())

### Name: to_string
### Title: Export a tibble with the parameters fits for this model
### Aliases: to_string

### ** Examples

# The same dataset used in the package vignette
data = data.frame(successes = c(rbinom(30, 100, .4), rbinom(70, 100, .7)), trials = 100)

# BMix fit with default parameters
x = bmixfit(data)

to_string(x)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
