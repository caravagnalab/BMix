
# BMix <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/caravagn/bmix.svg?branch=master)](https://travis-ci.org/caravagn/bmix)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

`BMix` provides univariate Binomial and Beta-Binomial mixture models, as
well as mixtures that combine both type of distributions. Count-based
mixtures can be used in a variety of settings, for instance to model
genome sequencing data of somatic mutations in diseases such as cancer.

`BMix` fits these mixtures by maximum likelihood exploiting the
Expectation Maximization algorithm. Model selection for the number of
mixture components is carried out using the Integrated Classification
Likelihood, and extension of the Bayesian Information Criterion that
accounts for the entropy of the latent variables.

`BMix` is part of the `evoverse`, a package that gathers multiple R
packages to implement Cancer Evolution analyses; see more [about
evoverse](https://caravagn.github.io/evoverse).

#### Help and support

`BMix` has its own webpage at [GitHub
pages](https://caravagn.github.io/BMix/).

-----

### Installation

You can install the released version of `BMix` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("caravagn/bmix")
```

-----

#### Copyright and contacts

Giulio Caravagna, PhD. *Institute of Cancer Research, London, UK*.

  - Personal webpage:
    [https://bit.ly/2kc9E6Y](https://sites.google.com/site/giuliocaravagna/),
  - Email address: <giulio.caravagna@icr.ac.uk> and
    <gcaravagn@gmail.com>
  - Twitter feed: [@gcaravagna](https://twitter.com/gcaravagna)
  - GitHub space: [caravagn](https://github.com/caravagn)
