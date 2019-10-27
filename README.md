
# BMix <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/caravagn/BMix.svg?branch=master)](https://travis-ci.org/caravagn/BMix)
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
Likelihood, an extension of the Bayesian Information Criterion that
accounts for the entropy of the latent variables.

`BMix` is part of the `evoverse` set of [R
packages](https://caravagn.github.io/evoverse) to implement Cancer
Evolution analyses.

#### Help and support

`BMix` has its own webpage at [GitHub
pages](https://caravagn.github.io/BMix/).

-----

#### Installation

You can install the released version of `BMix` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("caravagn/BMix")
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

-----

<div id="bg">

<a href="https://caravagn.github.io/evoverse">
<img src="https://caravagn.github.io/evoverse/reference/figures/logo.png" width="8%">
</a> <a href="https://caravagn.github.io/CNAqc">
<img src="https://caravagn.github.io/CNAqc/reference/figures/logo.png" width="8%">
</a> <a href="https://caravagn.github.io/BMix">
<img src="https://caravagn.github.io/BMix/reference/figures/logo.png" width="8%">
</a> </a> <a href="https://caravagn.github.io/VIBER">
<img src="https://caravagn.github.io/VIBER/reference/figures/logo.png" width="8%">
</a> <a href="https://caravagn.github.io/ctree">
<img src="https://caravagn.github.io/ctree/reference/figures/logo.png" width="8%">
</a> <a href="https://caravagn.github.io/mtree">
<img src="https://caravagn.github.io/mtree/reference/figures/logo.png" width="8%">
</a> </a> <a href="https://caravagn.github.io/revolver">
<img src="https://caravagn.github.io/revolver/reference/figures/logo.png" width="8%">
</a>

</div>
