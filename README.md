
# BMix <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/caravagn/BMix.svg?branch=master)](https://travis-ci.org/caravagn/BMix)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![](https://img.shields.io/badge/Part%20of-evoverse-blue.svg)](https://caravagn.github.io/evoverse)

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
Evolution
analyses.

#### Help and support

[![](https://img.shields.io/badge/GitHub%20Pages-https://caravagn.github.io/BMix/-yellow.svg)](https://caravagn.github.io/BMix)

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

Giulio Caravagna, PhD. *Institute of Cancer Research, London,
UK*.

<!-- Place this tag in your head or just before your close body tag. -->

<script async defer src="https://buttons.github.io/buttons.js"></script>

<!-- Place this tag where you want the button to render. -->

<a class="github-button" href="https://github.com/caravagn" aria-label="Follow @caravagn on GitHub">Follow
@caravagn</a>

[![](https://img.shields.io/badge/Email-gcaravagn@gmail.com-seagreen.svg)](mailto:gcaravagn@gmail.com)
[![](https://img.shields.io/badge/Github-caravagn-seagreen.svg)](https://github.com/caravagn)
[![](https://img.shields.io/badge/Twitter-@gcaravagna-steelblue.svg)](https://twitter.com/gcaravagna)
[![](https://img.shields.io/badge/Personal%20webpage-https://bit.ly/2kc9E6Y-red.svg)](https://sites.google.com/site/giuliocaravagna/)
