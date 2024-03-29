
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4478886.svg)](https://doi.org/10.5281/zenodo.4478886)

# Replication package for manuscript on Bayesian analysis on programming languages data

In the `docs` folder you will find two `Rmd` scripts developed for this replication package. Both scripts can also be found as `html` files in that folder. The easiest way to work with this package is to:

1. Clone the repository. 
2. Open the `BDA-PL.Rproj` file in `RStudio`.
3. Open up the script `index.Rmd` (which is the analysis we conduct in the manuscript).
4. Either, 
    1. run the `R` commands in that file in the `R` console or, 
    2. simply knit it to an `html` file in `RStudio`.

If one feels confident about the analysis, then you can always read it as published by us here: [https://torkar.github.io/BDA-PL/index.html](https://torkar.github.io/BDA-PL/index.html). It is, however, worthwhile to point out that we're using dynamic Hamiltonian Monte Carlo (i.e., a stochastic algorithm). That means that each time you run the scripts you will have slightly different outputs (but not different enough to invalidate the conclusions!), which could lead to plots that are not 1:1 copies of the plots in the manuscript. 

In the `docs` folder you will also find an analysis using TOPLAS' data (`bda-cq-2.Rmd`). That file can also be found published at: https://torkar.github.io/BDA-PL/bda-cq-2.html

Please contact [Richard Torkar](mailto:torkarr@chalmers.se?subject=[GitHub]%20BDA-PL) if you have any comments concerning this analysis. We would be most grateful if you report flaws so we can improve the analysis!
