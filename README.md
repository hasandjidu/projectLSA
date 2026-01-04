
# projectLSA <img src="man/figures/logoProjectLSA.png" align="right" width="120" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/projectLSA)](https://CRAN.R-project.org/package=projectLSA)
[![Downloads](https://cranlogs.r-pkg.org/badges/projectLSA)](https://cranlogs.r-pkg.org/badges/projectLSA)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/projectLSA)](https://cranlogs.r-pkg.org/badges/grand-total/projectLSA)

[![R-CMD-check](https://github.com/hasandjidu/projectLSA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hasandjidu/projectLSA/actions/workflows/R-CMD-check.yaml)

[![License:
MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)

<!-- badges: end -->

projectLSA is an R package that provides a complete graphical user
interface (GUI) for conducting **Latent Structure Analysis (LSA)**
through a Shiny application. It integrates multiple latent variable
methods, including:

- **Latent Profile Analysis (LPA)**
- **Latent Class Analysis (LCA)**
- **Latent Trait Analysis (LTA / IRT)**
- **Exploratory Factor Analysis (EFA)**
- **Confirmatory Factor Analysis (CFA)**

All analyses can be performed **without writing any code**, making the
package accessible for researchers, students, and applied analysts.

------------------------------------------------------------------------

## Installation

``` r
# Install from CRAN (when available)
install.packages("projectLSA")

# Install development version from GitHub (optional)
# remotes::install_github("hasandjidu/projectLSA")
```

------------------------------------------------------------------------

## Launch the Application

``` r
library(projectLSA)
run_projectLSA()
```

This opens the full Shiny application, including all LSA modules, data
upload, built-in datasets, interactive plots, and reporting features.

------------------------------------------------------------------------

## Features

### ✔ Latent Profile Analysis (LPA)

- Upload your own dataset or use built-in examples.
- Fit multiple LPA models automatically.
- Compare AIC, BIC, entropy, and class size.
- Visualize the best model with customizable class names.

### ✔ Latent Class Analysis (LCA)

- Supports categorical indicators.
- Fits multiple class solutions.
- Interactive plots with **ggiraph**.
- Probability tables and class membership export.

### ✔ Latent Trait Analysis (LTA / IRT)

- Supports dichotomous and polytomous items.
- Automatically fits Rasch, 2PL, 3PL (or PCM/GRM/GPCM).
- ICC plots, test information, factor scores.
- Multi-dimensional visualization with 3D surfaces and heatmaps.

### ✔ Exploratory Factor Analysis (EFA)

- KMO, Bartlett test, parallel analysis.
- Factor extraction with rotation.
- Factor scores and loading matrix export.
- Clean HTML summaries for clearer interpretation.

### ✔ Confirmatory Factor Analysis (CFA)

- Lavaan model editor.
- Fit measures, loadings, factor scores.
- Fully customized SEM path diagrams.

------------------------------------------------------------------------

## Example Screenshots

### LPA (Latent Profile Analysis)

![LPA1](man/figures/LPA1.png) ![LPA2](man/figures/LPA2.png)
![LPA3](man/figures/LPA3.png) ![LPA4](man/figures/LPA4.png)
![LPA5](man/figures/LPA5.png)

### LCA (Latent Class Analysis)

![LCA1](man/figures/LCA1.png) ![LCA2](man/figures/LCA2.png)
![LCA3](man/figures/LCA3.png) ![LCA4](man/figures/LCA4.png)

### CFA (Confirmatory Factor Analysis)

![CFA1](man/figures/CFA1.png) ![CFA2](man/figures/CFA2.png)
![CFA3](man/figures/CFA3.png) ![CFA4](man/figures/CFA4.png)
![CFA5](man/figures/CFA5.png) ![CFA6](man/figures/CFA6.png)
![CFA7](man/figures/CFA7.png) ![CFA8](man/figures/CFA8.png)
![CFA9](man/figures/CFA9.png)

### SEM (Structural Equation Modelling)

<figure>
<img src="man/figures/SEM.png" alt="SEM" />
<figcaption aria-hidden="true">SEM</figcaption>
</figure>

### LTA (Latent Trait Analysis/ IRT)

![LTA1](man/figures/LTA1.png) ![LTA2](man/figures/LTA2.png)
![LTA3](man/figures/LTA3.png) ![LTA4](man/figures/LTA4.png)
![LTA5](man/figures/LTA5.png) ![LTA6](man/figures/LTA6.png)

------------------------------------------------------------------------

## Citation

If you use projectLSA in publications, please cite:

Djidu, H., Retnawati, H., Hadi, S., & Haryanto (2026). *projectLSA:R
Shiny application for latent structure analysis with a graphical user
interface. <https://doi.org/10.32614/CRAN.package.projectLSA>*

------------------------------------------------------------------------

## Contributing

Bug reports and feature requests are welcome:

<https://github.com/hasandjidu/projectLSA/issues>

------------------------------------------------------------------------

## License

MIT License © 2026 Hasan Djidu
