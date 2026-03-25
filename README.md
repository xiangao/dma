# dma

Distributional Mediation Analysis using energy regression.

## Overview

dma implements causal mediation analysis using `engression` (distributional regression with energy score loss) for nuisance parameter estimation. It supports four effect decompositions:

- **Natural (N)**: Direct and indirect effects
- **Organic (O)**: Organic direct and indirect effects
- **Randomized Interventional (RI)**: RIDE and RIIE
- **Recanting Twins (RT)**: Four-way path decomposition

Based on the framework of Liu, Williams, Rudolph & Diaz (2024), with engression replacing traditional machine learning for both outcome regressions and Riesz representer density ratio estimation.

## Installation

```r
# Install from GitHub
devtools::install_github("xiangao/dma")
```

## Quick Start

```r
library(dma)

result <- dma(
  data = df,
  trt = "A",
  outcome = "Y",
  mediators = "M",
  covar = "W",
  effect = "N",
  d0 = \(data, trt) 0,
  d1 = \(data, trt) 1
)

print(result)
tidy(result)
plot(result)

# Counterfactual density plots
plot_counterfactual_density(result)                        # marginal P(Y|do(A=a))
plot_counterfactual_density(result, use_weights = TRUE)    # all mediation regimes
```

## Features

- Distributional outcome regression via `engression` (learns full P(Y|X), not just E[Y|X])
- Neural network Riesz representer estimation for density ratios
- Cross-fitting with parallel fold processing via `future.apply`
- Observation weights propagated through all nuisance estimation stages
- Coefficient plots and counterfactual density visualization
- Weighted counterfactual density estimation via Riesz representers, showing all mediation regimes (Y(0,M(0)), Y(1,M(0)), Y(1,M(1))) including cross-world counterfactuals

## Vignettes

- `dma` — Comparison with crumble on natural and organic effects, oracle vs estimated distributions, Monte Carlo study
- `distributional-mediation` — Non-linear DGP demonstrating distributional mediation with weighted counterfactual densities
- `effect-types` — All four effect decompositions (N, O, RI, RT) with oracle potential outcome distributions
- `advantages-of-engression` — When distributional regression outperforms conditional mean methods
