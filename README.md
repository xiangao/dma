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
```

## Features

- Distributional outcome regression via `engression` (learns full P(Y|X), not just E[Y|X])
- Neural network Riesz representer estimation for density ratios
- Cross-fitting with parallel fold processing via `future.apply`
- Observation weights propagated through all nuisance estimation stages
- Coefficient plots and counterfactual density visualization

## Vignettes

- `dma.Rmd` — Comparison with crumble on natural and organic effects
- `effect-types.Rmd` — All four effect decompositions (N, O, RI, RT)
- `advantages-of-engression.Rmd` — When distributional regression outperforms conditional mean methods
