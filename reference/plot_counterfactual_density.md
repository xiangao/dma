# Plot counterfactual outcome densities

Visualizes the distribution of the outcome under different treatment
regimes. Two modes: (1) shift-and-predict using trained engression
models (default), which shows marginal outcome distributions
P(Y\|do(A=a)); (2) weighted densities using the learned Riesz
representers (`use_weights = TRUE`), which shows all mediation
counterfactual regimes including cross-world quantities like Y(1, M(0)).

## Usage

``` r
plot_counterfactual_density(
  result,
  n_samples = 1,
  regimes = NULL,
  use_weights = FALSE
)
```

## Arguments

- result:

  An object of class `dma_result`.

- n_samples:

  Number of samples to draw per observation (for shift-and-predict).

- regimes:

  A named list of shift functions or numeric values for treatment.
  Default uses the d0/d1 from the original call. Only used when
  `use_weights = FALSE`.

- use_weights:

  If `TRUE`, uses the learned density ratios (Riesz representers) to
  estimate counterfactual distributions via importance weighting. This
  shows all mediation-relevant regimes, not just the two marginal
  treatment regimes. Default is `FALSE`.

## Value

A ggplot2 object.
