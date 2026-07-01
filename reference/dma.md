# Causal mediation analysis using energy regression

Estimates common mediation causal effects using engression for outcome
regressions and Riesz learning for density ratio estimation. Supports
natural effects, organic effects, randomized interventional effects, and
recanting twins decompositions.

## Usage

``` r
dma(
  data,
  trt,
  outcome,
  mediators,
  moc = NULL,
  covar,
  obs = NULL,
  id = NULL,
  d0 = NULL,
  d1 = NULL,
  effect = c("N", "O", "RI", "RT"),
  weights = rep(1, nrow(data)),
  nn_module = sequential_module(),
  control = dma_control()
)
```

## Arguments

- data:

  \[`data.frame`\]  
  A `data.frame` containing all necessary variables.

- trt:

  \[`character`\]  
  Column names of treatment variables.

- outcome:

  \[`character(1)`\]  
  Column name of the outcome variable.

- mediators:

  \[`character`\]  
  Column names of mediator variables.

- moc:

  \[`character`\]  
  Optional column names of mediator-outcome confounders. Required for RI
  and RT effects.

- covar:

  \[`character`\]  
  Column names of baseline covariates.

- obs:

  \[`character(1)`\]  
  Optional column name for censoring indicator (0/1).

- id:

  \[`character(1)`\]  
  Optional column name for cluster identifiers.

- d0:

  \[`function`\]  
  Shift function for control regime.

- d1:

  \[`function`\]  
  Shift function for treatment regime.

- effect:

  \[`character(1)`\]  
  Effect type: "N" (natural), "O" (organic), "RI" (randomized
  interventional), or "RT" (recanting twins).

- weights:

  \[`numeric`\]  
  Optional survey weights.

- nn_module:

  \[`function`\]  
  A function returning a neural network module for Riesz estimation.

- control:

  \[`list`\]  
  Control parameters from
  [`dma_control()`](https://xiangao.github.io/dma/reference/dma_control.md).

## Value

An object of class `dma_result` with components:

- estimates:

  Named list of effect estimates (ife objects).

- outcome_reg:

  Outcome regression predictions.

- alpha_n:

  Natural density ratio estimates.

- alpha_r:

  Randomized density ratio estimates.

- models_y:

  Trained engression models from cross-fitting.

- folds:

  Cross-fitting fold structure.

- vars:

  Variable specification (dma_vars object).

- data:

  Original data frame.

- call:

  The matched call.

- effect:

  The estimated effect type.

## Examples

``` r
# \donttest{
if (torch::torch_is_installed()) {
  # See vignette for full examples
}
#> NULL
# }
```
