# dma result object

dma result object

## Usage

``` r
new_dma_result(
  estimates,
  outcome_reg,
  alpha_n,
  alpha_r,
  models_y,
  folds,
  vars,
  data,
  call,
  effect,
  d0,
  d1
)
```

## Arguments

- estimates:

  A list of effect estimates.

- outcome_reg:

  Outcome regression predictions.

- alpha_n:

  Natural density ratio estimates.

- alpha_r:

  Randomized density ratio estimates.

- models_y:

  List of trained engression models.

- folds:

  List of cross-fit folds.

- vars:

  Object of class dma_vars.

- data:

  Data frame used for estimation.

- call:

  The matched call.

- effect:

  The estimated effect type.

## Value

An object of class `dma_result`.
