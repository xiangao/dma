# Counterfactual CDF estimates by y

For a fitted `dma_result` and a grid of y-values, computes the
distributional analog \\F\_{a,a'}(y) = P(Y(a, M(a')) \le y)\\ along with
pointwise standard errors and confidence intervals derived from the
y-indexed efficient influence function (see companion paper, 5.1).

## Usage

``` r
F_hat(fit, y, level = 0.95, N_sim = 200L, jkl = NULL)
```

## Arguments

- fit:

  A `dma_result` returned by `dma`.

- y:

  Numeric vector of thresholds at which to evaluate \\F\\.

- level:

  Confidence level for pointwise intervals (default 0.95).

- N_sim:

  Number of engression samples per observation used to approximate \\P(Y
  \le y \mid \cdot)\\. Default 200.

- jkl:

  Character string identifying which (a, a') regime to evaluate. Must be
  one of `colnames(fit$alpha_n[[1]])` (for natural/organic/RT effects)
  or `colnames(fit$alpha_r[[1]])` (for RI effects). Defaults to the
  *last* column (by convention, the (1,1) regime for natural effects).
  Pass explicitly to select a different counterfactual.

## Value

A `data.frame` with columns:

- y:

  The input threshold values.

- Fhat:

  Point estimate \\\hat F\_{a,a'}(y)\\.

- std_error:

  Pointwise standard error.

- conf_low:

  Lower bound of the pointwise `level`-confidence interval.

- conf_high:

  Upper bound of the pointwise confidence interval.

## Details

The function reuses the trained engression conditional-distribution
model and the Riesz density-ratio nuisances from a fitted `dma_result`.
For each threshold `y`, the conditional CDF nuisance \\\tilde\mu_y(a, m,
w) = P(Y \le y \mid A=a, M=m, W=w)\\ is obtained from the engression
model by drawing `N_sim` samples per observation and computing the
empirical proportion below `y`. The intermediate iterated-expectation
stages (\\\tilde\theta_y\\ and its marginal) are computed via ordinary
least squares, following the same telescoping structure as the mean EIF.
The propensity and mediator-density-ratio weights (`alpha`) are
unchanged because they do not depend on \\Y\\.
